# ============================================================
# Germination data analysis workflow
# ============================================================

library(lme4)
library(dplyr)
library(emmeans)
library(multcompView)

# ------------------------------------------------------------
# 0. Data preparation
# ------------------------------------------------------------
.prepare_germ_data <- function(data, trt_col, y_col, n_col, dish_col) {
  dat <- data
  dat$trt_group <- as.factor(dat[[trt_col]])
  dat$y_val     <- dat[[y_col]]
  dat$n_val     <- dat[[n_col]]
  dat$dish_id   <- as.factor(dat[[dish_col]])
  
  if (any(is.na(dat$trt_group)) || any(is.na(dat$y_val)) ||
      any(is.na(dat$n_val)) || any(is.na(dat$dish_id))) {
    stop("Missing values are present in treatment, response, total seed, or dish columns.")
  }
  
  if (any(dat$y_val < 0) || any(dat$n_val <= 0) || any(dat$y_val > dat$n_val)) {
    stop("Invalid binomial counts: require 0 <= y <= n and n > 0.")
  }
  
  dat
}

.find_extreme_groups <- function(dat) {
  dat %>%
    group_by(trt_group) %>%
    summarise(sum_y = sum(y_val), sum_n = sum(n_val), .groups = "drop") %>%
    filter(sum_y == 0 | sum_y == sum_n) %>%
    pull(trt_group)
}

# ------------------------------------------------------------
# 1. Stable Rhie LRT without glm()
# ------------------------------------------------------------
.binom_ll <- function(y, n, p) {
  if (p == 0) {
    return(ifelse(y == 0, 0, -Inf))
  }
  if (p == 1) {
    return(ifelse(y == n, 0, -Inf))
  }
  y * log(p) + (n - y) * log(1 - p)
}

rhie_lrt <- function(dat) {
  group_sum <- dat %>%
    group_by(trt_group) %>%
    summarise(sum_y = sum(y_val), sum_n = sum(n_val), .groups = "drop")
  
  p0 <- sum(group_sum$sum_y) / sum(group_sum$sum_n)
  
  ll_null <- sum(mapply(.binom_ll, group_sum$sum_y, group_sum$sum_n,
                        MoreArgs = list(p = p0)))
  
  ll_full <- sum(mapply(function(y, n) {
    .binom_ll(y, n, y / n)
  }, group_sum$sum_y, group_sum$sum_n))
  
  stat <- 2 * (ll_full - ll_null)
  if (stat < 0) stat <- 0
  
  df <- nrow(group_sum) - 1
  pval <- pchisq(stat, df = df, lower.tail = FALSE)
  
  list(statistic = stat, df = df, p_value = pval)
}

pairwise_rhie_lrt <- function(dat, alpha = 0.05, p_adjust_method = "holm") {
  trts <- levels(droplevels(dat$trt_group))
  pairs <- combn(trts, 2, simplify = FALSE)
  
  pair_res <- lapply(pairs, function(pr) {
    d <- dat %>%
      filter(trt_group %in% pr) %>%
      droplevels()
    
    res <- rhie_lrt(d)
    
    data.frame(
      group1 = pr[1],
      group2 = pr[2],
      chisq = res$statistic,
      df = res$df,
      p_value = res$p_value
    )
  }) %>% bind_rows()
  
  pair_res$p_adj <- p.adjust(pair_res$p_value, method = p_adjust_method)
  pair_res$significant <- pair_res$p_adj < alpha
  
  pvec <- pair_res$p_adj
  names(pvec) <- paste(pair_res$group1, pair_res$group2, sep = "-")
  
  letters_raw <- multcompView::multcompLetters(pvec, threshold = alpha)$Letters
  
  prob_tbl <- dat %>%
    group_by(trt_group) %>%
    summarise(prob = sum(y_val) / sum(n_val), .groups = "drop")
  
  cld_tbl <- data.frame(
    trt_group = names(letters_raw),
    CLD = unname(letters_raw),
    row.names = NULL
  )
  
  cld_out <- prob_tbl %>%
    left_join(cld_tbl, by = "trt_group") %>%
    relabel_cld_by_prob()
  
  list(pairwise_table = pair_res, cld_table = cld_out)
}

# ------------------------------------------------------------
# 2. CLD relabeling: highest probability gets "a"
# ------------------------------------------------------------
relabel_cld_by_prob <- function(cld_df) {
  cld_df$CLD <- gsub(" ", "", cld_df$CLD)
  
  old_letters <- sort(unique(unlist(strsplit(paste0(cld_df$CLD, collapse = ""), ""))))
  
  letter_score <- sapply(old_letters, function(z) {
    max(cld_df$prob[grepl(z, cld_df$CLD, fixed = TRUE)], na.rm = TRUE)
  })
  
  old_order <- names(sort(letter_score, decreasing = TRUE))
  new_letters <- letters[seq_along(old_order)]
  map <- setNames(new_letters, old_order)
  
  cld_df$CLD <- sapply(cld_df$CLD, function(x) {
    xs <- unlist(strsplit(x, ""))
    xs_new <- unname(map[xs])
    paste(xs_new[order(match(xs_new, new_letters))], collapse = "")
  })
  
  cld_df[order(cld_df$prob, decreasing = TRUE), ]
}

# ------------------------------------------------------------
# 3. Dish effect assessment
# ------------------------------------------------------------
check_dish_effect <- function(data, trt_col = "treatment",
                              y_col = "y", n_col = "n",
                              dish_col = "dish",
                              dataset_name = "Dataset",
                              alpha = 0.05,
                              singular_tol = 1e-5) {
  
  cat("\n==========================================================\n")
  cat(" [Dish-Level Effect Assessment] \n")
  cat("==========================================================\n")
  
  dat <- .prepare_germ_data(data, trt_col, y_col, n_col, dish_col)
  
  extreme_groups <- .find_extreme_groups(dat)
  
  dat_clean <- dat %>%
    filter(!trt_group %in% extreme_groups) %>%
    droplevels()
  
  if (length(extreme_groups) > 0) {
    cat(" >> Extreme treatment groups excluded for dish-effect assessment:",
        paste(extreme_groups, collapse = ", "), "\n")
  }
  
  if (nrow(dat_clean) == 0) {
    cat(" >> Dish effect cannot be assessed: all treatment groups are extreme.\n")
    return(invisible(list(
      extreme_groups = extreme_groups,
      dish_p_value = NA,
      dish_variance = NA,
      dish_significant = NA
    )))
  }
  
  if (length(unique(dat_clean$trt_group)) > 1) {
    f_glm  <- cbind(y_val, n_val - y_val) ~ trt_group
    f_glmm <- cbind(y_val, n_val - y_val) ~ trt_group + (1 | dish_id)
  } else {
    f_glm  <- cbind(y_val, n_val - y_val) ~ 1
    f_glmm <- cbind(y_val, n_val - y_val) ~ 1 + (1 | dish_id)
    cat(" >> Only one non-extreme treatment remains; testing dish effect only.\n")
  }
  
  model_glm <- glm(f_glm, data = dat_clean, family = binomial)
  
  model_glmm <- suppressMessages(
    suppressWarnings(
      glmer(
        f_glmm,
        data = dat_clean,
        family = binomial,
        control = glmerControl(optimizer = "bobyqa")
      )
    )
  )
  
  sigma_sq <- as.numeric(VarCorr(model_glmm)$dish_id[1, 1])
  
  if (is.na(sigma_sq) || sigma_sq < singular_tol || isSingular(model_glmm)) {
    dish_pval <- 1.0
    cat(" >> No evidence of dish-level variability: variance estimate is near zero.\n")
  } else {
    cat(" >> Testing dish-level variability using boundary-adjusted LRT.\n")
    
    stat <- 2 * (as.numeric(logLik(model_glmm)) - as.numeric(logLik(model_glm)))
    if (stat < 0) stat <- 0
    
    dish_pval <- 0.5 * pchisq(stat, df = 1, lower.tail = FALSE)
  }
  
  cat(" >> Dish-level variance estimate:", signif(sigma_sq, 4), "\n")
  cat(" >> Dish-level effect P-value:", format.pval(dish_pval, digits = 4), "\n")
  
  if (!is.na(dish_pval) && dish_pval < alpha) {
    cat(" >> Significant dish-level variability detected.\n")
  } else {
    cat(" >> No significant dish-level variability detected.\n")
  }
  
  invisible(list(
    extreme_groups = extreme_groups,
    dish_p_value = dish_pval,
    dish_variance = sigma_sq,
    dish_significant = !is.na(dish_pval) && dish_pval < alpha
  ))
}

# ------------------------------------------------------------
# 4. Scenario-based final analysis
# ------------------------------------------------------------
apply_scenario_analysis <- function(data, trt_col = "treatment",
                                    y_col = "y", n_col = "n",
                                    dish_col = "dish",
                                    dish_result,
                                    alpha = 0.05,
                                    p_adjust_method = "holm") {
  
  dat <- .prepare_germ_data(data, trt_col, y_col, n_col, dish_col)
  
  dish_pval <- dish_result$dish_p_value
  extreme_groups <- dish_result$extreme_groups
  
  dish_significant <- !is.na(dish_pval) && dish_pval < alpha
  has_extreme <- length(extreme_groups) > 0
  
  dat_clean <- dat %>%
    filter(!trt_group %in% extreme_groups) %>%
    droplevels()
  
  extreme_summary <- dat %>%
    group_by(trt_group) %>%
    summarise(sum_y = sum(y_val), sum_n = sum(n_val), .groups = "drop") %>%
    filter(trt_group %in% extreme_groups) %>%
    mutate(
      prob = ifelse(sum_y == 0, 0, 1),
      CLD = "(extreme)"
    )
  
  scenario <- ifelse(dish_significant && !has_extreme, "A",
                     ifelse(dish_significant && has_extreme, "B", "C"))
  #scenario <-"C"
  cat("\n==========================================================\n")
  cat(" [Final Germination Analysis] \n")
  cat(" Scenario:", scenario, "\n")
  cat("==========================================================\n")
  
  # --------------------------
  # Scenario A: GLMM, no extreme
  # --------------------------
  if (scenario == "A") {
    cat(" >> Scenario A: significant dish effect and no extreme treatment.\n")
    cat(" >> Recommended method: GLMM.\n\n")
    
    model_full <- glmer(
      cbind(y_val, n_val - y_val) ~ trt_group + (1 | dish_id),
      data = dat,
      family = binomial,
      control = glmerControl(optimizer = "bobyqa")
    )
    
    model_null <- glmer(
      cbind(y_val, n_val - y_val) ~ 1 + (1 | dish_id),
      data = dat,
      family = binomial,
      control = glmerControl(optimizer = "bobyqa")
    )
    
    test <- anova(model_null, model_full)
    
    main_stat <- test$Chisq[2]
    main_df   <- test$Df[2]
    main_pval <- test$`Pr(>Chisq)`[2]
    
    cat(" >> [Main Effect Test: GLMM LRT]\n")
    cat("    Chi-square =", round(main_stat, 4),
        ", df =", main_df,
        ", P-value =", format.pval(main_pval, digits = 4), "\n")
    
    cld_table <- NULL
    
    if (main_pval < alpha) {
      cat(" >> Significant treatment effect detected. \n")
      cat(" >> Performing GLMM pairwise comparisons.\n\n")
      
      emm <- emmeans(model_full, specs = "trt_group", type = "response")
      cld_res <- cld(emm, Letters = letters, reversed = TRUE, alpha = alpha)
      
      cld_table <- as.data.frame(cld_res) %>%
        transmute(trt_group, prob, CLD = .group) %>%
        relabel_cld_by_prob()
      
      print(cld_table)
    } else {
      cat(" >> No significant treatment effect detected.\n")
      cat(" >> No pairwise comparisons performed.\n\n")
    }
    
    return(invisible(list(
      scenario = scenario,
      method = "GLMM",
      main_p_value = main_pval,
      cld_table = cld_table
    )))
  }
  
  # --------------------------
  # Scenario B: GLMM after excluding extremes
  # --------------------------
  if (scenario == "B") {
    cat(" >> Scenario B: significant dish effect with extreme treatment(s).\n")
    cat(" >> Recommended method: GLMM on non-extreme treatments only.\n")
    cat(" >> Interpretation is limited to the non-extreme treatments.\n\n")
    
    if (nrow(dat_clean) == 0 || length(unique(dat_clean$trt_group)) < 2) {
      cat(" >> Fewer than two non-extreme treatments remain. Main effect test cannot be performed.\n")
      
      if (nrow(extreme_summary) > 0) {
        cat("\n >> Extreme treatments for reference:\n")
        print(extreme_summary[, c("trt_group", "prob", "CLD")])
      }
      
      return(invisible(list(
        scenario = scenario,
        method = "GLMM excluding extreme treatments",
        main_p_value = NA,
        cld_table = NULL
      )))
    }
    
    model_full <- glmer(
      cbind(y_val, n_val - y_val) ~ trt_group + (1 | dish_id),
      data = dat_clean,
      family = binomial,
      control = glmerControl(optimizer = "bobyqa")
    )
    
    model_null <- glmer(
      cbind(y_val, n_val - y_val) ~ 1 + (1 | dish_id),
      data = dat_clean,
      family = binomial,
      control = glmerControl(optimizer = "bobyqa")
    )
    
    test <- anova(model_null, model_full)
    
    main_stat <- test$Chisq[2]
    main_df   <- test$Df[2]
    main_pval <- test$`Pr(>Chisq)`[2]
    
    cat(" >> [Main Effect Test: GLMM LRT on Non-extreme Data]\n")
    cat("    Chi-square =", round(main_stat, 4),
        ", df =", main_df,
        ", P-value =", format.pval(main_pval, digits = 4), "\n")
    
    cld_table <- NULL
    
    if (main_pval < alpha) {
      cat(" >> Significant treatment effect detected among non-extreme treatments.\n\n")
      
      emm <- emmeans(model_full, specs = "trt_group", type = "response")
      cld_res <- cld(emm, Letters = letters, reversed = TRUE, alpha = alpha)
      
      cld_non_extreme <- as.data.frame(cld_res) %>%
        transmute(
          trt_group = as.character(trt_group),
          prob = prob,
          CLD = .group
        ) %>%
        relabel_cld_by_prob()
      
      extreme_rows <- extreme_summary %>%
        transmute(
          trt_group = as.character(trt_group),
          prob = prob,
          CLD = "(extreme)"
        )
      
      cld_table <- bind_rows(cld_non_extreme, extreme_rows) %>%
        arrange(desc(prob))
      
      print(cld_table)
      
    } else {
      cat(" >> No significant treatment effect detected among non-extreme treatments.\n")
      cat(" >> No pairwise comparisons performed.\n")
      
      #if (nrow(extreme_summary) > 0) {
      #  cat("\n >> Extreme treatments for reference:\n")
      #  print(extreme_summary[, c("trt_group", "prob", "CLD")])
      #}
    }
    
    return(invisible(list(
      scenario = scenario,
      method = "GLMM excluding extreme treatments",
      main_p_value = main_pval,
      cld_table = cld_table
    )))
  }
  
  # --------------------------
  # Scenario C: Rhie LRT
  # --------------------------
  if (scenario == "C") {
    cat(" >> Scenario C: no significant dish-level effect.\n")
    cat(" >> Recommended method: likelihood-ratio test of Rhie et al. (2024).\n\n")
    
    main_res <- rhie_lrt(dat)
    
    cat(" >> [Main Effect Test: Rhie LRT]\n")
    cat("    Chi-square =", round(main_res$statistic, 4),
        ", df =", main_res$df,
        ", P-value =", format.pval(main_res$p_value, digits = 4), "\n")
    
    cld_table <- NULL
    pairwise_table <- NULL
    
    if (main_res$p_value < alpha) {
      cat(" >> Significant treatment effect detected. \n")
      cat(" >> Performing pairwise Rhie LRT comparisons.\n\n")
      
      posthoc <- pairwise_rhie_lrt(
        dat,
        alpha = alpha,
        p_adjust_method = p_adjust_method
      )
      
      pairwise_table <- posthoc$pairwise_table
      cld_table <- posthoc$cld_table
      
      print(cld_table)
    } else {
      cat(" >> No significant treatment effect detected.\n")
      cat(" >> No pairwise comparisons performed.\n")
    }
    
    return(invisible(list(
      scenario = scenario,
      method = "Rhie LRT",
      main_p_value = main_res$p_value,
      pairwise_table = pairwise_table,
      cld_table = cld_table
    )))
  }
}


# ============================================================
# Interactive germination analysis (file.choose version)
# ============================================================

run_germination_analysis <- function(
    trt_col="treatment",
    y_col = "y",
    n_col = "n",
    dish_col = NULL,
    alpha = 0.05,
    p_adjust_method = "holm",
    save_result = TRUE
) {
  
  # 1. File selection
  file <- file.choose()
  cat(" >> Selected file:", file, "\n")
  
  # dataset name 
  dataset_name <- tools::file_path_sans_ext(basename(file))
  
  # 2. Read data
  dat <- read.csv(file, stringsAsFactors = FALSE)
  
  # 3. Dish column treatment
  if (is.null(dish_col)) {
    dat$dish <- paste0("Dish_", seq_len(nrow(dat)))
    dish_col <- "dish"
    cat(" >> No dish column detected → each row treated as one replicate.\n")
  }
  
  # 4. Column check
  required_cols <- c(trt_col, y_col, n_col, dish_col)
  missing_cols <- setdiff(required_cols, names(dat))
  
  if (length(missing_cols) > 0) {
    stop(
      paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    )
  }
  
  # 5. Basic validation
  if (any(dat[[y_col]] > dat[[n_col]])) {
    stop("Error: some y values exceed n.")
  }
  
  if (any(dat[[y_col]] < 0) || any(dat[[n_col]] <= 0)) {
    stop("Error: require y ≥ 0 and n > 0.")
  }
  
  # 6. Convert to factor
  dat[[trt_col]]  <- as.factor(dat[[trt_col]])
  dat[[dish_col]] <- as.factor(dat[[dish_col]])
  
  # ============================================================
  # Analysis start
  # ============================================================
  
  cat("\n==========================================================\n")
  cat(" Germination Data Analysis\n")
  cat(" Dataset:", dataset_name, "\n")
  cat("==========================================================\n")
  
  # Phase 1: Dish effect
  dish_result <- check_dish_effect(
    data = dat,
    trt_col = trt_col,
    y_col = y_col,
    n_col = n_col,
    dish_col = dish_col,
    dataset_name = dataset_name,
    alpha = alpha
  )
  
  # Phase 2: Scenario analysis
  final_result <- apply_scenario_analysis(
    data = dat,
    trt_col = trt_col,
    y_col = y_col,
    n_col = n_col,
    dish_col = dish_col,
    dish_result = dish_result,
    alpha = alpha,
    p_adjust_method = p_adjust_method
  )
  
  if (save_result) {
    
    prefix <- paste0(dataset_name, "_result")
    
    summary_df <- data.frame(
      dataset = dataset_name,
      scenario = final_result$scenario,
      method = final_result$method,
      dish_p_value = dish_result$dish_p_value,
      main_p_value = final_result$main_p_value
    )
    
    write.csv(summary_df, paste0(prefix, "_summary.csv"), row.names = FALSE)
    
    if (!is.null(final_result$cld_table)) {
      write.csv(final_result$cld_table, paste0(prefix, "_CLD.csv"), row.names = FALSE)
    }
    
    cat("\n >> Results saved with prefix:", prefix, "\n")
  }
  
  invisible(list(
    data = dat,
    dish_result = dish_result,
    final_result = final_result
  ))
}

