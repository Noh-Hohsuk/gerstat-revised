source(file.choose())

# ==============================================================================
# [Data Preparation]
# ==============================================================================

# [1] Asarum
dat_asarum_cold <- data.frame(
  treatment = factor(rep(c(0, 4, 8, 12), each = 3)), 
  n = c(21,24,28, 28,21,28, 22,20,27, 22,20,27),
  y = c(0,0,0, 0,0,0, 0,0,0, 6,12,13)
)
dat_asarum_cold$dish <- paste0("D_", seq_len(nrow(dat_asarum_cold)))

dat_asarum_ga <- data.frame(
  treatment = factor(rep(c(0, 10, 100, 1000), each = 3)), 
  n = c(20,20,20, 20,20,20, 20,20,15, 21,20,20),
  y = c(0,0,0, 1,0,12, 20,14,14, 21,10,1)
)
dat_asarum_ga$dish <- paste0("D_", seq_len(nrow(dat_asarum_ga)))

# [3] Gymnospermium
dat_gymnospermium <- data.frame(
  treatment = factor(rep(c("25/15", "20/10", "15/6", "5"), each = 3)),
  n = c(20,20,20, 20,20,20, 20,20,20, 20,20,20),
  y = c(0,0,0, 0,0,0, 20,14,18, 0,0,0)
)
dat_gymnospermium$dish <- paste0("D_", seq_len(nrow(dat_gymnospermium)))

# [4] Lespedeza
dat_lespedeza <- data.frame(
  treatment = factor(rep(c(0, 1, 3, 6, 12, 24, 48, 96, 192, 384), each = 3)), 
  n    = c(77, 79, 69,    78, 53, 66,    82, 73, 45,    56, 69, 92,    122, 68, 108,
           146, 77, 108,  121, 119, 119, 71, 106, 66,   70, 77, 48,    108, 71, 101),
  y    = c(1, 8, 9,       10, 4, 7,      22, 24, 6,     19, 32, 28,    53, 25, 53,
           84, 48, 71,    119, 119, 117, 71, 101, 66,   70, 77, 46,    68, 53, 77)
)
dat_lespedeza$dish <- paste0("D_", seq_len(nrow(dat_lespedeza)))

# [5] Tiarella
dat_tiarella_cold <- data.frame(
  treatment = factor(rep(c(0, 12), each = 3)), 
  n = c(30,30,30, 30,30,30),
  y = c(20,21,19, 10,13,18)
)
dat_tiarella_cold$dish <- paste0("D_", seq_len(nrow(dat_tiarella_cold)))

dat_tiarella_ga <- data.frame(
  treatment = factor(rep(c(0, 10, 100, 1000), each = 4)), 
  n = c(30,30,30,30, 30,30,30,30, 30,30,30,30, 30,30,30,30),
  y = c(12,12,4,12, 7,18,19,10, 14,13,12,12, 21,24,21,21)
)
dat_tiarella_ga$dish <- paste0("D_", seq_len(nrow(dat_tiarella_ga)))

# [6] Nandina
dat_nandina_temp <- data.frame(
  treatment = factor(c(rep("20C",3), rep("25/15C",4), rep("20/10C",4), rep("15/6C",3), rep("5C",3))),
  n = c(16,26,18, 19,19,19,24, 27,26,25,22, 20,20,20, 20,20,20),
  y = c(13,24,18, 18,19,15,24, 12,6,6,5, 0,0,0, 0,0,0)
)
dat_nandina_temp$dish <- paste0("D_", seq_len(nrow(dat_nandina_temp)))

dat_nandina_ga <- data.frame(
  treatment = factor(rep(c(0, 10, 100, 1000), each = 3)), 
  n = c(20,20,20, 20,20,20, 13,16,16, 15,12,13),
  y = c(0,0,0, 0,0,0, 2,7,2, 12,11,10) 
)
dat_nandina_ga$dish <- paste0("D_", seq_len(nrow(dat_nandina_ga)))

dat_nandina_light <- data.frame(
  treatment = factor(rep(c("light", "dark"), each = 3)),
  n = c(44,46,45, 43,43,43),
  y = c(39,42,37, 40,42,42)
)
dat_nandina_light$dish <- paste0("D_", seq_len(nrow(dat_nandina_light)))

# [7] Viola
dat_viola_t1_cold <- data.frame(
  treatment = factor(rep(c(0, 4, 8), each = 3)),
  n = c(14,14,6, 9,11,14, 11,13,6),
  y = c(13,14,6, 8,10,14, 11,13,5)
)
dat_viola_t1_cold$dish <- paste0("D_", seq_len(nrow(dat_viola_t1_cold)))

dat_viola_t1_ga <- data.frame(
  treatment = factor(rep(c(0, 10, 100), each = 3)), 
  n = c(14,11,16, 6,29,8, 14,19,24),
  y = c(12,11,16, 6,24,4, 8,16,17)
)
dat_viola_t1_ga$dish <- paste0("D_", seq_len(nrow(dat_viola_t1_ga)))

dat_viola_t2_cold <- data.frame(
  treatment = factor(rep(c(0, 4, 8, 16), each = 3)), 
  n = c(16,13,20, 20,13,19, 20,20,15, 19,15,20),
  y = c(5,5,9, 12,8,10, 16,13,10, 16,15,17)
)
dat_viola_t2_cold$dish <- paste0("D_", seq_len(nrow(dat_viola_t2_cold)))

dat_viola_t2_ga <- data.frame(
  treatment = factor(rep(c(0, 1000), each = 3)), 
  n = c(30,30,30, 7,7,7),
  y = c(12,14,18, 7,7,7)
)
dat_viola_t2_ga$dish <- paste0("D_", seq_len(nrow(dat_viola_t2_ga)))

dat_viola_t3_ga <- data.frame(
  treatment = factor(rep(c(0, 10, 100, 1000), each = 3)), 
  n = c(20,20,20, 20,20,20, 20,20,20, 20,20,20),
  y = c(16,20,15, 20,20,14, 20,19,20, 20,20,20)
)
dat_viola_t3_ga$dish <- paste0("D_", seq_len(nrow(dat_viola_t3_ga)))

dat_viola_t4_cold <- data.frame(
  treatment = factor(rep(c(0, 12), each = 3)), 
  n = c(18,20,20, 20,20,20),
  y = c(1,11,0, 9,14,9)
)
dat_viola_t4_cold$dish <- paste0("D_", seq_len(nrow(dat_viola_t4_cold)))

# ==============================================================================
# [4. Dataset List]
# ==============================================================================

germ_datasets <- list(
  "Asarum - Cold" = list(data = dat_asarum_cold, trt_col = "treatment"),
  "Asarum - GA" = list(data = dat_asarum_ga, trt_col = "treatment"),
  "Gymnospermium" = list(data = dat_gymnospermium, trt_col = "treatment"),
  "Lespedeza" = list(data = dat_lespedeza, trt_col = "treatment"),
  "Tiarella - Cold" = list(data = dat_tiarella_cold, trt_col = "treatment"),
  "Tiarella - GA" = list(data = dat_tiarella_ga, trt_col = "treatment"),
  "Nandina - Temp" = list(data = dat_nandina_temp, trt_col = "treatment"),
  "Nandina - GA" = list(data = dat_nandina_ga, trt_col = "treatment"),
  "Nandina - Light" = list(data = dat_nandina_light, trt_col = "treatment"),
  "Viola T1 - Cold" = list(data = dat_viola_t1_cold, trt_col = "treatment"),
  "Viola T1 - GA" = list(data = dat_viola_t1_ga, trt_col = "treatment"),
  "Viola T2 - Cold" = list(data = dat_viola_t2_cold, trt_col = "treatment"),
  "Viola T2 - GA" = list(data = dat_viola_t2_ga, trt_col = "treatment"),
  "Viola T3 - GA" = list(data = dat_viola_t3_ga, trt_col = "treatment"),
  "Viola T4 - Cold" = list(data = dat_viola_t4_cold, trt_col = "treatment")
)


# ==============================================================================
# [5. One-by-One Analysis for All Datasets]
# ===============================================================================

analyze_one_dataset <- function(nm, obj,
                                y_col = "y",
                                n_col = "n",
                                dish_col = "dish",
                                alpha = 0.05,
                                p_adjust_method = "holm",
                                save_result = TRUE,
                                output_dir = "analysis_results") {

  cat("\n\n##########################################################\n")
  cat("### Dataset:", nm, "\n")
  cat("##########################################################\n")

  dat <- obj$data
  trt_col <- obj$trt_col

  # Phase 1: dish-level random effect assessment for this dataset only
  dish_result <- check_dish_effect(
    data = dat,
    trt_col = trt_col,
    dataset_name = nm,
    y_col = y_col,
    n_col = n_col,
    dish_col = dish_col,
    alpha = alpha
  )

  # Phase 2: scenario classification and final analysis for this dataset only
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

  # Safe file prefix
  safe_nm <- gsub("[^A-Za-z0-9]+", "_", nm)
  safe_nm <- gsub("(^_+|_+$)", "", safe_nm)

  if (save_result) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    summary_df <- data.frame(
      dataset = nm,
      scenario = final_result$scenario,
      method = final_result$method,
      dish_p_value = dish_result$dish_p_value,
      dish_variance = dish_result$dish_variance,
      dish_significant = dish_result$dish_significant,
      extreme_groups = paste(dish_result$extreme_groups, collapse = "; "),
      main_p_value = final_result$main_p_value,
      stringsAsFactors = FALSE
    )

    write.csv(
      summary_df,
      file = file.path(output_dir, paste0(safe_nm, "_summary.csv")),
      row.names = FALSE
    )

    if (!is.null(final_result$cld_table)) {
      write.csv(
        final_result$cld_table,
        file = file.path(output_dir, paste0(safe_nm, "_CLD.csv")),
        row.names = FALSE
      )
    }

    if (!is.null(final_result$pairwise_table)) {
      write.csv(
        final_result$pairwise_table,
        file = file.path(output_dir, paste0(safe_nm, "_pairwise.csv")),
        row.names = FALSE
      )
    }
  }

  invisible(list(
    data = dat,
    dish_result = dish_result,
    final_result = final_result
  ))
}

run_entire_analysis <- function(germ_datasets,
                                y_col = "y",
                                n_col = "n",
                                dish_col = "dish",
                                alpha = 0.05,
                                p_adjust_method = "holm",
                                save_result = TRUE,
                                output_dir = "analysis_results") {

  all_results <- list()
  summary_list <- list()

  for (nm in names(germ_datasets)) {

    res <- analyze_one_dataset(
      nm = nm,
      obj = germ_datasets[[nm]],
      y_col = y_col,
      n_col = n_col,
      dish_col = dish_col,
      alpha = alpha,
      p_adjust_method = p_adjust_method,
      save_result = save_result,
      output_dir = output_dir
    )

    all_results[[nm]] <- res

    summary_list[[nm]] <- data.frame(
      dataset = nm,
      scenario = res$final_result$scenario,
      method = res$final_result$method,
      dish_p_value = res$dish_result$dish_p_value,
      dish_variance = res$dish_result$dish_variance,
      dish_significant = res$dish_result$dish_significant,
      extreme_groups = paste(res$dish_result$extreme_groups, collapse = "; "),
      main_p_value = res$final_result$main_p_value,
      stringsAsFactors = FALSE
    )
  }

  combined_summary <- do.call(rbind, summary_list)
  rownames(combined_summary) <- NULL

  if (save_result) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    write.csv(
      combined_summary,
      file = file.path(output_dir, "all_datasets_summary.csv"),
      row.names = FALSE
    )
  }

  invisible(list(
    all_results = all_results,
    combined_summary = combined_summary
  ))
}

# ==============================================================================
# [6. Run]
# ===============================================================================

entire_results <- run_entire_analysis(
  germ_datasets = germ_datasets,
  y_col = "y",
  n_col = "n",
  dish_col = "dish",
  alpha = 0.05,
  p_adjust_method = "holm",
  save_result = TRUE,
  output_dir = "analysis_results"
)

# Print compact combined summary on console
print(entire_results$combined_summary)
