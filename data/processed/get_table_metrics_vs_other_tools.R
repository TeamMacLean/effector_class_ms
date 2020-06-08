library(dplyr)

metrics_list <- yardstick::metric_set(
  yardstick::accuracy,
  yardstick::f_meas
)

compute_metrics <- function(data_path, model_list) {
  res <- data.table::fread(data_path) %>%
    dplyr::mutate_if(
      .predicate = is.integer,
      .funs = function(x) factor(x, levels = c(1,0), labels = c("effector", "non-effector"))
    ) %>%
    tidyr::pivot_longer(
      cols = -c(sequence, true_label),
      names_to = "model",
      values_to = "prediction"
    ) %>%
    dplyr::filter(model %in% model_list) %>%
    dplyr::mutate(
      model = factor(model, levels = model_list)
    ) %>%
    dplyr::group_by(model) %>%
    metrics_list(
      truth = true_label,
      estimate = prediction
    )
  
  return(res)
}

acc_f1score_vs_othertools <- dplyr::bind_rows(
  compute_metrics(
    data_path = "data/processed/all_bacteria_pred_deepredeff_deept3_effectiveT3.csv",
    model_list = c("ensemble_weighted", "deepT3", "effectiveT3")
  ) %>%
    dplyr::mutate(groups = "Bacteria"),
  compute_metrics(
    data_path = "data/processed/all_fungi_pred_deepredeff_effectorp_1_2.csv",
    model_list = c("cnn_lstm", "effectorp_1", "effectorp_2")
  ) %>%
    dplyr::mutate(groups = "Fungi"),
  compute_metrics(
    data_path = "data/processed/all_oomycete_pred_deepredeff_effectR.csv",
    model_list = c("cnn_lstm", "effectR")
  ) %>%
    dplyr::mutate(groups = "Oomycete")
) %>%
  dplyr::mutate(
    .metric = dplyr::case_when(
      .metric == "accuracy" ~ "Accuracy",
      .metric == "f_meas" ~ "F1 Score"
    )
  )


readr::write_csv(acc_f1score_vs_othertools, "data/processed/acc_f1score_vs_othertools.csv")
