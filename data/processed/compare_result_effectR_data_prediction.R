library(dplyr)

# Function to get true and pred from deepredeff output
get_true_pred_value_from_deepredeff <- function(data_path, label_data) {
  df <- data.table::fread(data_path) %>% 
    dplyr::mutate(
      pred = factor(ifelse(prob < 0.5, 0, 1), levels = c(1, 0), labels = c("effector", "non-effector")),
      label = factor(ifelse(label_data == "effector", 1, 0), levels = c(1, 0), labels = c("effector", "non-effector"))
    )
  return(df)
}

get_confusion_mat <- function(data_frame, filter){
  
  
  conf_mat_df <- data_frame %>%
    dplyr::filter(tool == filter) %>%
    yardstick::conf_mat(truth = label, estimate = value) %>% 
    .$table %>% 
    as.data.frame() %>% 
    filter(Truth != "non-effector") %>% 
    mutate(
      tool = filter
    )
  
  return(conf_mat_df)
}


# Get all accuracy and f1_score
metrics_list <- yardstick::metric_set(
  yardstick::accuracy,
  yardstick::f_meas
)



effectR_data_evaluated_on_deepredeff_and_effectR <- get_true_pred_value_from_deepredeff(
  "data/raw/effectR_reference_rxlr_effectors_evaluated_on_deepredeff.csv", 
  label_data = "effector"
) %>% 
  mutate(
    name = sub(" .*", "", name)
  ) %>% 
  select(name, label, pred_cnn_lstm = pred) %>% 
  left_join(
    data.table::fread("data/raw/effectR_reference_rxlr_effectors_evaluated_on_effectR.csv") %>% 
      select(name, pred_effectR = candidate), 
    by = "name"
  ) %>% 
  dplyr::mutate(
    pred_effectR = ifelse(is.na(pred_effectR), "non-effector", "effector")
  ) %>% 
  tidyr::pivot_longer(
    cols = -c(name, label),
    names_to = "tool"
  ) %>% 
  dplyr::mutate(
    value = factor(value, levels = c("effector", "non-effector")),
    label = factor(label, levels = c("effector", "non-effector"))
  ) 

acc_f1_score_effectR_data_prediction <- effectR_data_evaluated_on_deepredeff_and_effectR %>% 
  group_by(tool) %>% 
  metrics_list(truth = label, estimate = value)  %>%
  dplyr::mutate(
    .metric = dplyr::case_when(
      `.metric` == "accuracy" ~ "Accuracy",
      `.metric` == "f_meas" ~ "F1 Score"
    ), 
    tool = dplyr::case_when(
      tool == "pred_cnn_lstm" ~ "cnn_lstm",
      tool == "pred_effectR" ~ "effectR"
    )
  ) %>% 
  dplyr::select(-c(`.estimator`)) %>% 
  tidyr::pivot_wider(
    id_cols = c(tool),
    names_from = ".metric",
    values_from = ".estimate"
  ) %>% 
  dplyr::rename(Tool = tool)

readr::write_csv(acc_f1_score_effectR_data_prediction, "data/processed/acc_f1_score_effectR_data_prediction.csv")

conf_mat_effectR_data_prediction <- rbind(
get_confusion_mat(effectR_data_evaluated_on_deepredeff_and_effectR, "pred_cnn_lstm"),
get_confusion_mat(effectR_data_evaluated_on_deepredeff_and_effectR, "pred_effectR")
) %>% 
  mutate(
    tool = dplyr::case_when(
      tool == "pred_cnn_lstm" ~ "cnn_lstm",
      tool == "pred_effectR" ~ "effectR"
    )
  )

readr::write_csv(conf_mat_effectR_data_prediction, "data/processed/conf_mat_effectR_data_prediction.csv")
