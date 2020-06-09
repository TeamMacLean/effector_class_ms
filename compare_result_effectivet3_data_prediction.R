library(dplyr)

# Function to get true and pred value from effectorp output
get_true_pred_value_from_deepredeff <- function(data_path, label_data) {
  df <- data.table::fread(data_path) %>% 
    dplyr::mutate(
      pred = factor(ifelse(prob < 0.5, 0, 1), levels = c(1, 0), labels = c("effector", "non-effector")),
      label = factor(ifelse(label_data == "effector", 1, 0), levels = c(1, 0), labels = c("effector", "non-effector"))
    )
  return(df)
}


get_true_pred_value_from_effectivet3 <- function(data_path, label_data){
  
  df <- data.table::fread(data_path, skip = 1) %>% 
    dplyr::select(-c("Description", "Score")) %>% 
    `colnames<-`(c("id", "is_effective")) %>% 
    dplyr::mutate(
      pred = factor(ifelse(is_effective == TRUE, 1, 0), levels = c(1, 0), labels = c("effector", "non-effector")), 
      label = factor(ifelse(label_data == "effector", 1, 0), levels = c(1, 0), labels = c("effector", "non-effector"))
    )
  
  return(df)
}


# Get confusion matrix from both
get_confusion_mat <- function(data_path, label_data, data_origin, tool_used){
  
  if (tool_used != "ensemble_weighted"){
    df <- get_true_pred_value_from_effectivet3(data_path, label_data = label_data)
  } else {
    df <- get_true_pred_value_from_deepredeff(data_path, label_data = label_data) 
  }
  
  conf_mat_df <- df %>%
    yardstick::conf_mat(truth = label, estimate = pred) %>% 
    .$table %>% 
    as.data.frame() %>% 
    filter(Truth != "non-effector") %>% 
    mutate(
      data = data_origin, 
      tool = tool_used
    )
  
  return(conf_mat_df)
}



# Get all accuracy and f1_score
metrics_list <- yardstick::metric_set(
  yardstick::accuracy,
  yardstick::f_meas
)


acc_f1_score_effectivet3_data_prediction <- rbind(
rbind(
get_true_pred_value_from_effectivet3("data/raw/effectiveT3_positive_training_evaluated_on_effectivet3_on_all_training_data.csv", label_data = "effector") %>% 
  dplyr::mutate(data = "effectivet3_pos", tool = "effectivet3_all"), 
get_true_pred_value_from_effectivet3("data/raw/effectiveT3_positive_training_evaluated_on_effectivet3_on_plant_associated_protein.csv", label_data = "effector") %>% 
  dplyr::mutate(data = "effectivet3_pos", tool = "effectivet3_plant")
) %>% 
  group_by(tool) %>% 
  metrics_list(truth = label, estimate = pred), 
get_true_pred_value_from_deepredeff("data/raw/effectiveT3_positive_training_evaluated_on_deepredeff.csv", label_data = "effector") %>% 
  dplyr::mutate(data = "effective3_pos", tool = "ensemble_weighted") %>% 
  group_by(tool) %>% 
  metrics_list(truth = label, estimate = pred)
) %>%
  dplyr::mutate(
    .metric = dplyr::case_when(
      `.metric` == "accuracy" ~ "Accuracy",
      `.metric` == "f_meas" ~ "F1 Score"
    )
  ) %>% 
  dplyr::select(-c(`.estimator`)) %>% 
  tidyr::pivot_wider(
    id_cols = c(tool),
    names_from = ".metric",
    values_from = ".estimate"
  ) %>% 
  dplyr::rename(Tool = tool) %>% 
  dplyr::arrange(Tool)

readr::write_csv(acc_f1_score_effectivet3_data_prediction, "data/processed/acc_f1score_result_effectivet3_data_prediction.csv")
  
# Get the confusion mat df
conf_mat_effectivet3_data_prediction <- rbind(
get_confusion_mat("data/raw/effectiveT3_positive_training_evaluated_on_effectivet3_on_all_training_data.csv", label_data = "effector", data_origin = "effectivet3_pos", tool_used = "effectivet3_all"), 
get_confusion_mat("data/raw/effectiveT3_positive_training_evaluated_on_effectivet3_on_plant_associated_protein.csv", label_data = "effector", data_origin = "effectivet3_pos", tool_used = "effectivet3_plant"), 
get_confusion_mat("data/raw/effectiveT3_positive_training_evaluated_on_deepredeff.csv", label_data = "effector", data_origin = "effectivet3_pos", tool_used = "ensemble_weighted")
)

readr::write_csv(conf_mat_effectivet3_data_prediction, "data/processed/conf_mat_effectivet3_data_prediction.csv")
