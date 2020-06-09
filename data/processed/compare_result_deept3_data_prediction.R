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

# Function to get true and pred value from effectorp output
get_true_pred_value_from_deept3_pred <- function(data_path, label_data){
  df <- data.table::fread(data_path, header = FALSE) %>% 
    `colnames<-`(c("Prediction")) %>% 
    dplyr::mutate(
      pred = factor(ifelse(Prediction == "T3SE", 1, 0), levels = c(1, 0), labels = c("effector", "non-effector")), 
      label = factor(ifelse(label_data == "effector", 1, 0), levels = c(1, 0), labels = c("effector", "non-effector"))
    )
  return(df)
}

# Get confusion matrix from both
get_confusion_mat <- function(data_path, label_data, data_origin, tool_used){
  
  if (tool_used != "ensemble_weighted"){
    df <- get_true_pred_value_from_deept3_pred(data_path, label_data = label_data)
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

# Calculate accuracy and f1_score
acc_f1_score_deept3_data_prediction <- rbind(
rbind(
  get_true_pred_value_from_deepredeff("data/raw/deept3_P.syringae_nr_effector_data_evaluated_on_deepredeff.csv", label_data = "effector") %>% 
    mutate(data = "deept3_P.syringae_nr_effector", tool = "ensemble_weighted"), 
  get_true_pred_value_from_deepredeff("data/raw/deept3_pos_independent_test_data_evaluated_on_deepredeff.csv", label_data = "effector") %>% 
    mutate(data = "deept3_pos_independent_test", tool = "ensemble_weighted"), 
  get_true_pred_value_from_deepredeff("data/raw/deept3_pos_training_data_evaluated_on_deepredeff.csv", label_data = "effector") %>% 
    mutate(data = "deept3_pos_training", tool = "ensemble_weighted")
) %>% 
  group_by(data, tool) %>% 
  metrics_list(truth = label, estimate = pred),
rbind(
  get_true_pred_value_from_deept3_pred("data/raw/deept3_P.syringae_nr_effector_data_evaluated_on_deept3.csv", label_data = "effector") %>% 
    mutate(data = "deept3_P.syringae_nr_effector", tool = "deept3"), 
  get_true_pred_value_from_deept3_pred("data/raw/deept3_pos_independent_test_data_evaluated_on_deept3.csv", label_data = "effector") %>% 
    mutate(data = "deept3_pos_independent_test", tool = "deept3"),
  get_true_pred_value_from_deept3_pred("data/raw/deept3_pos_training_data_evaluated_on_deept3.csv", label_data = "effector") %>% 
    mutate(data = "deept3_pos_training", tool = "deept3")
)  %>% 
  group_by(data, tool) %>% 
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
    id_cols = c(data, tool),
    names_from = ".metric",
    values_from = ".estimate"
  ) %>% 
  dplyr::rename(Data = data, Tool = tool)

# Save into a table 
readr::write_csv(acc_f1_score_deept3_data_prediction, "data/processed/acc_f1score_result_deept3_data_prediction.csv")

# Get the confusion mat df
conf_mat_deept3_data_prediction <- rbind(
  get_confusion_mat("data/raw/deept3_P.syringae_nr_effector_data_evaluated_on_deepredeff.csv", label_data = "effector", data_origin = "deept3_P.syringae_nr_effector", tool_used = "ensemble_weighted"), 
  get_confusion_mat("data/raw/deept3_pos_independent_test_data_evaluated_on_deepredeff.csv", label_data = "effector", data_origin = "deept3_pos_independent_test", tool_used = "ensemble_weighted"), 
  get_confusion_mat("data/raw/deept3_pos_training_data_evaluated_on_deepredeff.csv", label_data = "effector", data_origin = "deept3_pos_training", tool_used = "ensemble_weighted"),
  get_confusion_mat("data/raw/deept3_P.syringae_nr_effector_data_evaluated_on_deept3.csv", label_data = "effector", data_origin = "deept3_P.syringae_nr_effector", tool_used = "deept3"), 
  get_confusion_mat("data/raw/deept3_pos_independent_test_data_evaluated_on_deept3.csv", label_data = "effector", data_origin = "deept3_pos_independent_test", tool_used = "deept3"),
  get_confusion_mat("data/raw/deept3_pos_training_data_evaluated_on_deept3.csv", label_data = "effector", data_origin = "deept3_pos_training", tool_used = "deept3")
)

readr::write_csv(conf_mat_deept3_data_prediction, "data/processed/conf_mat_deept3_data_prediction.csv")
