library(dplyr)

# Function to get true and pred value from effectorp output
get_true_pred_value_from_effectorp_pred <- function(data_path, label_data){
  df <- data.table::fread(data_path) %>% 
    dplyr::mutate(
      pred = factor(ifelse(Prediction == "Effector", 1, 0), levels = c(1, 0), labels = c("effector", "non-effector")), 
      label = factor(ifelse(label_data == "effector", 1, 0), levels = c(1, 0), labels = c("effector", "non-effector"))
    )
  return(df)
}

# Function to get true and pred from deepredeff output
get_true_pred_value_from_deepredeff <- function(data_path, label_data) {
  df <- data.table::fread(data_path) %>% 
    dplyr::mutate(
      pred = factor(ifelse(prob < 0.5, 0, 1), levels = c(1, 0), labels = c("effector", "non-effector")),
      label = factor(ifelse(label_data == "effector", 1, 0), levels = c(1, 0), labels = c("effector", "non-effector"))
    )
  return(df)
}

# Get confusion matrix from both
get_confusion_mat <- function(data_frame, label_data, data_origin, tool_used){
  
  if (tool_used != "cnn_lstm"){
    df <- get_true_pred_value_from_effectorp_pred(data_frame, label_data = label_data)
  } else {
    df <- get_true_pred_value_from_deepredeff(data_frame, label_data = label_data) 
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
acc_f1_score_effectorp_data_prediction <- rbind(
  rbind(
    get_true_pred_value_from_deepredeff("data/raw/effectorP1_pos_training_evaluated_on_deepredeff.csv", label_data = "effector") %>%
      mutate(data = "effectorP1_training", tool = "cnn_lstm"), 
    get_true_pred_value_from_deepredeff("data/raw/effectorP2_pos_training_evaluated_on_deepredeff.csv", label_data = "effector") %>%
      mutate(data = "effectorP2_training", tool = "cnn_lstm"), 
    get_true_pred_value_from_deepredeff("data/raw/effectorP2_pos_validation_evaluated_on_deepredeff.csv", label_data = "effector") %>%
      mutate(data = "effectorP2_validation", tool = "cnn_lstm")
  ) %>% 
    group_by(data, tool) %>% 
    metrics_list(truth = label, estimate = pred), 
  rbind(
    get_true_pred_value_from_effectorp_pred("data/raw/effectorP1_pos_training_evaluated_on_effectorP1.csv", label_data = "effector") %>% 
      mutate(data = "effectorP1_training", tool = "effectorP1"), 
    get_true_pred_value_from_effectorp_pred("data/raw/effectorP2_pos_training_evaluated_on_effectorP1.csv", label_data = "effector") %>% 
      mutate(data = "effectorP2_training", tool = "effectorP1"), 
    get_true_pred_value_from_effectorp_pred("data/raw/effectorP2_pos_validation_evaluated_on_effectorP1.csv", label_data = "effector") %>% 
      mutate(data = "effectorP2_validation", tool = "effectorP1"), 
    get_true_pred_value_from_effectorp_pred("data/raw/effectorP1_pos_training_evaluated_on_effectorP2.csv", label_data = "effector") %>% 
      mutate(data = "effectorP1_training", tool = "effectorP2"), 
    get_true_pred_value_from_effectorp_pred("data/raw/effectorP2_pos_training_evaluated_on_effectorP2.csv", label_data = "effector") %>% 
      mutate(data = "effectorP2_training", tool = "effectorP2"),
    get_true_pred_value_from_effectorp_pred("data/raw/effectorP2_pos_validation_evaluated_on_effectorP2.csv", label_data = "effector") %>% 
      mutate(data = "effectorP2_validation", tool = "effectorP2")
  ) %>% 
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
  )

# Rearrange the table
acc_f1_score_effectorp_data_prediction <- acc_f1_score_effectorp_data_prediction %>% 
  dplyr::rename(Data = data, Tool = tool) %>% 
  dplyr::arrange(Tool)

readr::write_csv(acc_f1_score_effectorp_data_prediction, "data/processed/acc_f1score_result_effectorp_data_prediction.csv")
  
# Get the confusion mat df
conf_mat_effectorp_data_prediction <- rbind(
  get_confusion_mat("data/raw/effectorP1_pos_training_evaluated_on_deepredeff.csv", label_data = "effector", data_origin = "effectorP1_trainining", tool_used = "cnn_lstm"), 
  get_confusion_mat("data/raw/effectorP2_pos_training_evaluated_on_deepredeff.csv", label_data = "effector", data_origin = "effectorP2_trainining", tool_used = "cnn_lstm"), 
  get_confusion_mat("data/raw/effectorP2_pos_validation_evaluated_on_deepredeff.csv", label_data = "effector", data_origin = "effectorP2_validation", tool_used = "cnn_lstm"),
  get_confusion_mat("data/raw/effectorP1_pos_training_evaluated_on_effectorP1.csv", label_data = "effector", data_origin = "effectorP1_trainining", tool_used = "effectorP1"), 
  get_confusion_mat("data/raw/effectorP2_pos_training_evaluated_on_effectorP1.csv", label_data = "effector", data_origin = "effectorP2_trainining", tool_used = "effectorP1"), 
  get_confusion_mat("data/raw/effectorP2_pos_validation_evaluated_on_effectorP1.csv", label_data = "effector", data_origin = "effectorP2_validation", tool_used = "effectorP1"),
  get_confusion_mat("data/raw/effectorP1_pos_training_evaluated_on_effectorP2.csv", label_data = "effector", data_origin = "effectorP1_trainining", tool_used = "effectorP2"), 
  get_confusion_mat("data/raw/effectorP2_pos_training_evaluated_on_effectorP2.csv", label_data = "effector", data_origin = "effectorP2_trainining", tool_used = "effectorP2"),
  get_confusion_mat("data/raw/effectorP2_pos_validation_evaluated_on_effectorP2.csv", label_data = "effector", data_origin = "effectorP2_validation", tool_used = "effectorP2")
)

readr::write_csv(conf_mat_effectorp_data_prediction, "data/processed/conf_mat_effectorp_data_prediction.csv")

