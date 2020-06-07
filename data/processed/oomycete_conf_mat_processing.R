library(tidyverse)


# Read all results from effectR
pred_result_fungi_effectR <- data.table::fread("data/raw/oomycete_pred_effectR_testing.csv", drop = c("V1")) %>% 
  dplyr::rename(effectR = pred)

# Get the prediction results from deepredeff
oomycete_pred_ens <-  data.table::fread("data/raw/oomycete_pred_ens.csv", drop = c("V1"))

# Get the real data 
oomycete_test_data <- data.table::fread("data/raw/oomycete_testing.csv")

# Combine real data and deepredeff results to get the true label

oomycete_pred_ens <- oomycete_pred_ens %>% 
  dplyr::left_join(., oomycete_test_data, by = c("sequence" = "Sequence")) 


all_oomycete_pred <- oomycete_pred_ens %>% 
  dplyr::left_join(., pred_result_fungi_effectR, by = c("sequence" = "id", "label" = "true_label")) %>% 
  # if they are not predicted as effector, then non-effector
  dplyr::mutate(effectR = ifelse(is.na(effectR), 0, effectR)) %>% 
  dplyr::rename(true_label = label)

# Save to the model
readr::write_csv(all_oomycete_pred, "data/processed/all_oomycete_pred_deepredeff_effectR.csv")


model_list <- c("cnn_lstm", "effectR")

# Get the confusion matrix data
confusion_mat_oomycete_df <- all_oomycete_pred %>% 
  dplyr::mutate_all(function(x) factor(x, levels = c(1, 0))) %>% 
  dplyr::select(-c(ensemble_weighted)) %>% 
  tidyr::pivot_longer(-true_label, names_to = "model", values_to = "Prediction") %>% 
  dplyr::mutate(model = factor(model, levels = model_list)) %>% 
  table() %>%
  as.data.frame()

readr::write_csv(confusion_mat_oomycete_df, "data/processed/oomycete_confusion_mat_df.csv")
