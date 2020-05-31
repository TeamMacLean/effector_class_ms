library(tidyverse)

all_oomycete_pred <-  data.table::fread("data/raw/oomycete_pred_all.csv", drop = c("V1", "sequence"))

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
