library(tidyverse)

bacteria_test_effectiveT3 <- data.table::fread(
  "data/raw/bacteria_pred_effectiveT3_testing.csv", drop = "V1"
) %>% 
  dplyr::arrange(desc(label)) %>% 
  dplyr::select(-c("identifier")) %>% 
  `colnames<-`(c("effectiveT3", "true_label"))

bacteria_test_deept3 <- data.table::fread(
  "data/raw/bacteria_pred_DeepT3_testing.csv", 
  drop = "V1"
) %>% 
  `colnames<-`("deepT3")

bacteria_ensemble_results <- data.table::fread(
  "data/raw/bacteria_pred_ens.csv",
  drop = c("V1", "sequence")
)

all_bacteria_pred <- bacteria_ensemble_results %>%
    cbind(., bacteria_test_deept3, bacteria_test_effectiveT3)

model_list <- c("ensemble_weighted", "deepT3", "effectiveT3")

confusion_mat_bacteria_df <- all_bacteria_pred %>% 
  dplyr::mutate_all(function(x) factor(x, levels = c(1, 0))) %>% 
  tidyr::pivot_longer(-true_label, names_to = "model", values_to = "Prediction") %>% 
  dplyr::mutate(model = factor(model, levels = model_list)) %>% 
  table() %>%
  as.data.frame()

readr::write_csv(confusion_mat_bacteria_df, "data/processed/bacteria_confusion_mat_df.csv")
