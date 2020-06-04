library(tidyverse)


get_data_ready <- function(data) {
  data <- data %>% 
    dplyr::select(-c("Probability")) %>% 
    `colnames<-`(c("identifier", "prediction")) %>% 
    dplyr::mutate(
      prediction = case_when(
        prediction == "Effector" ~ 1, 
        TRUE ~ 0
      ),
      label = stringr::str_remove_all(identifier, ".*_"), 
      identifier =  stringr::str_extract(identifier, "[^_]*")
    )
  
  return(data)
}

fungi_test_effectorp_1 <- get_data_ready(data.table::fread("data/raw/fungi_pred_effectorp_1_testing.csv"))
fungi_test_effectorp_2 <- get_data_ready(data.table::fread("data/raw/fungi_pred_effectorp_2_testing.csv"))
fungi_ensemble_results <- data.table::fread("data/raw/fungi_pred_ens.csv", drop = "V1")

all_pred_together <- fungi_ensemble_results %>%
  dplyr::select(-c(sequence)) %>% 
  cbind(fungi_test_effectorp_1 %>% 
          dplyr::select(prediction) %>% 
          `colnames<-` (c("effectorp_1")), 
        fungi_test_effectorp_2 %>% 
          dplyr::select(-c("identifier")) %>% 
          `colnames<-` (c("effectorp_2", "true_label")))

model_list <- c("cnn_lstm", "effectorp_1", "effectorp_2" )

confusion_mat_df <- all_pred_together %>% 
  dplyr::mutate_all(function(x) factor(x, levels = c(1, 0))) %>% 
  dplyr::select(c(model_list, true_label)) %>% 
  tidyr::pivot_longer(-true_label, names_to = "model", values_to = "Prediction") %>% 
  dplyr::mutate(model = factor(model, levels = model_list)) %>% 
  table() %>%
  as.data.frame()

readr::write_csv(confusion_mat_df, "data/processed/fungi_confusion_mat_df.csv")
