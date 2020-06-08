library(tidyverse)


# get_data_ready <- function(data) {
#   data <- data %>% 
#     dplyr::select(-c("Probability")) %>% 
#     `colnames<-`(c("identifier", "prediction")) %>% 
#     dplyr::mutate(
#       prediction = case_when(
#         prediction == "Effector" ~ 1, 
#         TRUE ~ 0
#       ),
#       label = stringr::str_remove_all(identifier, ".*_"), 
#       identifier =  stringr::str_extract(identifier, "[^_]*")
#     )
#   
#   return(data)
# }

fungi_test_effectorp_1_2 <- data.table::fread("data/raw/fungi_test_effectorp1_results.csv") %>% 
  dplyr::mutate(label = as.numeric(gsub(".*_", "", Identifier))) %>% 
  dplyr::rename(effectorp_1 = Prediction) %>% 
  dplyr::select(-c(Probability)) %>% 
  dplyr::left_join(
    data.table::fread("data/raw/fungi_test_effectorp2_results.csv") %>% 
      dplyr::mutate(label = as.numeric(gsub(".*_", "", Identifier))) %>% 
      dplyr::rename(effectorp_2 = Prediction) %>% 
      dplyr::select(-c(Probability)), 
    by = c("Identifier", "label")
  ) %>% 
  dplyr::mutate(
    Identifier = substr(Identifier, 1, nchar(Identifier) - 2),
    effectorp_1 = ifelse(effectorp_1 == "Effector", 1, 0), 
    effectorp_2 = ifelse(effectorp_2 == "Effector", 1, 0)
  ) %>% 
  dplyr::rename(true_label = label)

fungi_ensemble_results <- data.table::fread("data/raw/fungi_pred_ens.csv", drop = "V1") 

# Put together the results prediction using deepredeff vs using both effectorp_1 and effectorp_2
all_pred_together <- data.table::fread("data/raw/fungi_pred_ens.csv", drop = "V1")  %>% 
  dplyr::left_join(fungi_test_effectorp_1_2, by = c("sequence" = "Identifier"))

readr::write_csv(all_pred_together, "data/processed/all_fungi_pred_deepredeff_effectorp_1_2.csv")

model_list <- c("cnn_lstm", "effectorp_1", "effectorp_2" )

confusion_mat_df <- all_pred_together %>% 
  dplyr::mutate_all(function(x) factor(x, levels = c(1, 0))) %>% 
  dplyr::select(c(model_list, true_label)) %>% 
  tidyr::pivot_longer(-true_label, names_to = "model", values_to = "Prediction") %>% 
  dplyr::mutate(model = factor(model, levels = model_list)) %>% 
  table() %>%
  as.data.frame()

readr::write_csv(confusion_mat_df, "data/processed/fungi_confusion_mat_df.csv")
