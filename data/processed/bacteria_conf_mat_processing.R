library(tidyverse)

# Get results from effectiveT3 ---

effectivet3_using_plants_module <- read_lines("data/raw/bacteria_pred_effectiveT3_testing_using_plant_module.out")

col_names <- effectivet3_using_plants_module %>%
  .[2] %>%
  stringr::str_split(";") %>%
  unlist() %>%
  stringr::str_trim() %>%
  stringr::str_to_lower() %>%
  stringr::str_replace_all(" ", "_")

bacteria_test_effectiveT3 <- effectivet3_using_plants_module %>%
  # Get rid of header
  .[-c(1, 2)] %>%
  stringr::str_split(";") %>%
  purrr::reduce(rbind) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  `colnames<-`(col_names) %>%
  mutate(
    score = as.numeric(score),
    is_effective = (is_effective == "true")
  ) %>%
  dplyr::as_tibble()

bacteria_test_effectiveT3 <- bacteria_test_effectiveT3 %>% 
  dplyr::select(-c("description", "score")) %>% 
  `colnames<-`(c("identifier", "effectiveT3")) %>% 
  dplyr::mutate(effectiveT3= case_when(
    effectiveT3 == "TRUE" ~ 1, 
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(true_label = stringr::str_remove_all(identifier, ".*_"), 
                identifier = stringr::str_extract(identifier, "[^_]*"))



bacteria_test_deept3 <- data.table::fread(
    "data/raw/bacteria_pred_DeepT3_testing.txt", header = FALSE
  ) %>% 
  `colnames<-`(c("deepT3")) %>% 
  dplyr::mutate(deepT3 = case_when(
    deepT3 == "T3SE" ~ 1, 
    TRUE ~ 0
  ))

bacteria_ensemble_results <- data.table::fread(
  "data/raw/bacteria_pred_ens.csv",
  drop = c("V1")
)

bacteria_pred_deepredeff_deept3 <- bacteria_ensemble_results %>% 
    cbind(., bacteria_test_deept3)

all_bacteria_pred <- bacteria_pred_deepredeff_deept3 %>%
  dplyr::left_join(bacteria_test_effectiveT3, by = c("sequence" = "identifier"))

readr::write_csv(all_bacteria_pred, "data/processed/all_bacteria_pred_deepredeff_deept3_effectiveT3.csv")
    

model_list <- c("ensemble_weighted", "deepT3", "effectiveT3")

confusion_mat_bacteria_df <- all_bacteria_pred %>% 
  dplyr::mutate_all(function(x) factor(x, levels = c(1, 0))) %>% 
  tidyr::pivot_longer(-true_label, names_to = "model", values_to = "Prediction") %>% 
  dplyr::mutate(model = factor(model, levels = model_list)) %>% 
  table() %>%
  as.data.frame()

readr::write_csv(confusion_mat_bacteria_df, "data/processed/bacteria_confusion_mat_df.csv")
