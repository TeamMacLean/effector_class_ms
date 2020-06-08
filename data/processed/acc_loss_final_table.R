get_acc_loss_table <- function(path) {
  group <- path %>%
    stringr::str_match("\\/\\s*(.*?)\\s*_") %>%
    .[[2]] %>%
    stringr::str_remove_all("processed/") %>%
    stringr::str_to_title()
  
  dataset <- path %>%
    stringr::str_match("hist_\\s*(.*?)\\s*.csv") %>%
    .[[2]] %>%
    switch(
      train = "Training",
      val = "Validation"
    )
  
  data.table::fread(path) %>%
    dplyr::mutate(
      Group = group, `Data set` = dataset
    ) %>%
    dplyr::select(
      Group, `Data set`, Model = model,
      Accuracy = acc, Loss = loss,
      Epoch = epochs
    )
}

acc_loss_final <- dplyr::bind_rows(
  get_acc_loss_table("data/processed/bacteria_hist_train.csv") %>%
    dplyr::filter(
      (Model == "CNN-LSTM" & Epoch == 30) |
        (Model == "CNN-GRU" & Epoch == 30) |
        (Model == "LSTM-Emb" & Epoch == 30) |
        (Model == "GRU-Emb" & Epoch == 30)
    ),
  get_acc_loss_table("data/processed/bacteria_hist_val.csv") %>%
    dplyr::filter(
      (Model == "CNN-LSTM" & Epoch == 30) |
        (Model == "CNN-GRU" & Epoch == 30) |
        (Model == "LSTM-Emb" & Epoch == 30) |
        (Model == "GRU-Emb" & Epoch == 30)
    ),
  get_acc_loss_table("data/processed/fungi_hist_train.csv") %>%
    dplyr::filter(
      (Model == "CNN-LSTM" & Epoch == 30) |
        (Model == "CNN-GRU" & Epoch == 60) |
        (Model == "LSTM-Emb" & Epoch == 60) |
        (Model == "GRU-Emb" & Epoch == 30)
    ),
  get_acc_loss_table("data/processed/fungi_hist_val.csv") %>%
    dplyr::filter(
      (Model == "CNN-LSTM" & Epoch == 30) |
        (Model == "CNN-GRU" & Epoch == 60) |
        (Model == "LSTM-Emb" & Epoch == 60) |
        (Model == "GRU-Emb" & Epoch == 30)
    ),
  get_acc_loss_table("data/processed/oomycete_hist_train.csv") %>%
    dplyr::filter(
      (Model == "CNN-LSTM" & Epoch == 30) |
        (Model == "CNN-GRU" & Epoch == 30) |
        (Model == "LSTM-Emb" & Epoch == 30) |
        (Model == "GRU-Emb" & Epoch == 30)
    ),
  get_acc_loss_table("data/processed/oomycete_hist_val.csv") %>%
    dplyr::filter(
      (Model == "CNN-LSTM" & Epoch == 30) |
        (Model == "CNN-GRU" & Epoch == 30) |
        (Model == "LSTM-Emb" & Epoch == 30) |
        (Model == "GRU-Emb" & Epoch == 30)
    )
) %>%
  dplyr::select(-c(Epoch))


acc_loss_final %>% readr::write_tsv("tables/acc_loss_final.tab")