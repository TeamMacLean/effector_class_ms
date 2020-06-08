# Load raw data ----

bacteria_cnn_lstm_train <- data.table::fread("data/raw/bacteria_hist_cnn_lstm_train.csv")
bacteria_cnn_lstm_val <- data.table::fread("data/raw/bacteria_hist_cnn_lstm_val.csv")
bacteria_cnn_gru_train <- data.table::fread("data/raw/bacteria_hist_cnn_gru_train.csv")
bacteria_cnn_gru_val <- data.table::fread("data/raw/bacteria_hist_cnn_gru_val.csv")
bacteria_lstm_emb_train <- data.table::fread("data/raw/bacteria_hist_lstm_emb_train.csv")
bacteria_lstm_emb_val <- data.table::fread("data/raw/bacteria_hist_lstm_emb_val.csv")
bacteria_gru_emb_train <- data.table::fread("data/raw/bacteria_hist_gru_emb_train.csv")
bacteria_gru_emb_val <- data.table::fread("data/raw/bacteria_hist_gru_emb_val.csv")

fungi_cnn_lstm_train <- data.table::fread("data/raw/fungi_hist_cnn_lstm_train.csv")
fungi_cnn_lstm_val <- data.table::fread("data/raw/fungi_hist_cnn_lstm_val.csv")
fungi_cnn_gru_train <- data.table::fread("data/raw/fungi_hist_cnn_gru_train.csv")
fungi_cnn_gru_val <- data.table::fread("data/raw/fungi_hist_cnn_gru_val.csv")
fungi_lstm_emb_train <- data.table::fread("data/raw/fungi_hist_lstm_emb_train.csv")
fungi_lstm_emb_val <- data.table::fread("data/raw/fungi_hist_lstm_emb_val.csv")
fungi_gru_emb_train <- data.table::fread("data/raw/fungi_hist_gru_emb_train.csv")
fungi_gru_emb_val <- data.table::fread("data/raw/fungi_hist_gru_emb_val.csv")

oomycete_cnn_lstm_train <- data.table::fread("data/raw/oomycete_hist_cnn_lstm_train.csv")
oomycete_cnn_lstm_val <- data.table::fread("data/raw/oomycete_hist_cnn_lstm_val.csv")
oomycete_cnn_gru_train <- data.table::fread("data/raw/oomycete_hist_cnn_gru_train.csv")
oomycete_cnn_gru_val <- data.table::fread("data/raw/oomycete_hist_cnn_gru_val.csv")
oomycete_lstm_emb_train <- data.table::fread("data/raw/oomycete_hist_lstm_emb_train.csv")
oomycete_lstm_emb_val <- data.table::fread("data/raw/oomycete_hist_lstm_emb_val.csv")
oomycete_gru_emb_train <- data.table::fread("data/raw/oomycete_hist_gru_emb_train.csv")
oomycete_gru_emb_val <- data.table::fread("data/raw/oomycete_hist_gru_emb_val.csv")

# Bind results together ----

bacteria_train_data <- dplyr::bind_rows(
  bacteria_cnn_lstm_train %>% mutate(model = "CNN-LSTM"), 
  bacteria_cnn_gru_train %>% mutate(model = "CNN-GRU"), 
  bacteria_lstm_emb_train %>% mutate(model = "LSTM-Emb"), 
  bacteria_gru_emb_train %>% mutate(model = "GRU-Emb")
) %>% 
  dplyr::mutate(epochs = V1 + 1) %>%
  dplyr::select(model, epochs, acc, loss)

bacteria_val_data <- dplyr::bind_rows(
  bacteria_cnn_lstm_val %>% mutate(model = "CNN-LSTM"), 
  bacteria_cnn_gru_val %>% mutate(model = "CNN-GRU"), 
  bacteria_lstm_emb_val %>% mutate(model = "LSTM-Emb"), 
  bacteria_gru_emb_val %>% mutate(model = "GRU-Emb")
) %>% 
  dplyr::mutate(epochs = V1 + 1) %>%
  dplyr::select(model, epochs, acc, loss)

fungi_train_data <- dplyr::bind_rows(
  fungi_cnn_lstm_train %>% mutate(model = "CNN-LSTM"),
  fungi_cnn_gru_train %>% mutate(model = "CNN-GRU"),
  fungi_lstm_emb_train %>% mutate(model = "LSTM-Emb"),
  fungi_gru_emb_train %>% mutate(model = "GRU-Emb")
) %>% 
  dplyr::mutate(epochs = V1 + 1) %>%
  dplyr::select(model, epochs, acc, loss)

fungi_val_data <- dplyr::bind_rows(
  fungi_cnn_lstm_val %>% mutate(model = "CNN-LSTM"),
  fungi_cnn_gru_val %>% mutate(model = "CNN-GRU"),
  fungi_lstm_emb_val %>% mutate(model = "LSTM-Emb"),
  fungi_gru_emb_val %>% mutate(model = "GRU-Emb")
) %>% 
  dplyr::mutate(epochs = V1 + 1) %>%
  dplyr::select(model, epochs, acc, loss)

oomycete_train_data <- dplyr::bind_rows(
  oomycete_cnn_lstm_train %>% mutate(model = "CNN-LSTM"),
  oomycete_cnn_gru_train %>% mutate(model = "CNN-GRU"),
  oomycete_lstm_emb_train %>% mutate(model = "LSTM-Emb"), 
  oomycete_gru_emb_train %>% mutate(model = "GRU-Emb")
) %>% 
  dplyr::mutate(epochs = V1 + 1) %>%
  dplyr::select(model, epochs, acc, loss)

oomycete_val_data <- dplyr::bind_rows(
  oomycete_cnn_lstm_val %>% mutate(model = "CNN-LSTM"),
  oomycete_cnn_gru_val %>% mutate(model = "CNN-GRU"),
  oomycete_lstm_emb_val %>% mutate(model = "LSTM-Emb"),
  oomycete_gru_emb_val %>% mutate(model = "GRU-Emb")
) %>% 
  dplyr::mutate(epochs = V1 + 1) %>%
  dplyr::select(model, epochs, acc, loss)

# Save data ----

bacteria_train_data %>% data.table::fwrite("data/processed/bacteria_hist_train.csv")
bacteria_val_data %>% data.table::fwrite("data/processed/bacteria_hist_val.csv")
fungi_train_data %>% data.table::fwrite("data/processed/fungi_hist_train.csv")
fungi_val_data %>% data.table::fwrite("data/processed/fungi_hist_val.csv")
oomycete_train_data %>% data.table::fwrite("data/processed/oomycete_hist_train.csv")
oomycete_val_data %>% data.table::fwrite("data/processed/oomycete_hist_val.csv")