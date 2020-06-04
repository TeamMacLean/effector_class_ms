get_lower_corr_mat <- function(data) {
  # Create a correlation matrix.
  corr_mat <- data %>%
    dplyr::select(lstm_emb, gru_emb, cnn_gru, cnn_lstm) %>%
    stats::cor()
  
  var_levels <- colnames(corr_mat)
  
  corr_mat[upper.tri(corr_mat)] <- NA
  
  corr_mat <- corr_mat %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "x_var")
  
  corr_mat_long <- corr_mat %>%
    tidyr::pivot_longer(
      cols = -x_var,
      names_to = "y_var",
      values_drop_na = TRUE
    ) %>%
    mutate(
      x_var = factor(x_var, levels = rev(var_levels)),
      y_var = factor(y_var, levels = var_levels)
    )
  
  gg_corr <- ggplot(
    corr_mat_long,
    aes(x = x_var, y = y_var, fill = value)
  ) +
    geom_tile() +
    # viridis::scale_fill_viridis(limits = c(0, 1)) +
    scale_fill_viridis_c(limits = c(0, 1)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    geom_text(aes(label = round(value, 2)),
              vjust = 0.5,
              size = 3
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.position = "none"
    ) +
    coord_fixed()
  
  return(gg_corr)
}