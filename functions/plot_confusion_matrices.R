plot_confusion_matrices <- function(data, true_label, model_list, nrow = 1, fill_upper_limit = NA) {
  conf_matrix_df <- data %>%
    select(-sequence) %>%
    # Add true labels
    dplyr::mutate(Reference = true_label$label) %>%
    # Transform into factors
    dplyr::mutate_all(function(x) factor(x, levels = c(1, 0))) %>%
    # Select chosen model only
    tidyr::pivot_longer(-Reference, names_to = "model", values_to = "Prediction") %>%
    # Filter models
    dplyr::filter(model %in% model_list) %>% 
    dplyr::mutate(model = factor(model, levels = model_list)) %>%
    # Calculate frequencies
    table() %>%
    as.data.frame()
  
  # Make plot
  gg_matrix <- conf_matrix_df %>%
    ggplot() +
    aes(x = Reference, y = Prediction) +
    geom_tile(aes(fill = Freq), colour = "white") +
    geom_text(aes(label = Freq), vjust = 0.5) +
    # scale_fill_gradient(low = "lightpink", high = "mediumpurple1") +
    scale_fill_viridis_c(begin = 0.1, end = 1, limits = c(0, fill_upper_limit)) +
    labs(x = "True value", y = "Prediction") +
    coord_fixed() +
    facet_wrap(~model, nrow = nrow) +
    theme_bw() +
    theme(legend.position = "none")
  
  return(gg_matrix)
}