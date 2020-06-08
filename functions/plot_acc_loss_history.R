plot_acc_loss_history <- function(train_data, val_data, plot_tag = NULL) {
  
  # Melt data
  train_data_melt <- train_data %>%
    tidyr::pivot_longer(
      cols = -c(epochs, model),
      names_to = "metric",
      values_to = "training"
    )
  
  val_data_melt <- val_data %>%
    tidyr::pivot_longer(
      cols = -c(epochs, model),
      names_to = "metric",
      values_to = "validation"
    )

  # Left join into 1 dataframe
  train_val_data <- train_data_melt %>%
    dplyr::left_join(
      val_data_melt,
      by = c("epochs", "model", "metric")
    ) %>%
    tidyr::pivot_longer(
      cols = -c(epochs, model, metric),
      names_to = "dataset",
      values_to = "values"
    )
  
  # Plot the accuracy data
  gg_accuracy <- train_val_data %>% 
    dplyr::mutate(
      metric = factor(metric, levels = c("acc", "loss"), labels = c("Accuracy", "Loss")),
      model = as.factor(model)
  ) %>% 
  ggplot() +
    aes(
      x = epochs,
      y = values,
      color = dataset,
      group = dataset,
      linetype = dataset
    ) +
    geom_point(size = 1) +
    geom_line() +
    scale_color_viridis_d(begin = 0.65, end = 0.1) +
    scale_linetype_manual(values = c("validation" = "dashed", "training" = "solid")) +
    labs(x = "Epochs", y = "Values", tag = plot_tag) +
    facet_grid(metric ~ model, scales = "free")
  
  return(gg_accuracy)
}
