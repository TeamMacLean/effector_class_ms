# Function to draw the confusion matrix

plot_confusion_mat <- function(data_frame, model_list, fill_upper_limit = NA){
  plot_conf <- data_frame %>% 
    ggplot2::ggplot() +
    ggplot2::aes(x = true_label, y = Prediction) +
    ggplot2::geom_tile(ggplot2::aes(fill = Freq), colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = Freq), vjust = 0.5) +
    ggplot2::scale_fill_viridis_c(begin = 0.25, end = 1, limits = c(0, fill_upper_limit)) +
    ggplot2::labs(x = "True value", y = "Prediction") +
    ggplot2::coord_fixed() +
    ggplot2::facet_wrap(~factor(model, levels = model_list), ncol = length(model_list)) +
    ggplot2::theme_bw()
  
  return(plot_conf)
}
