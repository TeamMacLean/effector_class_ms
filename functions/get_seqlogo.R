get_seqlogo <- function(data, method_plot, start_pos, end_pos, break_size = 1) {
  
  data <- data %>% 
    dplyr::mutate(sequence = substr(Sequence, start_pos, end_pos)) 
  
  # Define the length of the sequence string selected
  len_seq_str = end_pos - start_pos + 1
  
  # Define additional column to get the value of the length of sequence 
  data_with_length <- data %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(len = nchar(sequence))
  
  # Take all data with the same length
  data_with_length <- data_with_length %>% 
    dplyr::filter(!len < len_seq_str)
  
  gg_seqlogo <- ggseqlogo::ggseqlogo(data_with_length$sequence, method = method_plot, seq_type = "aa") +
    ggplot2::scale_x_continuous(breaks = seq(1, len_seq_str, break_size), labels = seq(start_pos, end_pos, break_size)) +
    ggplot2::labs(x = "Position")
  
  gg_seqlogo <- gg_seqlogo + 
    ggplot2::theme_bw()+
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "#FFFFFF"),
      panel.grid.minor = ggplot2::element_line(colour = "#FFFFFF")
    )
  
  return(gg_seqlogo)
}
