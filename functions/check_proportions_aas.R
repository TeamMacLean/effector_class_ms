library(dplyr)
library(tidyr)

count_types <- function(seq) {
  s <- strsplit(seq, split = "")[[1]]
  tibble::tibble(
  "hydrophobic" = sum(s %in% c("P", "A","W","F","L","I","M","V")) ,
  "polar" = sum(s %in% c("G", "S", "T", "Y", "C" )),
  "neutral" = sum(s %in% c("N", "Q")),
  "basic" = sum(s %in% c("K","R","H")),
  "acidic" = sum(s %in% c("D", "E")),
  "length" = length(s)
  )
}


get_counts <- function(df){
  
  df %>% 
    mutate(res = purrr::map(Sequence,count_types)) %>% 
    unnest(res)
  
}

get_global_counts <- function(df){
  
  
  counts <- get_counts(df)
  
  tibble::tibble(
    all_length =  sum(counts$length),
    all_hydrophobic = sum(counts$hydrophobic),
    all_polar = sum(counts$polar),
    all_neutral =sum(counts$neutral),
    all_basic = sum(counts$basic),
    all_acidic =  sum(counts$acidic)
  )
  
}

get_effector_counts <- function(df, keep_label = 1){
  
  counts <- df %>% filter(label == keep_label) %>% 
    get_counts()
  tibble::tibble(
    eff_length =  sum(counts$length),
    eff_hydrophobic = sum(counts$hydrophobic),
    eff_polar = sum(counts$polar),
    eff_neutral =sum(counts$neutral),
    eff_basic = sum(counts$basic),
    eff_acidic =  sum(counts$acidic)
  )
}


join_files <- function(...){
  files <- c(...)
  seqs <- lapply(files, readr::read_csv)
  names(seqs) <- files
  bind_rows(seqs)
}

do_hyper <- function(df) {

  all_seqs <- get_global_counts(df)
  eff_seqs <- get_effector_counts(df)
  
  counts <- bind_cols(all_seqs, eff_seqs) 
  counts %>% 
    mutate(prop_hydro = all_hydrophobic / all_length,
           prop_polar = all_polar / all_length,
           prop_neutral = all_neutral / all_length,
           prop_basic = all_basic / all_length,
           prop_acidic = all_acidic / all_length
           ) %>% 
    mutate(eff_prop_hydro = eff_hydrophobic / eff_length,
           eff_prop_polar = eff_polar / eff_length, 
           eff_prop_neutral = eff_neutral / eff_length,
           eff_prop_basic = eff_basic / eff_length,
           eff_prop_acidic = eff_acidic / eff_length,
           diff = all_length - all_hydrophobic) %>% 
    rowwise() %>% 
    mutate(p_hydro = if_else(eff_prop_hydro > prop_hydro, 
                             phyper(eff_hydrophobic, all_hydrophobic, (all_length - all_hydrophobic), eff_length, lower.tail = FALSE ),
                             phyper(eff_hydrophobic, all_hydrophobic, diff, eff_length, lower.tail = TRUE )
                             ),
           p_polar = if_else(eff_prop_polar > prop_polar, 
                             phyper(eff_polar, all_polar, (all_length - all_polar), eff_length, lower.tail = FALSE ),
                             phyper(eff_polar, all_polar, (all_length - all_polar), eff_length, lower.tail = TRUE )
                             ),
           p_neutral = if_else(eff_prop_neutral > prop_neutral, 
                             phyper(eff_neutral, all_neutral, (all_length - all_neutral), eff_length, lower.tail = FALSE ),
                             phyper(eff_neutral, all_neutral, (all_length - all_neutral), eff_length, lower.tail = TRUE )
                              ),
           p_basic = if_else(eff_prop_basic > prop_basic, 
                             phyper(eff_basic, all_basic, (all_length - all_basic), eff_length, lower.tail = FALSE ),
                             phyper(eff_basic, all_basic, (all_length - all_basic), eff_length, lower.tail = TRUE )
                              ),
           p_acidic = if_else(eff_prop_acidic > prop_acidic, 
                             phyper(eff_acidic, all_acidic, (all_length - all_acidic), eff_length, lower.tail = FALSE ),
                             phyper(eff_acidic, all_acidic, (all_length - all_acidic), eff_length, lower.tail = TRUE )
                            )
           
           )
  
  
    
  
  

  # tibble::tibble(
  #   types = c("Hydrophobic", "basic", "Neutral", "Basic", "Acidic"),
  #   p = c( 
  #     phyper(eff_seqs$total_hydrophobic, all_seqs$total_hydrophobic, (all_seqs$total_length - all_seqs$total_hydrophobic), eff_seqs$total_length ),
  #     phyper(eff_seqs$total_basic, all_seqs$total_basic, (all_seqs$total_length - all_seqs$total_basic), eff_seqs$total_length ),
  #     phyper(eff_seqs$total_neutral, all_seqs$total_neutral, (all_seqs$total_length - all_seqs$total_neutral), eff_seqs$total_length ),
  #     phyper(eff_seqs$total_basic, all_seqs$total_basic, (all_seqs$total_length - all_seqs$total_basic), eff_seqs$total_length ),
  #     phyper(eff_seqs$total_acidic, all_seqs$total_acidic, (all_seqs$total_length - all_seqs$total_acidic), eff_seqs$total_length )
  #   ),
  # prop_eff = c(
  #     (eff_seqs$total_hydrophobic / eff_seqs$total_length),
  #     (eff_seqs$total_basic / eff_seqs$total_length), 
  #     (eff_seqs$total_neutral / eff_seqs$total_length),
  #     (eff_seqs$total_basic / eff_seqs$total_length ),
  #     (eff_seqs$total_acidic / eff_seqs$total_length )
  #   ),
  # prop_all = c(
  #   (all_seqs$total_hydrophobic / all_seqs$total_length),
  #   (all_seqs$total_basic / all_seqs$total_length ),
  #   (all_seqs$total_neutral / all_seqs$total_length ),
  #   (all_seqs$total_basic / all_seqs$total_length ),
  #   (all_seqs$total_acidic / all_seqs$total_length) 
  #   )
  #   
  # )
  
}

truncate_seqs <- function(df, start=NA, stop=NA){
  df$Sequence <- substr(df$Sequence, start, stop)
  return(df)
}




