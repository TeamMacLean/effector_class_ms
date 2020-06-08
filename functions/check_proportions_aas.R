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
  
  list(
    total_length = sum(counts$length),
    total_hydrophobic = sum(counts$hydrophobic),
    total_polar = sum(counts$polar),
    total_neutral = sum(counts$neutral),
    total_basic = sum(counts$basic),
    total_acidic = sum(counts$acidic)
  )
  
}

get_effector_counts <- function(df, keep_label = 1){
  
  counts <- df %>% filter(label == keep_label) %>% 
    get_counts()
  list(
    total_length = sum(counts$length),
    total_hydrophobic = sum(counts$hydrophobic),
    total_polar = sum(counts$polar),
    total_neutral = sum(counts$neutral),
    total_basic = sum(counts$basic),
    total_acidic = sum(counts$acidic)
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
  tibble::tibble(
    Hydrophobic = phyper(eff_seqs$total_hydrophobic, all_seqs$total_hydrophobic, (all_seqs$total_length - all_seqs$total_hydrophobic), eff_seqs$total_length ),
    Polar = phyper(eff_seqs$total_polar, all_seqs$total_polar, (all_seqs$total_length - all_seqs$total_polar), eff_seqs$total_length ),
    Neutral = phyper(eff_seqs$total_neutral, all_seqs$total_neutral, (all_seqs$total_length - all_seqs$total_neutral), eff_seqs$total_length ),
    Basic = phyper(eff_seqs$total_basic, all_seqs$total_basic, (all_seqs$total_length - all_seqs$total_basic), eff_seqs$total_length ),
    Acidic = phyper(eff_seqs$total_acidic, all_seqs$total_acidic, (all_seqs$total_length - all_seqs$total_acidic), eff_seqs$total_length )
  )
}

truncate_seqs <- function(df, start=NA, stop=NA){
  df$Sequence <- substr(df$Sequence, start, stop)
  return(df)
}




