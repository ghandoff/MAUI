#' takes frame, item id, and bootstrap size
#' returns a column of partIDs in the bootstrap
sample_ids <- function(data, item, size) {
  d <- data %>%
    select(one_of(c('partID', item))) %>%
    na.omit() %>%
    select(partID) %>%
    sample_n(size, replace = FALSE)
}

#' takes df, id list, and item id
#' returns a df of responses for that item & P list
sample_responses <- function(all_responses, ids, item) {
  d <- all_responses %>%
    filter(TypeItem == item) %>% #TypeItem is file-specific
    filter(partID %in% ids)
} 

#' takes df of responses
#' returns a df of response counts
sort_count <- function(resp) {
  d <- resp %>%
    group_by(Std) %>% #Std is file-specific
    summarise(count = n())
} 

#' takes df of sample responses   
#' returns a df by rankwise scoring
ranks <- function(resp, size) {
  rnk <- resp %>%
    arrange(desc(count)) %>%
    mutate(cum = cumsum(count)) %>%
    group_by(count) %>%
    summarise(cumsum = max(cum),
              mass = n()) %>%
    mutate(mass = mass*count,
           MAUI = ((cumsum - mass) + (mass/2))/max(cumsum),
           pct_giving = count/(size + 100)) %>%
    arrange(desc(count))
}    

#' frame for Gini & other calculations
#' returns a df for calculating Gini & gamma/delta
item_calcs <- function(rnk) {
  item_calcs <- rnk %>%
    add_row(MAUI = 1, .after = nrow(rnk)) %>%
    add_row(MAUI = 0, .before = 1) %>%
    rowid_to_column("rank") %>%
    mutate(rank = rank - 1) %>%
    mutate(norm_rank = rank/max(rank))
}