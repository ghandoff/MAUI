#' takes entire P x (ID, Type, Response, Std, FlexCat) frame
#' returns P x item fram of flex scores
std_to_flextable <- function(resp, item_col, id_col, flex_col) {
  item <- enquo(item_col)
  id <- enquo(id_col)
  flex <- enquo(flex_col)
  tbl <- all_responses %>%
    select(one_of(c(!!item, !!id, !!flex))) %>%
    group_by_at(vars(one_of(c(!!id, !!item)))) %>%
    summarise(flexibility = n()) %>%
    spread(!!item, flexibility) %>%
    ungroup()
}

#' takes P x flex frame and bootstrap size
#' returns bootstrap size x (part ID, n) table
boot_nums <- function(data, size) {
  data %>%
  select(partID) %>%
  sample_n(size, replace = FALSE) %>%
  mutate(n = size)
}

#' takes frame, item id, and bootstrap size
#' returns a column of partIDs in the bootstrap
#' used if we need to ensure that the bootstrap sample has done the focal task
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
    summarise(count = n()) %>%
    ungroup()
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
    arrange(desc(count)) %>%
    ungroup()
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

