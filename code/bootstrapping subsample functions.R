#' takes entire P x (ID, Type, Response, Std, FlexCat) frame
#' returns P x item frame of fluency scores
std_to_fluency_table <- function(resp, item_col, id_col, resp_col) {
  item <- enquo(item_col)
  id <- enquo(id_col)
  responses <- enquo(resp_col)
  tbl <- all_responses %>%
    select(one_of(c(!!item, !!id, !!responses))) %>%
    group_by_at(vars(one_of(c(!!id, !!item)))) %>%
    summarise(fluency = n()) %>%
    spread(!!item, fluency) %>%
    ungroup()
}

#' takes P x flex frame and bootstrap size
#' returns bootstrap size x (part ID, n) table
boot_nums <- function(data, size) {
  data %>%
    select(partID) %>%
    sample_n(size, replace = FALSE) %>%
    mutate(n = size + 100)
}

#' takes frame, item id, and bootstrap size
#' returns a column of partIDs in the bootstrap
#' used if we need to ensure that the bootstrap sample has done the focal task
sample_ids <- function(data, item, size) {
  data %>%
    select(one_of(c('partID', item))) %>%
    na.omit() %>%
    select(partID) %>%
    sample_n(size, replace = FALSE)
}

#' takes df, id list, and item id
#' returns a df of responses for that item & P list
sample_responses <- function(all_responses, ids, item) {
  all_responses %>%
    filter(TypeItem == item) %>% #TypeItem is file-specific
    filter(partID %in% ids)
} 

#' takes df of responses item id
#' returns a df of response counts
sort_freq <- function(resp, item) {
  resp %>%
    filter(TypeItem == item) %>% #TypeItem is file-specific
    group_by(Std) %>% #Std is file-specific
    summarise(frequency = n()) %>%
    ungroup() %>%
    mutate(TypeItem = item)
} 

#' takes df of sample responses   
#' returns a df by rankwise scoring
ranks <- function(resp, size, item) {
  resp %>%
    filter(TypeItem == item) %>% #TypeItem is file-specific
    arrange(desc(frequency)) %>%
    #mutate(cum = cumsum(frequency)) %>%
    group_by(frequency) %>%
    summarise(#cumsum = max(cum),
              count = n()) %>%
    mutate(mass = count*frequency) %>%
    mutate(cum_mass = cumsum(mass),
           MAUI = (cum_mass - mass/2)/max(cum_mass),
           UI = 1 - frequency/n,
           norm_rank = (rank(cum_mass) - .5)/nrow(resp)) %>%
    arrange(desc(frequency)) %>%
    ungroup() %>%
    mutate(TypeItem = item)
} 

append_scores <- function(resp, rnks, item) {
  ranks <- rnks %>%
    filter(TypeItem == item) %>%
    select(-cumsum, -mass)
  appended <- resp %>%
    filter(TypeItem == item) %>%
    left_join(ranks)
}

p_score <- function(resp, scrs, item){
  df <- resp %>%
    filter(TypeItem == item) %>%
    left_join(filter(scrs, TypeItem == item), by = 'Std')
  totals <- df %>%
    mutate(UI95 = ifelse(pct_giving <= .05, 1, 0)) %>%
    group_by(partID) %>%
    summarise(sum_MAUI = sum(MAUI),
              avg_MAUI = mean(MAUI),
              sum_UI95 = sum(UI95),
              avg_UI95 = mean(UI95)) %>%
    mutate(TypeItem = item)
  top_MAUI <- df %>%
    group_by(partID) %>%
    arrange(desc(MAUI), .by_group=TRUE) %>%
    slice(seq_len(5)) %>%
    summarise(top5_MAUI = sum(MAUI))
  top_pct <- df %>%
    group_by(partID) %>%
    arrange(pct_giving, .by_group=TRUE) %>%
    slice(seq_len(5)) %>%
    mutate(pct_UI = 1-pct_giving) %>%
    summarise(top5_pct = sum(pct_UI))
  totals %>%
    left_join(top_MAUI, by='partID') %>%
    left_join(top_pct, by='partID')
}

#' frame for Gini & other calculations
#' returns a df for calculating Gini & gamma/delta
item_calcs <- function(rnk) {
  rnk %>%
    add_row(MAUI = 1, .after = nrow(rnk)) %>%
    add_row(MAUI = 0, .before = 1) %>%
    rowid_to_column("rank") %>%
    mutate(rank = rank - 1) %>%
    mutate(norm_rank = rank/max(rank))
}


##### deprecated, delete eventually

#' takes entire P x (ID, Type, Response, Std, FlexCat) frame
#' returns P x item fram of flex scores
#' 
# std_to_flextable <- function(resp, item_col, id_col, flex_col) {
#   item <- enquo(item_col)
#   id <- enquo(id_col)
#   flex <- enquo(flex_col)
#   tbl <- all_responses %>%
#     select(one_of(c(!!item, !!id, !!flex))) %>%
#     group_by_at(vars(one_of(c(!!id, !!item)))) %>%
#     summarise(flexibility = n()) %>%
#     spread(!!item, flexibility) %>%
#     ungroup()
# }
