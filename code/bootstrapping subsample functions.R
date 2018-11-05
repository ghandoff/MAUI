#' takes entire P x (ID, Type, Response, Std, FlexCat) frame
#' returns P x item fram of fluency scores
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

#' takes df of responses and item id
#' returns a df of response counts
sort_count <- function(resp, item) {
  resp %>%
    filter(TypeItem == item) %>% #TypeItem is file-specific
    group_by(Std) %>% #Std is file-specific
    summarise(count = n()) %>%
    ungroup() %>%
    mutate(TypeItem = item)
} 

#' takes df of sample responses   
#' returns a df by rankwise scoring
ranks <- function(resp, size, item) {
  resp %>%
    filter(TypeItem == item) %>% #TypeItem is file-specific
    arrange(desc(count)) %>%
    mutate(cum = cumsum(count)) %>%
    group_by(count) %>%
    summarise(cumsum = max(cum),
              mass = n()) %>%
    mutate(mass = mass*count,
           MAUI = ((cumsum - mass) + (mass/2))/max(cumsum),
           pct_giving = count/size) %>%
    arrange(desc(count)) %>%
    ungroup() %>%
    mutate(TypeItem = item)
} 

append_scores <- function(resp, rnks, item) {
  ranks <- rnks %>%
    select(-cumsum, -mass)
  appended <- resp %>%
    filter(TypeItem == item) %>%
    left_join(ranks)
}

p_score <- function(resp, scrs, item){
  resp %>%
    filter(TypeItem == item) %>%
    left_join(scrs, by = 'Std') %>%
    mutate(UI95 = ifelse(pct_giving <= .05, 1, 0)) %>%
    group_by(partID) %>%
    summarise(sum_MAUI = sum(MAUI),
              avg_MAUI = mean(MAUI),
              sum_UI95 = sum(UI95),
              avg_UI95 = mean(UI95)) %>%
    mutate(TypeItem = item)
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
