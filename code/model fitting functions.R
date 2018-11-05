#' takes df of responses item id
#' returns a df of response counts
sort_count <- function(resp, item) {
  resp %>%
    filter(TypeItem == item) %>% #TypeItem is file-specific
    group_by(count) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    mutate(TypeItem = item)
} 