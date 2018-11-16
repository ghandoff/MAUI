#' takes df of responses item id
#' returns a df of response counts
density_helper <- function(resp, item) {
  resp %>%
    group_by(count) %>%
    summarise(frequency = n()) %>%
    ungroup()
} 