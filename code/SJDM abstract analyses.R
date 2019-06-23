library(foreach)
library(tidyverse)

tipi <- read_csv('data/Profiles Tall99 TIPI.csv') %>%
  mutate(extroversion = (TIPI1 + TIPI6)/2,
         agreeableness = (TIPI2 + TIPI7)/2,
         conscientiousness = (TIPI3 + TIPI8)/2,
         neuroticism = (TIPI4 + TIPI9)/2,
         openness = (TIPI5 + TIPI10)/2)

responses <- read_csv('data/Profiles Tall99 responses.csv') %>%
  mutate(item = paste(Type, ItemOrd, sep = "-")) %>%
  select('partID', 'item', 'Std', 'Flex56')
names(responses) <- c('partID', 'TypeItem', 'Std', 'FlexCat')
items <- c('1-1', '1-2', '1-3', '2-1', '2-2', '2-3', '3-1', '3-2', '3-3')

responses_times <- read_csv('data/Profiles Tall99 responses.csv') %>%
  mutate(TypeItem = paste(Type, ItemOrd, sep = "-")) %>%
  select('partID', 'TypeItem', 'Std', 'Flex56', 'RespOrd', 'RespSec', 'ShiftCount')

comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
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
rank_score <- function(resp, item) {
 sz <- filter(responses, TypeItem == item)
 size <- length(unique(sz$partID))
  resp %>%
    filter(TypeItem == item) %>% #TypeItem is file-specific
    group_by(frequency) %>%
    summarise(count = n()) %>%
    arrange(desc(frequency)) %>%
    mutate(mass = count*frequency) %>%
    mutate(size = size,
           cum_mass = cumsum(mass),
           MAUI = (cum_mass - mass/2)/max(cum_mass),
           UI = 1 - frequency/size,
           norm_rank = (rank(cum_mass) - .5)/nrow(.),
           weight = mass/max(cum_mass)) %>%
    arrange(desc(frequency)) %>%
    ungroup() %>%
    mutate(TypeItem = item)
} 

p_score <- function(resp, scrs, item){
  df <- resp %>%
    filter(TypeItem == item) %>%
    left_join(filter(scrs, TypeItem == item), by = 'Std')
  totals <- df %>%
    mutate(UI95 = ifelse(UI >= .95, 1, 0)) %>%
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
  # top_pct <- df %>%
  #   group_by(partID) %>%
  #   arrange(pct_giving, .by_group=TRUE) %>%
  #   slice(seq_len(5)) %>%
  #   mutate(pct_UI = 1-pct_giving) %>%
  #   summarise(top5_pct = sum(pct_UI))
  totals %>%
    left_join(top_MAUI, by='partID')
}

response_scores <- foreach(j = items, .combine='rbind') %do%
  sort_freq(responses, j) #' returns response frequency for standardized responses in a single df

ranks <- foreach(k = items, .combine='rbind') %do%
  rank_score(response_scores, k) #outputs MAUI rank table of bootstrap sample responses


append_scores <- function(resp, rnks, item) {
  ranks <- rnks %>%
    filter(TypeItem == item) %>%
    select(-cum_mass, -mass)
  appended <- resp %>%
    filter(TypeItem == item) %>%
    left_join(ranks)
}

response_scores <- foreach(l = items, .combine='rbind') %do% 
  append_scores(response_scores, ranks, l) #' appends scores

participant_scores <- foreach(m = items, .combine='rbind') %do% 
  p_score(responses, response_scores, m)  #' calculates participant scores

resp_time_scores <- responses_times %>%
  left_join(response_scores, by = c('TypeItem', 'Std')) %>%
  mutate(delta_MAUI = if_else(RespOrd == 1, NA_real_, MAUI-lag(MAUI)))

participant_deltas <- resp_time_scores %>%
  group_by(partID) %>%
  summarise(total_shifts = sum(ShiftCount),
            mean_delta = mean(delta_MAUI, na.rm = TRUE),
            var_delta = var(delta_MAUI, na.rm= TRUE),
            fluency = n())


#'participant comparisons across sample sizes
sample_variation <- participant_scores %>%
  group_by(partID) %>%
  summarise(mean_MAUI = mean(sum_MAUI),
            sd_MAUI = var(sum_MAUI, na.rm = TRUE),
            mean_UI95 = mean(sum_UI95),
            sd_UI95 = var(sum_UI95, na.rm = TRUE),
            mean_top5 = mean(top5_MAUI),
            sd_top5 = var(top5_MAUI, na.rm = TRUE),
            mean_avg_MAUI = mean(avg_MAUI),
            sd_avg_MAUI = var(avg_MAUI, na.rm = TRUE),
            mean_avg_UI95 = mean(avg_UI95),
            sd_avg_UI95 = var(avg_UI95, na.rm = TRUE),
            sd_diff = sd_UI95 - sd_MAUI)

scores_w_tipi <- sample_variation %>%
  left_join(select(tipi, partID, extroversion, agreeableness, conscientiousness, neuroticism, openness))

t.test(sample_variation$sd_MAUI, sample_variation$sd_UI95, paired = TRUE)
t.test(sample_variation$sd_MAUI, sample_variation$sd_top5, paired = TRUE)
t.test(sample_variation$sd_avg_MAUI, sample_variation$sd_avg_UI95, paired = TRUE)

library(Hmisc)

responses_variation <- response_scores %>%
  group_by(TypeItem) %>%
  summarise(mean_MAUI = mean(sum_MAUI),
            sd_MAUI = var(sum_MAUI, na.rm = TRUE),
            mean_UI95 = mean(sum_UI95),
            sd_UI95 = var(sum_UI95, na.rm = TRUE),
            mean_top5 = mean(top5_MAUI),
            sd_top5 = var(top5_MAUI, na.rm = TRUE),
            mean_avg_MAUI = mean(avg_MAUI),
            sd_avg_MAUI = var(avg_MAUI, na.rm = TRUE),
            mean_avg_UI95 = mean(avg_UI95),
            sd_avg_UI95 = var(avg_UI95, na.rm = TRUE),
            sd_diff = sd_UI95 - sd_MAUI)

lm_MAUI <- lm(mean_MAUI ~ extroversion + agreeableness + conscientiousness + neuroticism + openness, scores_w_tipi)
lm_UI95 <- lm(mean_UI95 ~ extroversion + agreeableness + conscientiousness + neuroticism + openness, scores_w_tipi)
lm_top5 <- lm(mean_top5 ~ extroversion + agreeableness + conscientiousness + neuroticism + openness, scores_w_tipi)
lm_avg_MAUI <- lm(mean_avg_MAUI ~ extroversion + agreeableness + conscientiousness + neuroticism + openness, scores_w_tipi)
lm_avg_UI95 <- lm(mean_avg_UI95 ~ extroversion + agreeableness + conscientiousness + neuroticism + openness, scores_w_tipi)
lm_sd_MAUI <- lm(sd_MAUI ~ extroversion + agreeableness + conscientiousness + neuroticism + openness, scores_w_tipi)
lm_sd_UI95 <- lm(sd_UI95 ~ extroversion + agreeableness + conscientiousness + neuroticism + openness, scores_w_tipi)

library(multilevel)

null <- lme(MAUI ~ 1, random=~1|partID, data = resp_time_scores, control = list(opt = 'optim'))
null_gls <- gls(MAUI ~ 1, data = resp_time_scores, control = list(opt = 'optim'))
anova(null_gls, null)
model1 <- lme(MAUI ~ ShiftCount, random =~ 1|partID, data = resp_time_scores, control = list(opt = 'optim'), na.action = na.omit)

null <- lme(UI ~ 1, random=~1|partID, data = resp_time_scores, control = list(opt = 'optim'))
null_gls <- gls(UI ~ 1, data = resp_time_scores, control = list(opt = 'optim'))
anova(null_gls, null)
model1 <- lme(UI ~ ShiftCount, random =~ 1|partID, data = resp_time_scores, control = list(opt = 'optim'), na.action = na.omit)

#'participant comparisons across sample sizes
sample_var_time <- resp_time_scores %>%
  group_by(partID) %>%
  summarise(mean_MAUI = mean(sum_MAUI),
            sd_MAUI = var(sum_MAUI, na.rm = TRUE),
            mean_UI95 = mean(sum_UI95),
            sd_UI95 = var(sum_UI95, na.rm = TRUE),
            mean_top5 = mean(top5_MAUI),
            sd_top5 = var(top5_MAUI, na.rm = TRUE),
            mean_avg_MAUI = mean(avg_MAUI),
            sd_avg_MAUI = var(avg_MAUI, na.rm = TRUE),
            mean_avg_UI95 = mean(avg_UI95),
            sd_avg_UI95 = var(avg_UI95, na.rm = TRUE),
            sd_diff = sd_UI95 - sd_MAUI)