library(foreach)
library(tidyverse)

jots_tall <- read_csv('data/Profiles Tall99 responses.csv') %>%
  mutate(TypeItem = paste(Type, ItemOrd, sep = "-")) %>%
  filter(TypeItem == '2-1') %>%
  select('partID', 'RespOrd', 'Std') %>%
  rename(participant = partID,
         jot = RespOrd,
         standardized = Std)

write_csv(jots_tall, 'data/SOU/jots tall.csv')

jots_wide <- jots_tall %>%
  spread(jot, standardized, sep = "_")

write_csv(jots_wide, 'data/SOU/jots wide.csv')

freq_table <- jots_tall %>%
  group_by(standardized) %>%
  summarise(frequency = n()) %>%
  rename(response = standardized)

write_csv(freq_table, 'data/SOU/response frequency.csv')

n <- length(unique(jots_tall$participant))

mass_table <- freq_table %>%
  group_by(frequency) %>%
  summarise(count = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(mass = frequency*count) %>%
  mutate(cum_mass = cumsum(mass)) %>%
  mutate(MAUI = (cum_mass - mass/2)/max(cum_mass),
         UI = 1 - frequency/n,
         norm_rank = (rank(cum_mass) - .5)/nrow(.))

write_csv(mass_table, 'data/SOU/scores.csv')
