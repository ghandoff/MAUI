library(openxlsx)
library(doParallel)
library(foreach)
library(tidyverse)
library(readxl)

raw <- read_csv('data/test data.csv')

freq_table <- raw %>%
  group_by(response) %>%
  summarise(frequency = n())

mass_table <- freq_table %>%
  group_by(frequency) %>%
  summarise(count = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(mass = frequency*count) %>%
  mutate(cum_mass = cumsum(mass)) %>%
  mutate(MAUI =  (cum_mass - mass/2)/max(cum_mass))
