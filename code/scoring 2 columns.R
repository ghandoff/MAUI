library(openxlsx)
library(doParallel)
library(foreach)
library(tidyverse)
library(readxl)

#'This codelet operates on a single item

#' input file
#' first column must be named 'participant' and holds participant IDs
#' secon column must be named 'response' and holds the standardized responses



# raw <- read_csv('data/test data.csv')
# raw$response <- str_replace_all(raw$response, "[^[:alnum:]]", " ") %>% #gets rid of non alphanumerics
#   tolower() #' turns everything to lowercase

# use next line for paperclip data
#raw <- read_csv('data/Garrett Dissertation Data Answers Only.csv') %>%
  filter(TypeItem == 'U1') %>%
  select(one_of(c('partID', 'Std')))

# Use next line for gear data
raw <- read_csv('data/Garrett GearToy Data_TwoColumn.csv')
names(raw) <- c('participant', 'response')

n <- length(unique(raw$participant)) #' calculates number of participants

UI_thresh <- .95 #' the 'old UI' threshhold, default at 95%
top_x <- 5 #' how many of the highest MAUI scores to use for the top X score

#' freq_table holds standardized responses and the frequency they occur in the item (i.e. the size of the fruit)
freq_table <- raw %>%
  group_by(response) %>%
  summarise(frequency = n())

#' mass_table has a bunch of intermediate calculations, and also the scores
#' 'frequency' is how often that response occurs in the item (fruit size)
#' 'count' is how many responses are of that frequency (i.e. how many fruits are on that level)
#' 'mass' is frequency*count (i.e. the total mass of fruit on that level)
#' 'cum_mass' is the mass of responses on that level and below
#' MAUI is calculated from the cum_mass of the level below + half of the current level mass
#' UI is calculated in the standard way from n and UI_thresh
mass_table <- freq_table %>%
  group_by(frequency) %>%
  summarise(count = n()) %>%
  arrange(desc(frequency)) %>%
  mutate(mass = frequency*count) %>%
  mutate(cum_mass = cumsum(mass)) %>%
  mutate(MAUI = (cum_mass - mass/2)/max(cum_mass),
         UI = 1 - frequency/n,
         norm_rank = (rank(cum_mass) - .5)/nrow(.))

#' appends MAUI and UI to freq_table
freq_table <- freq_table %>%
  left_join(select(mass_table, -count, -mass, -cum_mass))

#' p_response_scores is the original data with MAUI and UI scores appended to each response
p_response_scores <- raw %>%
  left_join(select(freq_table, -frequency)) %>%
  ungroup()

#' calculates the top X score to append below
top_x_scores <- p_response_scores %>%
  select(-UI) %>%
  arrange(participant, desc(MAUI)) %>%
  group_by(participant) %>%
  slice(seq_len(top_x)) %>%
  summarise(top_x_MAUI = sum(MAUI))

#' p_scores is a summary for each participant
#' current reports fluency, the sum of all MAUI scores and the sum of all UI scores
#' But very adaptable for new scores!
p_scores <- p_response_scores %>%
  mutate(UI = if_else(UI >= UI_thresh, 1, 0)) %>%
  group_by(participant) %>%
  summarise(fluency = n(),
            MAUI_sum = sum(MAUI),
            UI_sum = sum(UI)) %>%
  left_join(top_x_scores)

mass_table <- mass_table %>%
  mutate(mass_weight = mass/max(cum_mass)) %>%
  mutate(task = 'gears',
         freq_rank_color = dense_rank(frequency)%%2)

mass_graph <- ggplot(mass_table) +
  geom_line(aes(norm_rank, MAUI), color = 'blue') +
  geom_line(aes(norm_rank, UI), color = 'red')

response_tree <- ggplot(mass_table, aes(x = factor(task), y = norm_rank, weight = mass_weight)) +
  geom_violin() +
  geom_boxplot(width = 0.1)

MAUI_tree <- ggplot(mass_table, aes(x = factor(task), y = MAUI, weight = mass_weight)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  coord_cartesian(ylim=c(0,1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

UI_tree <- ggplot(mass_table, aes(x = factor(task), y = UI, weight = mass_weight)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  coord_cartesian(ylim=c(0,1)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#####
# code for 'mega-viz'
library(ggExtra)

hist_dens <- ggplot(p_response_scores, aes(x = MAUI, y=..ndensity..)) + 
  stat_density(position="identity", geom="line") +
  geom_histogram(binwidth = .005) +
  coord_cartesian(xlim=c(0,1)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

bar <- ggplot(mass_table) +
  geom_bar(aes(task, mass_weight, fill = freq_rank_color), stat = 'identity', width = 1) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position='none')

#'for prettier graphs
# library(Cairo)
# CairoPNG(filename="graphs/gears_hist_dens.png", 
#     units="in", 
#     width=8, 
#     height=5.6, 
#     pointsize=12, 
#     res=96)
# print(hist_dens)
# dev.off()
# 
# CairoPNG(filename="graphs/gears_bar.png", 
#          units="in", 
#          width=.5, 
#          height=8, 
#          pointsize=12, 
#          res=96)
# print(bar)
# dev.off()
# 
# CairoPNG(filename='graphs/gears_MAUI.png', 
#          units="in", 
#          width=4, 
#          height=5.6, 
#          pointsize=12, 
#          res=96)
# print(MAUI_tree)
# dev.off()
# 
# CairoPNG(filename='graphs/gears_UI.png', 
#          units="in", 
#          width=4, 
#          height=5.6, 
#          pointsize=12, 
#          res=96)
# print(UI_tree)
# dev.off()
# 
# CairoPNG(filename='graphs/gears_responses.png', 
#          units="in", 
#          width=4, 
#          height=5.6, 
#          pointsize=12, 
#          res=96)
# print(response_tree)
# dev.off()


#######
#Extra graphing stuff we might want later

# for_graph <- p_response_scores
# for_graph$task <- 'gears'
# for_graph <- left_join(for_graph, select(mass_table, one_of(c('MAUI', 'freq_rank_color'))))
# for_graph$freq_rank_color <- for_graph$freq_rank_color +1
# 
# hist_dens <- ggplot(for_graph, aes(x = MAUI)) + 
#   stat_density(position="identity", geom="line", aes(y=..ndensity..)) +
#   geom_histogram(binwidth = .005, aes(y=..ndensity..)) +
#   geom_bar()
# 
# bar <- ggplot(for_graph, aes(x = task, fill = factor(MAUI))) +
#   geom_bar(position = 'fill') +
#   scale_fill_manual(values = for_graph$freq_rank_color)
# 
# 
# mega <- ggplot(for_graph) +
#   geom_histogram(binwidth = .005, aes(x=MAUI, y=..density..)) +
#   geom_density(aes(x=MAUI, y=.01*..count..))
# 
# mega <- ggplot(for_graph, aes(MAUI)) +
#   geom_histogram(binwidth = .005, aes(y=..count../sum(..count..))) +
#   geom_density(aes(x = MAUI, stat(scaled)))
# 
# marg_mega <- ggMarginal(mega, type = 'density')
# 
# mega <- ggplot(for_graph, aes(MAUI)) +
#   geom_dotplot(binwidth = .01) +
#   geom_freqpoly()
# 
# 
# 
# bar_dens <- ggMarginal(bar, type = 'density')
# 
# 
# 

########
# code for the split violin viz
# ABSOLUTELY NOT READY FOR PRIMETIME

# GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, draw_group = function(self, data, ..., draw_quantiles = NULL){
#   data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
#   grp <- data[1,'group']
#   newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
#   newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
#   newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
#   if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
#     stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 
#                                               1))
#     quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
#     aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
#     aesthetics$alpha <- rep(1, nrow(quantiles))
#     both <- cbind(quantiles, aesthetics)
#     quantile_grob <- GeomPath$draw_panel(both, ...)
#     ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
#   }
#   else {
#     ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
#   }
# })
# 
# geom_split_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
#   layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
# }

#######

# split_mass_table <- mass_table %>%
#   select(-frequency, -count, -mass, -cum_mass) %>%
#   gather("measure", "score", -norm_rank, -mass_weight, -task)
# 
# split_tree <- ggplot(split_mass_table, aes(x = score, y = norm_rank, fill = measure, weight = mass_weight)) +
#   geom_split_violin(trim = TRUE) +
#   geom_boxplot(width = 0.25, notch = FALSE, notchwidth = .4, outlier.shape = NA, coef=0) +
#   labs(x=NULL,y="GM Attitude Score") +
#   theme_classic() +
#   theme(text = element_text(size = 20)) +
#   scale_x_discrete(labels=c("0" = "Control\nCondition", "1" = "GM\nCondition")) +
#   scale_fill_manual(values=c("#E69F00", "#999999"),
#                     name="Survey\nPart",
#                     breaks=c("1", "2"),
#                     labels=c("Time 1", "Time 5"))