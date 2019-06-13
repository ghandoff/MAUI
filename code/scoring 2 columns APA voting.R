library(openxlsx)
library(doParallel)
library(foreach)
library(tidyverse)
library(readxl)

freq_table <- read_csv('data/APA_ranked/APA_2003_ranked.csv', col_names = FALSE)%>%
  transmute(response = paste(X1, X2, X3, X4, X5, sep = "."),
            frequency = X6)

#n <- length(unique(raw$participant)) #' calculates number of participants

UI_thresh <- .95 #' the 'old UI' threshhold, default at 95%
top_x <- 5 #' how many of the highest MAUI scores to use for the top X score

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
         UI = 1 - frequency/max(cum_mass),
         norm_rank = (rank(cum_mass) - .5)/nrow(.))

#' appends MAUI and UI to freq_table
freq_table <- freq_table %>%
  left_join(select(mass_table, -count, -mass, -cum_mass))

mass_table <- mass_table %>%
  mutate(mass_weight = mass/max(cum_mass)) %>%
  mutate(task = 'voting',
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

curve_formula <- formula(MAUI ~ ((d*(norm_rank)^g)/((d*(norm_rank)^g)+(1-norm_rank)^g)))


#' SAVE SMPL_RANKS HERE FOR GRAPHING
#' ggplot(data = smpl_ranks, aes(x = MAUI, y = mass)) + geom_col() + xlim(0,1)
#' ggplot(data = smpl_ranks, aes(x = 1-pct_giving, y = mass)) + geom_col() + geom_vline(aes(xintercept = .95)) + xlim(0,1)
#' 
#FIT GAMMA AND DELTA HERE
fit <- nls(curve_formula, mass_table, start = list(d = .5, g = .6))
delta_gamma <- ggplot(mass_table, aes(x = norm_rank, y = MAUI)) + 
  geom_point() + 
  geom_smooth(method="nls",
              formula=curve_formula, # this is an nls argument
              method.args = list(start=c(d = .5, g = .5)), # this too
              se=FALSE)

s <- filter(all_responses, partID == target_sample$partID[1] & TypeItem == 'U1')
new <- smpl_itemcount 

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

#' This graph is good to show how the violin graph is generated
hist_dens_normrank <- ggplot(p_response_scores, aes(x = norm_rank,y = ..ndensity..)) + 
  stat_density(position="identity", geom="line") +
  geom_histogram(binwidth = .005) +
  coord_flip(xlim=c(0,1)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

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