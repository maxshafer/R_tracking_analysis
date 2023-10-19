library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(gridExtra)

ordered_bouts <- rest_data$bout_data[order(rest_data$bout_data$length), ]

short_bouts <- head(ordered_bouts, 0.25*length(rest_data$bout_data$length))
the_rest <- tail(ordered_bouts, 0.75*length(rest_data$bout_data$length))

long_bouts <- filter(the_rest, length < 1000)
bouts <- rbind(short_bouts, long_bouts)

long_hist <- ggplot(long_bouts, aes(x = length, fill = state)) + 
  geom_histogram(binwidth = 60) + 
  theme_classic() 

short_hist <- ggplot(short_bouts, aes(x = length, fill = state)) + 
  geom_histogram(binwidth = 1) + 
  theme_classic() 

grid.arrange(long_hist, short_hist, ncol=2)

consolidation <- rbind(rest_data$Ophboo$daily_summary, rest_data$Xenbat$daily_summary)
consolidation$species <- c(rep("Ophboo", 54), rep("Xenbat", 43))
consolidation <- filter(consolidation, state == "rest")


consol_plot <- ggplot(consolidation, aes(x = L50, y = N50, colour = species)) + 
  geom_point() + 
  theme_classic() 

freq_length <- ggplot(consolidation, aes(x = freq, y = mean_length, colour = species)) + 
  geom_point() + 
  theme_classic() 

grid.arrange(consol_plot, freq_length, ncol=2)

sfi <- ggplot(consolidation, aes(x = sfi, fill = species, position = 'identity')) + 
  geom_histogram() + 
  theme_classic() 

sfi

x1 <- filter(rest_data$Ophboo$FISH20201120_c1_r0$bout_data, length < 10)
x2 <- filter(rest_data$Ophboo$FISH20201120_c1_r1$bout_data, length < 10)

fig <- plot_ly() %>%
  add_histogram(x = ~x1, name = "Variable 1", nbinsx = 10, opacity = 0.7) %>%
  add_histogram(x = ~x2, name = "Variable 2", nbinsx = 10, opacity = 0.7)
fig
