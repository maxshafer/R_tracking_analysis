library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(gridExtra)
library(tidyr)
library(tidytext)
library(RColorBrewer)

weekly <- read.csv("/Users/AshasMacbookAir/Documents/master/sleep evo/all_samples_weekly.csv")
daily <- read.csv("/Users/AshasMacbookAir/Documents/master/sleep evo/all_samples_daily.csv")
bouts <- read.csv("/Users/AshasMacbookAir/Documents/master/sleep evo/all_samples_bout_structure.csv")

daily_consolidation <- filter(daily, state == "rest")
weekly_consolidation <- filter(weekly, state == "rest")

weekly_consolidation$avg_total <- as.numeric(weekly_consolidation$avg_total)
weekly_consolidation$avg_freq <- as.numeric(weekly_consolidation$avg_freq)
weekly_consolidation$avg_length <- as.numeric(weekly_consolidation$avg_length)
weekly_consolidation$log_sfi <- log(as.numeric(weekly_consolidation$sfi))
weekly_consolidation$avg_L50 <- as.numeric(weekly_consolidation$avg_L50)
weekly_consolidation$avg_N50 <- as.numeric(weekly_consolidation$avg_N50)

consolidation$total_hour <- as.numeric(consolidation$total_hour)
weekly_consolidation$avg_freq <- as.numeric(weekly_consolidation$avg_freq)
weekly_consolidation$avg_length <- as.numeric(weekly_consolidation$avg_length)
consolidation$log_sfi <- log(as.numeric(consolidation$sfi))
weekly_consolidation$avg_L50 <- as.numeric(weekly_consolidation$avg_L50)
weekly_consolidation$avg_N50 <- as.numeric(weekly_consolidation$avg_N50)

consol_byspecies <- group_by(weekly_consolidation, species_six, start_phase)
consol_byspecies <- summarise(consol_byspecies, avg_total = mean(avg_total), avg_freq = mean(avg_freq), 
                              avg_length = mean(avg_length), sfi = mean(sfi),
                              avg_L50 = mean(avg_L50), avg_N50 = mean(avg_N50))

consol_byspecies$avg_total <- as.numeric(consol_byspecies$avg_total)
consol_byspecies$avg_freq <- as.numeric(consol_byspecies$avg_freq)
consol_byspecies$avg_length <- as.numeric(consol_byspecies$avg_length)
consol_byspecies$log_sfi <- log(as.numeric(consol_byspecies$sfi))
consol_byspecies$avg_L50 <- as.numeric(consol_byspecies$avg_L50)
consol_byspecies$avg_N50 <- as.numeric(consol_byspecies$avg_N50)

consol_byspeciesONLY <- group_by(weekly_consolidation, species_six)

consol_byspeciesONLY$avg_total <- as.numeric(consol_byspeciesONLY$avg_total)
consol_byspeciesONLY$avg_freq <- as.numeric(consol_byspeciesONLY$avg_freq)
consol_byspeciesONLY$avg_length <- as.numeric(consol_byspeciesONLY$avg_length)
consol_byspeciesONLY$log_sfi <- log(as.numeric(consol_byspeciesONLY$sfi))
consol_byspeciesONLY$avg_L50 <- as.numeric(consol_byspeciesONLY$avg_L50)
consol_byspeciesONLY$avg_N50 <- as.numeric(consol_byspeciesONLY$avg_N50)

consol_byspeciesONLY <- summarise(consol_byspeciesONLY, avg_total = mean(avg_total), avg_freq = mean(avg_freq), 
                                  avg_length = mean(avg_length), log_sfi = mean(log_sfi),
                                  avg_L50 = mean(avg_L50), avg_N50 = mean(avg_N50))


dawn <- filter(consol_byspecies, start_phase == "dawn")
dusk <- filter(consol_byspecies, start_phase == "dusk")
night <- filter(consol_byspecies, start_phase == "night")
day <- filter(consol_byspecies, start_phase == "day")
#x = species_six, y = log_sfi, fill = species_six))
#aes(reorder(species_six, log_sfi, y = log_sfi))

sfi_byphase <- ggplot(consolidation, aes(reorder_within(species_six, log_sfi, start_phase), log_sfi, fill = species_six)) + 
  geom_boxplot() + 
  facet_wrap(~start_phase, scales = "free_x", ncol = 2) +
  scale_x_reordered() +
  theme(axis.ticks = element_blank(), axis.text.x = element_text(angle=90)) +
  guides(color = "none", fill = "none") +
  ggtitle("Cichlid log(SFI) boxplot by phase") +
  xlab("Species") + ylab("log(SFI)")

sfi_byphase

sfi <- ggplot(consolidation, aes(x=species_six, y=log_sfi, fill = species_six)) + 
  geom_boxplot(aes(reorder(species_six, log_sfi, y = log_sfi))) + 
  scale_x_reordered() +
  theme(axis.ticks = element_blank(), axis.text.x = element_text(angle=90)) +
  guides(color = "none", fill = "none") +
  ggtitle("Cichlid log(SFI) boxplot") +
  xlab("Species") + ylab("log(SFI)")

sfi

L50 <- ggplot(consol_byspeciesONLY, aes(x=avg_N50, y= avg_L50, colour = species_six, fill = species_six)) + 
  geom_point(aes(size = 5)) + 
  geom_text(aes(label = species_six), hjust="inward", vjust=-1) +
  scale_x_reordered() +
  guides(color = "none", size = "none", fill = "none") +
  ggtitle("Cichlids weekly average sleep by L50 and N50") +
  theme(axis.ticks = element_line(), axis.text.x = element_text(angle=90)) +
  xlab("N50") + ylab("L50") 
L50

freq <- ggplot(consol_byspeciesONLY, aes(x=avg_freq, y= avg_length, colour = species_six, fill = species_six, label = species_six)) + 
  geom_point(aes(size = 5)) + 
  geom_text(aes(label = species_six), hjust="inward", vjust=-1) +
  scale_x_reordered() +
  guides(color = "none", size = "none", fill = "none") +
  ggtitle("Cichlids weekly average sleep by length and frequency") +
  theme(axis.ticks = element_line(), axis.text.x = element_text()) +
  #theme_classic() +
  xlab("Frequency") + ylab("Length") 
freq

grid.arrange(consol_plot, freq_length, ncol=2)

sfi <- ggplot(consolidation, aes(x = log_sfi, fill = species, position = 'identity')) + 
  geom_histogram() + 
  theme_classic() 

sfi

x1 <- filter(rest_data$Ophboo$FISH20201120_c1_r0$bout_data, length < 10)
x2 <- filter(rest_data$Ophboo$FISH20201120_c1_r1$bout_data, length < 10)

fig <- plot_ly() %>%
  add_histogram(x = ~x1, name = "Variable 1", nbinsx = 10, opacity = 0.7) %>%
  add_histogram(x = ~x2, name = "Variable 2", nbinsx = 10, opacity = 0.7)
fig
