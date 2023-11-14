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
#weekly_consolidation$proportion <- weekly_consolidation$avg_total/24
#weekly_consolidation$phase_proportion = ifelse(weekly_consolidation$start_phase == "dawn" | weekly_consolidation$start_phase == "dusk", weekly_consolidation$avg_total/2, weekly_consolidation$avg_total/10)

daily_consolidation$total_hour <- as.numeric(daily_consolidation$total_hour)
daily_consolidation$freq <- as.numeric(daily_consolidation$freq)
daily_consolidation$mean_length <- as.numeric(daily_consolidation$mean_length)
daily_consolidation$log_sfi <- log(as.numeric(daily_consolidation$sfi))
daily_consolidation$L50 <- as.numeric(daily_consolidation$L50)
daily_consolidation$N50 <- as.numeric(daily_consolidation$N50)
#daily_consolidation$proportion <- daily_consolidation$total_hour/24
#daily_consolidation$phase_proportion = ifelse(daily_consolidation$start_phase == "dawn" | daily_consolidation$start_phase == "dusk", daily_consolidation$total_hour/2, daily_consolidation$total_hour/10)

consol_byspecies <- group_by(weekly_consolidation, species_six, start_phase)
consol_byspecies <- summarise(consol_byspecies, avg_total = mean(avg_total), avg_freq = mean(avg_freq), 
                              avg_length = mean(avg_length), sfi = mean(sfi),
                              avg_L50 = mean(avg_L50), avg_N50 = mean(avg_N50),
                              avg_proportion = mean(avg_proportion))

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


sfi_byphase <- ggplot(weekly_consolidation, aes(reorder_within(species_six, log_sfi, start_phase), log_sfi, fill = species_six)) + 
  geom_boxplot() + 
  stat_summary(fun=mean, colour="darkred", geom="point") +
  facet_wrap(~start_phase, scales = "free_x", ncol = 2) +
  #scale_fill_gradient(low=hcl(15,100,75), high=hcl(195,100,75)) +
  scale_x_reordered() +
  theme(axis.ticks = element_blank(), axis.text.x = element_text(angle=90)) +
  guides(color = "none", fill = "none") +
  ggtitle("Cichlid log(SFI) boxplot by phase") +
  xlab("Species") + ylab("log(SFI)")
sfi_byphase

sfi <- ggplot(weekly_consolidation, aes(x=species_six, y=log_sfi, fill = species_six)) + 
  geom_boxplot(aes(reorder(species_six, log_sfi, y = log_sfi))) + 
  scale_x_reordered() +
  theme(axis.ticks = element_blank(), axis.text.x = element_text(angle=90)) +
  guides(color = "none", fill = "none") +
  ggtitle("Cichlid log(SFI) boxplot") +
  xlab("Species") + ylab("log(SFI)")
sfi

N50 <- ggplot(weekly_consolidation, aes(x=species_six, y=avg_N50, fill = species_six)) + 
  geom_boxplot() + 
  facet_wrap(~start_phase) +
  scale_x_reordered() +
  theme(axis.ticks = element_blank(), axis.text.x = element_text(angle=90)) +
  guides(color = "none", fill = "none") +
  ggtitle("Cichlid N50 boxplot") +
  xlab("Species") + ylab("N50")
N50

sftotal_byphase <- ggplot(consol_byspecies, aes(reorder_within(species_six, avg_total, start_phase), avg_total, colour = species_six)) + 
  geom_point(aes(size = 5)) +
  facet_wrap(~start_phase, scales = "free_x", ncol = 2) +
  scale_x_reordered() +
  theme(axis.ticks = element_blank(), axis.text.x = element_text(angle=90)) +
  guides(color = "none", size = "none", fill = "none") +
  ggtitle("Cichlid total sleep boxplot by phase") +
  xlab("Species") + ylab("Total sleep (h)")
total_byphase

L50 <- ggplot(consol_byspeciesONLY, aes(x=avg_N50, y= avg_L50, colour = species_six, fill = species_six)) + 
  geom_point(aes(size = 5)) + 
  geom_text(aes(label = species_six), hjust="inward", vjust=-1) +
  #scale_x_reordered() + 
  scale_x_continuous() +
  guides(color = "none", size = "none", fill = "none") +
  ggtitle("Cichlids weekly average sleep by L50 and N50") +
  theme(axis.ticks.x = element_line(), axis.text.x = element_text(angle=90)) +
  xlab("N50") + ylab("L50") 
L50

freq <- ggplot(consol_byspeciesONLY, aes(x=avg_freq, y= avg_length, colour = species_six, fill = species_six, label = species_six)) + 
  geom_point(aes(size = 5)) + 
  geom_text(aes(label = species_six), hjust="inward", vjust=-1) +
  scale_x_continuous() +
  guides(color = "none", size = "none", fill = "none") +
  ggtitle("Cichlids weekly average sleep by length and frequency") +
  theme(axis.ticks = element_line(), axis.text.x = element_text()) +
  #theme_classic() +
  xlab("Frequency") + ylab("Length") 
freq

grid.arrange(L50, freq, ncol=2)
