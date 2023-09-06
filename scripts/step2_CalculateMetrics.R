library(lubridate)
library(tidyr)
library(ggplot2)
library(gsheet)
library(tictoc)
library(data.table)
library(dplyr)
library(reticulate)
library(patchwork)
library(stringr)
library(ggrepel)
library(here)
library(patchwork)

source(here("scripts", "tracking_analysis_functions.R"))
source(here("scripts", "peak_finding_functions.R"))


url <- 'https://docs.google.com/spreadsheets/d/14vd7fH7Ra64nNFi5mq34iEUfIIfE2MGHqLEfxJ8C71c/edit?usp=sharing'
meta_data <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)

als.data.list <- readRDS(file = here("all_fish_als_combined.rds"))
#als.data.list <- als.data.list[!(names(als.data.list) %in% 'Neogra/FISH20200907_c6_r1_Neolamprologus-gracilis_sm_als.csv')]

als.data.list.2 <- lapply(als.data.list, function(x) summariseALSdata(als_data = x, average_by = "hour"))

#### OK, I need to find averages for day vs night, calculate total rest, and calculate peak percentages
#### Averages represent the next bin (the time on the bin is the beginning of the bin)


### Calculate day vs night averages
### Exclude 6h-8h and 18h-20h

### Summarise by time_category and individual, and group by species
week.combined <- Reduce(rbind, als.data.list.2)
week.combined <- annotateTimePoints(als_data = week.combined, zoo_times = TRUE, avg_days = FALSE, ndays = 8)
week.combined <- merge(week.combined, meta_data, by = "sample_id")


summary <- week.combined %>% group_by(tribe, species_six, sample_id, time_category) %>% summarise(mean_speed_mm = mean(mean_speed_mm))
summary <- summary %>% pivot_wider(names_from = time_category, values_from = mean_speed_mm)

summary$diff <- (summary$day - summary$night) / (summary$day + summary$night)
# Add a column with the averages per species for day-night, used for ordering plot
summary <- summary %>% group_by(species_six) %>% mutate(order_speed = mean(diff))


#######################################################################################################################
#### Find peaks #######################################################################################################
#######################################################################################################################

# Plot all days for an invidual, first set all days to '01' in a new datetime column called 'daytime'
als.data.list.3 <- lapply(als.data.list.2, function(x) {
  x$daytime <- as.POSIXct(x$datetime, format = '%Y-%m-%d %H:%M:%S')
  day(x$daytime) <- 01
  return(x)
})

dist <- 4
prom <- 7

# ... and for the average day
als.data.list.3 <- lapply(als.data.list.3, function(x) findPeaks(als_data = x, distance = dist, prominence = prom))

percentages <- lapply(als.data.list.3, function(x) {
  y <- returnPeakPercentages(als_data = x, avg_days = FALSE, zoo_times = TRUE)
    return(y)
})


### Combine into a dataframe with sample_ids
df <- tibble(dawn_percentages = unlist(lapply(percentages, function(x) x[[1]])),
             dusk_percentages = unlist(lapply(percentages, function(x) x[[2]])),
             sample_id = str_extract(names(als.data.list.3), pattern = "FISH........_c._r."))


df$percentages <- rowMeans(df[,c("dawn_percentages", "dusk_percentages")])

summary <- merge(summary, df, by = "sample_id")

# Add a column with the averages per species, used for ordering plot
summary <- summary %>% group_by(tribe, species_six) %>% mutate(order_crepuscular = mean(percentages))


#######################################################################################################################
#### Calculate total rest #############################################################################################
#######################################################################################################################

## Load the fv2 files for each species, which contain the values per individual

fv2_files <- list.files(path = here("als_files"), recursive = TRUE, pattern = "_als_fv2.csv")
fv2_data <- lapply(fv2_files, function(x) read.csv(paste(here("als_files"), x, sep = "/")))
fv2_data <- Reduce(rbind, fv2_data)
metadata <- as.data.frame(str_split_fixed(fv2_data$X, "_", 4))
metadata$sample_id <- paste(metadata$V1, metadata$V2, metadata$V3, sep = "_")
fv2_data$sample_id <- metadata$sample_id

summary <- merge(summary, fv2_data[,c("sample_id", "total_rest")], by = "sample_id")

# Add a column with the averages per species, used for ordering plot
summary <- summary %>% group_by(tribe, species_six) %>% mutate(order_total_rest = mean(total_rest))

#######################################################################################################################
#### Make day plots ###################################################################################################
#######################################################################################################################

### Make individual day averages for N. buescheri, N. tretocephalus, N. brichardi, and N. crassus

avg.day.list <- lapply(als.data.list, function(x) averageDay(als_data = x, units = "halfhour", days_include = "all"))

day.combined <- Reduce(rbind, avg.day.list)
day.combined <- merge(day.combined, meta_data, by = "sample_id")


day.summary <- day.combined %>% group_by(tribe, species_six, hour, half_hour) %>% summarise(sd_speed_mm = sd(mean_speed_mm), mean_speed_mm = mean(mean_speed_mm), datetime = datetime)


day.summary$sd_min <- day.summary$mean_speed_mm-day.summary$sd_speed_mm
day.summary$sd_min <- ifelse(day.summary$sd_min < 0, 0, day.summary$sd_min)
day.summary$sd_max <- day.summary$mean_speed_mm+day.summary$sd_speed_mm

day.summary <- unique(day.summary)

example_plots <- lapply(c("Neobue", "Neotre", "Juldic", "Enamel", "Neobri", "Neocra", "Lamsig", "Ophboo"), function(x) {
  plot <- ggplot(day.summary[day.summary$species_six == x,], aes(x = datetime, y = mean_speed_mm, group = species_six)) + 
    geom_rect_shading_zoo() + 
    theme_classic() #+ 
    #ylim(c(0,40))
  return(plot)
  })
cols <- c("black", "black", "black", "black", "black", "black", "black", "black")
titles <- c("Diurnal (Neobue)", "Nocturnal (Neotre)", "Crepuscular (Neobri)", "Cathemeral (Enamel)", "N. brichardi (crepuscular)", "N. crassus (cathemeral)", "N. signatus (high-rest)", "O. boops (low-rest)")
example_plots <- lapply(seq_along(example_plots), function(x) {
  plot <- example_plots[[x]] + 
    geom_line(colour = cols[[x]]) + 
    geom_ribbon(aes(ymin = sd_min, ymax = sd_max), alpha = 0.1, fill = cols[[x]]) +
    theme(axis.title = element_blank(), axis.text = element_text(colour = "black"), axis.text.x = element_blank(), plot.title = element_text(size = 12)) +
    ggtitle(titles[[x]]) # + ylim(c(0,80))
  return(plot)
  })


################# Make plots ##################   

summary$tribe <- factor(summary$tribe, levels = c("Boulengerochromini", "Bathybatini", "Lamprologini", "Cyphotilapiini", "Limnochromini", "Ectodini", "Cyprichromini", "Benthochromini", "Perissodini", "Eretmodini", "Haplochromini", "Tropheini"))

## Sort and plot day-night dif
summary$species_six <- factor(summary$species_six, levels = unique(summary$species_six[with(summary, order(order_speed))]))
plot_day_night <- ggplot(summary, aes(y = species_six, x = order_speed, fill = order_speed)) + 
                            geom_rect(xmin = 0, xmax = 50, ymin =62, ymax = 0, fill = "lightyellow1", colour = "transparent", alpha = 0.05) + 
                            geom_rect(xmin = -50, xmax = 0, ymin =62, ymax = 0, fill = "grey95", colour = "transparent", alpha = 0.05) + 
                            geom_vline(xintercept = 0, colour = "black", linetype="dotted") +
                            scale_fill_distiller(palette = "RdBu") +
                            geom_point(shape = 21, colour = "black", size = 4) + 
                            theme_classic() + 
                            theme(legend.position = "none", axis.title.y = element_blank(), axis.text = element_text(colour = "black")) + 
                            xlim(c(-1,1)) +
                            xlab("Diurnal/Nocturnal ratio")

## Sort and plot crepuscular percentages
summary$species_six <- factor(summary$species_six, levels = unique(summary$species_six[with(summary, order(order_crepuscular))]))
plot_crepuscular <- ggplot(summary, aes(y = species_six, x = order_crepuscular, fill = order_crepuscular)) + 
                            scale_fill_distiller(palette = "BrBG") +
                            geom_point(shape = 21, colour = "black", size = 4) + 
                            theme_classic() + 
                            theme(legend.position = "none", axis.title.y = element_blank(), axis.text = element_text(colour = "black")) + 
                            xlab("Crepuscularity")

## Sort and plot total rest
summary$species_six <- factor(summary$species_six, levels = unique(summary$species_six[with(summary, order(order_total_rest))]))
plot_total_rest <- ggplot(summary, aes(y = species_six, x = order_total_rest, fill = order_total_rest)) + 
                            geom_vline(xintercept = 2, colour = "grey", size = 3, alpha = 0.5) +
                            geom_vline(xintercept = 8, colour = "grey", size = 3, alpha = 0.5) +
                            geom_vline(xintercept = 20, colour = "grey", size = 3, alpha = 0.5) +
                            scale_fill_distiller(palette = "PRGn") +
                            geom_point(shape = 21, colour = "black", size = 4) + 
                            theme_classic() + 
                            theme(legend.position = "none", axis.title.y = element_blank(), axis.text = element_text(colour = "black")) +
                            xlab("Total inactivity/sleep (hrs)")



final_plot <- (plot_spacer() + example_plots[[1]] + plot_spacer() + example_plots[[2]] +  plot_spacer() + example_plots[[3]] + plot_spacer() + example_plots[[4]] + plot_layout(ncol = 4)) / (plot_day_night + plot_crepuscular + plot_total_rest + plot_layout(ncol = 3)) + plot_layout(heights = c(5,17.5)) + plot_annotation(tag_levels = 'A')


pdf(file = here("figure_2.pdf"), width = 7.5, height =10)
final_plot
dev.off()

### Save out brichardi and crassus plots

pdf(file = here("QTL_parentals.pdf"), width = 7.5, height = 15)
example_plots[[5]] / example_plots[[6]]
dev.off()
