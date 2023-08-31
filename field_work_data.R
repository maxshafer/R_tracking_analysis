library(lubridate)
library(tidyr)
library(ggplot2)
library(gsheet)
library(tictoc)
library(data.table)
library(dplyr)
url <- 'https://docs.google.com/spreadsheets/d/1IU6dzH7gYW5OgLho26Nu9IfQ9D4nIqGG4y6XEY0fAH8/edit?pli=1#gid=1049314205'
fw_data <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)

# You can use 'dim()' or 'head()' to quickly examine the results dataframe to see if it's correct
dim(fw_data)
head(fw_data)

## Then plot

p <- ggplot(fw_data, aes(x = time, y = numfish, group = time, color = time)) + geom_boxplot() 
p

######################################################
####Boxplot mit Linien vom median verbinden ###
#####################################################

##step1
fw_mean <- fw_data %>% 
  group_by(time) %>% 
  summarize(value = median(as.numeric(numfish), na.rm = TRUE)) 

###step 2
fw_data %>% 
  ggplot(mapping = aes(x = time, y = numfish)) + 
  geom_boxplot() +
  geom_point(data = fw_mean,
             mapping = aes(x = time, y = value),
             color="red") 

### step3
p<- fw_data %>% 
  ggplot(mapping = aes(x = time, y = numfish)) + 
  geom_boxplot() +
  geom_point(data = fw_mean, 
             mapping = aes(x = time, y = value),
             color="red") +
  geom_line(data = fw_mean, 
            mapping = aes(x = time, y = value, group=1)) +
  annotate("rect", xmin=c(0,18.25), xmax=c(5.25,24.5), ymin=c(0,0) , ymax=c(380,380), alpha=0.2, fill="3333FF")
p1 <- p + annotate("rect", xmin=c(5.25,17.75), xmax=c(5.5,18.25), ymin=c(0,0) , ymax=c(380,380), alpha=0.2, fill="gold") + theme_classic()
p2 <- p1 + annotate("rect", xmin=c(12), xmax=c(15), ymin=c(0,0) , ymax=c(380,380), alpha=0.1, fill="darkolivegreen3")

### step3
# fw_data %>% 
#  ggplot(mapping = aes(x = time, y = numfish)) + 
#  geom_boxplot() +
#  geom_point(data = fw_mean, 
#             mapping = aes(x = time, y = value),
#             color="red") +
#  geom_line(data = fw_mean, 
#            mapping = aes(x = time, y = value, group=1, colour = time))

######################## plot by timegroups ##########################
p <- ggplot(fw_data, aes(x = d_n, y = numfish, group = d_n, color = d_n)) + geom_boxplot() 
p
