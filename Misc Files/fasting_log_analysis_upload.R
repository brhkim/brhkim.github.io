################################################################################
#  
#   Name: fasting_log_analysis.R
#   
#   Purpose: Take @brhkim's fasting log and visualize data
#
################################################################################


################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

rm(list = ls())

libs <- c('tidyverse','ggplot2', 'lubridate', 'ggridges', 'viridis')
sapply(libs, require, character.only = TRUE)

username <- Sys.getenv("USERNAME")
home <- Sys.getenv("HOME")

setwd(home)

root <- file.path('..','Google Drive', 'Misc Documents', 'Health and Fitness', 'Fasting Log Analysis')
project_output <- file.path(root, 'output')
project_data <- file.path(root, 'data')

################################################################################
#
#   #import - Load in a dataset and start taking a peek around
#
################################################################################

## Load in the data
fasting <- read_csv(file.path(project_data, 'Fasting Log - Log.csv'))
fasting <- fasting[-1,]
fasting <- fasting %>% filter(!is.na(fasting$'Eating Begin'))


################################################################################
#
#   #clean - Clean variables and process them before visualizing
#
################################################################################

## Get dates altogether
fasting$beginfix <- paste(as.character(fasting$Date), as.character(fasting$`Eating Begin`))
fasting$beginfix <- mdy_hms(fasting$beginfix)

fasting$endfix <- paste(as.character(fasting$Date), as.character(fasting$`Eating End`))
fasting$endfix <- mdy_hms(fasting$endfix)

## Drop out any dates outside the specific year of 2019
fasting <- fasting %>% filter(year(beginfix) == 2019)

## Add one minute and one hour to any instances of 23:59:00 to round it out
fasting <- fasting %>% mutate(endfix = case_when(as.character(fasting$'Eating End')=="23:59:00" ~ (endfix + hours(1) + minutes(1)),
                                              as.character(fasting$'Eating End')!="23:59:00" ~ endfix))

## Get eating duration
fasting <- fasting %>% mutate(windowfix = as.numeric(endfix - beginfix))

## Get fasting duration
fasting$lastend <- lag(fasting$endfix, 1)
fasting <- fasting %>% mutate(durationfix = as.numeric(beginfix-lastend))
fasting <- fasting %>% mutate(fastlength = ifelse(durationfix >= 16, 1, 0))
fasting <- fasting %>% mutate(fastlength = ifelse(durationfix >= 20, 2, fastlength))
fasting <- fasting %>% mutate(fastlength = ifelse(durationfix >= 24, 3, fastlength))
fasting$fastlength <- as.factor(fasting$fastlength)

## Properly create beginning and end eating window times
fasting$beginhour <- hour(fasting$beginfix)
fasting$beginmin <- minute(fasting$beginfix)
fasting$beginsec <- second(fasting$beginfix)

fasting <- fasting %>% mutate(begintime = hms::as.hms(beginhour*3600 + beginmin*60 + beginsec))

fasting$endhour <- hour(fasting$endfix)
fasting$endmin <- minute(fasting$endfix)
fasting$endsec <- second(fasting$endfix)

fasting <- fasting %>% mutate(endtime = hms::as.hms(endhour*3600 + endmin*60 + endsec))

fasting$datefix <- date(fasting$beginfix)
fasting$monthlab <- month(fasting$beginfix, label=TRUE, abbr = FALSE)
fasting$dayweek <- wday(fasting$beginfix)
fasting$day <- day(fasting$beginfix)

################################################################################
#
#   #visualize - Visualize the data using ggplot2 heatmaps
#
################################################################################

  
# Fasting Duration Heatmaps, Continuous
ggplot(fasting, aes(x = day, y = monthlab)) + 
  geom_tile(aes(fill = durationfix), colour="white", size=1) + 
  guides(fill = guide_colourbar(reverse = TRUE)) + 
  theme_ridges(font_size = 10, center_axis_labels = TRUE) +
  scale_x_continuous(lim=c(0,32), breaks=c(1, 5, 10, 15, 20, 25, 30)) + 
  scale_y_discrete(limits=rev(levels(fasting$monthlab))) + 
  # scale_fill_distiller(palette="YlGnBu", direction=1) +
  scale_fill_gradientn(lim=c(12,26), colors= c("#e0e0e0", "#9ecae1", "#2171b5", "#000d42"), na.value="#e0e0e0", breaks=c(8, 12, 16, 20, 24)) +
  labs(x="Day of Month", y="Month", fill="Fasting \nDuration \nin Hours \t \t \t", title="5. Day-by-Day Fasting Window Durations Across One Year", subtitle="Raw Window Durations")

  ## Save file
  filename <- "heatmap.jpg"
  ggsave(file.path(project_output, filename), width=10, height=6)
  
