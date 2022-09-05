install.packages("tidyverse")
install.packages("readr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("dplyr")
install.packages("ggplot2")
install.packages('lubridate')
installed.packages('rmarkdown')
install.packages("ggrepel")
library(tidyverse)
library(readr)
library(here)
library(skimr)
library(janitor)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggrepel)
##Import data
activity <- read.csv("daily_activity.csv",stringsAsFactors = FALSE)
intensity <- read.csv("hourly_intensities.csv")
sleep <- read.csv("sleep_day.csv")

##Inspect data
activity[duplicated(activity),]
intensity[duplicated(sleep),]
sleep[duplicated(sleep),]

str(activity)
str(intensity)
str(sleep)

is.null(activity)
is.null(intensity)
is.null(sleep)

is.na.(activity)
is.na(intensity)
is.na(sleep)

str(activity)
str(intensity)
str(sleep)

##Formating data
#The particular locale names accepted by Windows to changes into english
Sys.setlocale("LC_TIME", "English")
#Change in to weekday#
activity <- activity %>% 
  mutate( Weekday = weekdays(as.Date(ActivityDate, "%m/%d/%Y")))


#Mutate ActivtyDate data type to date / time and create new time, date#
activity$ActivityDate=as.POSIXct(activity$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")
activity$month <- format(activity$ActivityDate, format = "%B")
activity$day <- format(activity$ActivityDate, format = "%d")

#Mutate intensity data
intensity$ActivityHour=as.POSIXct(intensity$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensity$time <- format(intensity$ActivityHour, format = "%H:%M:%S")
intensity$date <- format(intensity$ActivityHour, format = "%m/%d/%y")
intensity$month <- format(intensity$ActivityHour, format = "%B")
intensity <-intensity %>% 
  mutate( Weekday = weekdays(as.Date(date, "%m/%d/%Y"))) 

intensity$Weekday <- ordered(intensity$Weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#Mutate Sleep data
sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")
sleep$month <- format(sleep$SleepDay, format = "%B")


##summary statistics
#to find Total Id
n_distinct(activity$Id)
n_distinct(sleep$Id)
n_distinct(intensity$Id)

#observations are there in each dataframe
nrow(activity)
nrow(intensity)
nrow(sleep)

#quick summary statistics
#activity
activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()
##Insert totalActiveMinutes in to the columns
activity <- activity %>%
  mutate(totalActiveMinutes = VeryActiveMinutes + FairlyActiveMinutes +
           LightlyActiveMinutes + SedentaryMinutes)
#sleep
sleep %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()


## Plotting a few explorations
#Create activity summary group by Id
activity_summary <- activity %>%
  group_by(Id) %>%
  summarise(VeryActiveMinutes = round(mean(VeryActiveMinutes)), 
            FairlyActiveMinutes = round(mean(FairlyActiveMinutes)), 
            LightlyActiveMinutes = round(mean(LightlyActiveMinutes)), 
            SedentaryMinutes = round(mean(SedentaryMinutes)))

##Insert average of totalActiveMinutes in to the columns
activity_summary <- activity_summary %>%
  mutate(avg_totalActiveMinutes = SedentaryMinutes + LightlyActiveMinutes +
           FairlyActiveMinutes + VeryActiveMinutes)
##Create activity_ratio to change in percent
activity_ratio <-activity_summary %>%
  summarise(sedentary= mean(SedentaryMinutes/avg_totalActiveMinutes),
            lightlyActive= mean(LightlyActiveMinutes/avg_totalActiveMinutes),
            fairlyActive= mean(FairlyActiveMinutes/avg_totalActiveMinutes),
            veryActive= mean(VeryActiveMinutes/avg_totalActiveMinutes)) %>%
  summarise(sedentary = round(sedentary*100,1), 
            lightlyActive = round(lightlyActive*100,1), 
            fairlyActive = round(fairlyActive*100,1), 
            veryActive=round(veryActive*100,1))
##gather type and percent together
activity_ratio <- gather(activity_ratio, activityType, perc)
##Get the positions to create chart
activity2 <- activity_ratio %>% 
  mutate(csum = rev(cumsum(rev(perc))), 
         pos = perc/2 + lead(csum, 1),
         pos = if_else(is.na(pos), perc/2, pos))
##find out ratio of Activity type
ggplot(activity_ratio, aes(x = "" , y = perc, fill = fct_inorder(activityType))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  labs(title="Daily activity ratio")+
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = activity2,
                   aes(y = pos, label = paste0(perc, "%")),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "activityType")) +
  theme_void()

#relationship in Activity dataframe
ggplot(data=activity)+
  geom_point(mapping=aes(x=TotalSteps, y=SedentaryMinutes))+
  labs(title="Relationship between Sedentary Minutes and Steps")

#relationship in Sleep dataframe
ggplot(data=sleep) +
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=TotalTimeInBed))+
  labs(title="Relationship between Time of Asleep and Time in bed")

# Merging Activity and sleep together
combined_data <- merge(sleep,activity, by=c("Id","date"))
n_distinct(combined_data$Id)
n_distinct(combined_data$date)
#relationship between activity and sleep dataframe
ggplot(data=combined_data , aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point(color='Darkgreen') + geom_smooth() +
  labs(title="Minutes Asleep vs. Sedentary Minutes")

#relationship intensity dataframe
#Average Total Intensity vs. Time
intensity_new <- intensity %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_intensity = mean(TotalIntensity))

ggplot(data=intensity_new, aes(x=time, y=mean_total_intensity)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")

#Average Total Intensity vs. Weekday
intensity_new <- intensity %>% 
  group_by(Weekday) %>%
  drop_na() %>%
  summarise(mean_total_intensity = mean(TotalIntensity))

ggplot(data=intensity_new, aes(x=Weekday, y=mean_total_intensity)) + 
  geom_bar(stat = "identity", fill='darkorange') +
  labs(title="Average Total Intensity vs. Weekday")

 