


### PRE CLEAN ###



# Setup Packages
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)

#check my working directory
setwd("~/Google Data Analytics Course/8. Data Analytics Capstone/2. Building your portfolio/Case_Study_1/Data")

# Import CSV data to create data frames
df_1 <- read_csv('202407-divvy-tripdata.csv')
df_2 <- read_csv('202408-divvy-tripdata.csv')
df_3 <- read_csv('202409-divvy-tripdata.csv')
df_4 <- read_csv('202410-divvy-tripdata.csv')
df_5 <- read_csv('202411-divvy-tripdata.csv')
df_6 <- read_csv ('202412-divvy-tripdata.csv')
df_7 <- read_csv('202501-divvy-tripdata.csv')
df_8 <- read_csv('202502-divvy-tripdata.csv')
df_9 <- read_csv('202503-divvy-tripdata.csv')
df_10 <- read_csv('202504-divvy-tripdata.csv')
df_11 <- read_csv('202505-divvy-tripdata.csv')
df_12 <- read_csv('202506-divvy-tripdata.csv')
df_13 <- read_csv('202507-divvy-tripdata.csv')

#Examine the data frames
str(df_1)
str(df_2)
str(df_3)
str(df_4)
str(df_5)
str(df_6)
str(df_7)
str(df_8)
str(df_9)
str(df_10)
str(df_11)
str(df_12)
str(df_13)

# Combined into 1 data frame
bikerides <- rbind(df_1,df_2,df_3,df_4,df_5,df_6,df_7,df_8,df_9,df_10,df_11,df_12,df_13)

# Confirm the data frames have merged successfully
rowtotal <- sum(nrow(df_1),nrow(df_2),nrow(df_3),nrow(df_4),nrow(df_5),nrow(df_6),nrow(df_7),nrow(df_8),nrow(df_9),nrow(df_10),nrow(df_11),nrow(df_12),nrow(df_13))

print (rowtotal)
print (nrow(bikerides))

# Examine the combined data frame
str (bikerides)
head (bikerides)

# Check that primary key has no duplicates
bikerides %>%
  distinct(ride_id, keep.all = TRUE) %>%
  count(keep.all) %>%
  view()

# Check what types of rideable_type there are
bikerides %>%
distinct(rideable_type) %>%
  view()

# Check for rides less than 1 minute 
bikerides %>%
  count(ended_at -started_at <=60) %>%
  view()

# Review values in start_station_id and end_station_id
bikerides %>%
  count(start_station_id) %>%
  view()

bikerides %>%
  count(end_station_id) %>%
  view()

# Check lat and lng for null values
bikerides %>%
  count(start_lat <0) %>%
  view()

bikerides %>%
  count(start_lng >0) %>%
  view()

bikerides %>%
  count(end_lat <0) %>%
  view()

bikerides %>%
  count(end_lng >0) %>%
  view()



### CLEAN ###



# Create columns for Date, Month, Day, Year, Day of the week, Ride length and Hours
bikerides <- bikerides %>%
  mutate(Date = date(started_at),
         Day = day(started_at),
         Month = month(started_at, label = TRUE, abbr = TRUE),
         Year = year(started_at),
         Day_of_week = weekdays(started_at),
         ride_duration = difftime(ended_at,started_at, units = 'mins'),
         Hours = hour(started_at))
  

# Make Day_of_week column read starting from Monday
bikerides$Day_of_week <- ordered(bikerides$Day_of_week, 
                                       levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Remove any rows with a ride time of less than 1 min
bikerides <- bikerides[!(bikerides$ride_duration <=1),]

# Drop NA values and remove unnecessary columns
bikerides_clean <- bikerides %>%
  drop_na() %>%
  select(-ride_id,-started_at,-ended_at,-start_station_id,-end_station_id,)

# Remove original data frames to de-clutter
rm(df_1,df_2,df_3,df_4,df_5,df_6,df_7,df_8,df_9,df_10,df_11,df_12,df_13)



### Data Manipulation ###



# work out min, max and mean ride_duration for both casual and members
aggregate(ride_duration ~ member_casual, bikerides_clean, FUN = min)

aggregate(ride_duration ~ member_casual, bikerides_clean, FUN = max)

aggregate(ride_duration ~ member_casual, bikerides_clean, FUN = mean)
  

# Chart for the number of casual vs members as a total
ggplot(data = bikerides_clean) +
  geom_bar(mapping = aes(x = member_casual, fill = member_casual),width = 0.618, show.legend = FALSE) +
   labs(x = 'Member type', 
       y = 'Number of rides', 
       title = 'Total number of Casual vs Member users',
       caption = 'Data provided by Motivate International Inc')
  

# Chart for the spread across rideable type
ggplot(data = bikerides_clean) + 
  geom_bar(mapping = aes(x = rideable_type, fill = member_casual),width = 0.618, position = 'dodge') +
  labs(x = 'Type of bike',
       y = 'Number of rides',
       title = 'Casual vs Member bike distribution',
       caption = 'Data provided by Motivate International Inc') +
  theme(legend.title=element_blank())


# Chart for casual vs members spread across a week
ggplot(data = bikerides_clean) +
  geom_bar(mapping = aes(x = Day_of_week, fill = member_casual), width = 0.618, position = 'dodge') +
  labs(x = 'Day of the Week', 
       y = 'Number of rides', 
       title = 'Casual vs Member: Week',
       caption = 'Data provided by Motivate International Inc') + 
  theme(legend.title=element_blank())


# Table for casual vs members spread across a month
ggplot(data = bikerides_clean) +
  geom_bar(mapping = aes(x = Day, fill = member_casual), width = 0.618, position = 'dodge') +
  labs(x = 'Day of the month', 
       y = 'Number of rides', 
       title = 'Casual vs Member: Month',
       caption = 'Data provided by Motivate International Inc') + 
  theme(legend.title=element_blank())


# Table for casual vs members spread across a year
ggplot(data = bikerides_clean) +
  geom_bar(mapping = aes(x = Month, fill = member_casual), width = 0.618, position = 'dodge') +
  labs(x = 'Month', 
       y = 'Number of rides', 
       title = 'Casual vs Member: Year',
       caption = 'Data provided by Motivate International Inc') +
  theme(legend.title=element_blank())
           

# Line chart for number of rides across a day 
bikerides_clean %>%
  group_by(member_casual, Hours) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = Hours, y = number_of_rides, color = member_casual)) +
  geom_line() +
  labs(title = 'Casual vs Member: Day',
       caption = 'Data provided by Motivate International Inc') +
  scale_y_continuous() + 
  theme(legend.title=element_blank())


# Vector for the top 10 most popular starting stations for casuals
bikerides_clean %>%  #####TRY OUT#####
  group_by(start_station_name) %>%
  summarise(total_count = length(start_station_name)) %>%
  arrange(desc(total_count)) %>%
  head(10) %>%
  view()
  
# Vector for the top 5 or 10 most popular ending stations for casuals
bikerides_clean %>%  
group_by(end_station_name) %>%
  summarise(total_count = length(end_station_name)) %>%
  arrange(desc(total_count)) %>%
  head(10) %>%
  view()


# Export data frame to work in Tableau
write.csv(bikerides_clean, file = 'Cyclistic_clean.csv')


