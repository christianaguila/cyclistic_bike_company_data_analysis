## install packages

install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("reshape")


## load packages
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(reshape)



## Step 1: Collect Data
## importing dataset
getwd()
setwd("C:/Users/Christian Rhomel/Documents/Data Analytics/Course_8_Capstone_Project")

data_09_2021 <- read_csv("202109-divvy-tripdata.csv")
data_10_2021 <- read_csv("202110-divvy-tripdata.csv")
data_11_2021 <- read_csv("202111-divvy-tripdata.csv")
data_12_2021 <- read_csv("202112-divvy-tripdata.csv")
data_01_2022 <- read_csv("202201-divvy-tripdata.csv")
data_02_2022 <- read_csv("202202-divvy-tripdata.csv")
data_03_2022 <- read_csv("202203-divvy-tripdata.csv")
data_04_2022 <- read_csv("202204-divvy-tripdata.csv")
data_05_2022 <- read_csv("202205-divvy-tripdata.csv")
data_06_2022 <- read_csv("202206-divvy-tripdata.csv")
data_07_2022 <- read_csv("202207-divvy-tripdata.csv")
data_08_2022 <- read_csv("202208-divvy-tripdata.csv")
data_09_2022 <- read_csv("202209-divvy-tripdata.csv")


## Step 2: Data Wrangling 
## Checking the column names before combining the data into single file
colnames(data_09_2021)
colnames(data_10_2021)
colnames(data_11_2021)
colnames(data_12_2021)
colnames(data_01_2022)
colnames(data_02_2022)
colnames(data_03_2022)
colnames(data_04_2022)
colnames(data_05_2022)
colnames(data_06_2022)
colnames(data_07_2022)
colnames(data_08_2022)
colnames(data_09_2022)


# Stack individual month's data frames into a one big data frame
all_trips <- bind_rows(data_09_2021, data_10_2021, data_11_2021,
                       data_12_2021, data_01_2022, data_02_2022,
                       data_03_2022, data_04_2022, data_05_2022,
                       data_06_2022, data_07_2022, data_08_2022,
                       data_09_2022)

## Changing the column names to be more consistent. Apparently both dplyr and reshape package has the same rename function. Hence, package must be stated using ::.
all_trips <- dplyr::rename(all_trips, trip_id = ride_id, bike_type = rideable_type,
                   from_station_name = start_station_name,
                   from_station_id = start_station_id,
                   to_station_name = end_station_name,
                   to_station_id = end_station_id,
                   user_type = member_casual)


# Checking the number of unique elements in user_type column
n_unique(all_trips$user_type)
n_unique(all_trips$bike_type)
n_unique(all_trips$trip_id)


## Cleaning and Adding Data to Prepare for Analysis
dim(all_trips)
str(all_trips)
summary(all_trips)


# Creating different fields/columns for month, day, year, and time. The goal is 
# to have a field for trip duration or ride length

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$month <- month.abb[as.numeric(all_trips$month)]

# Creating a field for ride length by getting the time difference
all_trips$ride_length_sec <- difftime(all_trips$ended_at, all_trips$started_at)

  
# Converting ride_length from Factor (contains string and integer) to numeric 
# so that I can perform calculations on it

is.factor(all_trips$ride_length_sec)
all_trips$ride_length_min <- (as.numeric(as.character(all_trips$ride_length_sec)))/60
is.numeric(all_trips$ride_length_min)


# Checking for out of bound/out of range/negative values in ride_length_sec field
all_trips[all_trips$ride_length_sec < 0,] 


# Alternative way of doing the code above
subset(all_trips, all_trips$ride_length_min < 0)

# Removing all entries with zero or less than zero ride length. It doesn't make any sense 
all_trips <- subset(all_trips, all_trips$ride_length_min > 0)


# Removing all rows with missing values
all_trips[rowSums(is.na(all_trips)) > 0,]
all_trips <- all_trips %>% na.omit()


# Descriptive Analysis in ride length
summary(all_trips$ride_length_min)


# Comparing annual members and casual riders
aggregate(all_trips$ride_length_min ~ all_trips$user_type, data = all_trips, FUN = mean)
aggregate(all_trips$ride_length_min ~ all_trips$user_type, data = all_trips, FUN = median)
aggregate(all_trips$ride_length_min ~ all_trips$user_type, data = all_trips, FUN = min)
aggregate(all_trips$ride_length_min ~ all_trips$user_type, data = all_trips, FUN = max)

# Putting the days of the week column in right order (or just use wday() function)
all_trips$day_of_week <- ordered(all_trips$day_of_week, levels = c("Sunday",
                                                                   "Monday",
                                                                   "Tuesday",
                                                                   "Wednesday",
                                                                   "Thursday",
                                                                   "Friday",
                                                                   "Saturday"))

aggregate(all_trips$ride_length_min ~ all_trips$user_type + all_trips$day_of_week,
          FUN = mean)





# Showing relationship between user_type and bike_type
all_trips_v1 <- all_trips %>% select(user_type, bike_type) 
all_trips_v1$count <- 1
View(cast(all_trips_v1, user_type~bike_type, value = "count"))
# Here we can see that member users don't use docked bikes

all_trips_export <- select(all_trips, -c(trip_id, started_at, ended_at, from_station_id, to_station_id, day, date, ride_length_sec))
all_trips_export_v2 <- select(all_trips, -c(trip_id, started_at, ended_at, from_station_id, to_station_id, day, ride_length_sec))
all_trips_same_station <- subset(all_trips_export_v2, all_trips_export_v2$from_station_name == all_trips_export_v2$to_station_name) 


write.csv(all_trips_export_v2, "C:/Users/Christian Rhomel/Documents/Data Analytics/Course_8_Capstone_Project/all_trips_v2.csv")
write.csv(all_trips_same_station, "C:/Users/Christian Rhomel/Documents/Data Analytics/Course_8_Capstone_Project/all_trips_same_station.csv")


# analyze ridership data by type and weekday
p <- all_trips %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(user_type, weekday) %>% 
  summarize(average_duration = mean(ride_length_sec)) %>% 
  arrange(user_type, weekday)

# creating visualization for average_duration
ggplot(p) + geom_col(aes(x = weekday, y = average_duration, fill = user_type),
                     position = "dodge") +
  labs(title = "Average Trip Duration of Casual Riders and Annual Members",
       subtitle = "Data from October 2021 to October 2022",
       caption = "Data Source: Motivate International Inc.",
       x = "Weekday", 
       y = "Average Trip DUration (sec)",
       fill = "User Type") +
  theme_bw()
 



all_trips %>% group_by(month, user_type) %>%
  ggplot(aes(x = month, y = ride_length_sec, fill = user_type)) +
  geom_col(position = "dodge") +
  labs(fill = "User Type") +
  theme_bw()




# Themes for R markdown: flatly, readable,simplex
