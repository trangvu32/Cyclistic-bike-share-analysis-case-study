#Import libraries
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
d10_2020 <- read_csv("202010-divvy-tripdata.csv")
d11_2020 <- read_csv("202011-divvy-tripdata.csv")
d12_2020 <- read_csv("202012-divvy-tripdata.csv")
d01_2021 <- read_csv("202101-divvy-tripdata.csv")
d02_2021 <- read_csv("202102-divvy-tripdata.csv")
d03_2021 <- read_csv("202103-divvy-tripdata.csv")
d04_2021 <- read_csv("202104-divvy-tripdata.csv")
d05_2021 <- read_csv("202105-divvy-tripdata.csv")
d06_2021 <- read_csv("202106-divvy-tripdata.csv")
d07_2021 <- read_csv("202107-divvy-tripdata.csv")
d08_2021 <- read_csv("202108-divvy-tripdata.csv")
d09_2021 <- read_csv("202109-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the sa me order, they DO need to match perfectly before we can use a command to join them into one file
colnames(d10_2020)
colnames(d11_2020)
colnames(d12_2020)
colnames(d01_2021)
colnames(d02_2021)
colnames(d03_2021)
colnames(d04_2021)
colnames(d05_2021)
colnames(d06_2021)
colnames(d07_2021)
colnames(d08_2021)
colnames(d09_2021)
# Rename columns  to make them consisent with q1_2020 (as this will be the supposed going-forward table design for Divvy)

#All names are already consistent - they do not need to be renamed. 
# Inspect the dataframes and look for incongruencies
str(d10_2020)
str(d11_2020)
str(d12_2020)
str(d01_2021)
str(d02_2021)
str(d03_2021)
str(d04_2021)
str(d05_2021)
str(d06_2021)
str(d07_2021)
str(d08_2021)
str(d09_2021)
#We need to convert start_station_id to character so we can peform calucations correctly later on.
d10_2020 <-  mutate(d10_2020, start_station_id = as.character(start_station_id) 
                    ,end_station_id = as.character(end_station_id))
d11_2020 <-  mutate(d11_2020, start_station_id = as.character(start_station_id)
                    ,end_station_id = as.character(end_station_id))

#We do not need to convert the id because the id is already in character. 
#Checking one dataset if the conversion happens or not --> but we do not need to check because we did not convert data. 

#=======
# Step 3: Merge the datasets
#=======
# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(d10_2020, d11_2020, d12_2020, d01_2021, d02_2021, d03_2021, d04_2021, d05_2021, d06_2021, d07_2021, d08_2021, d09_2021)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat,start_lng,end_lat,end_lng))
colnames(all_trips)
#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics
# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
table(all_trips$member_casual)
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "Casual"))
table(all_trips$member_casual)
# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
# Add a "ride_length" calculation to all_trips (in minutes)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at,units = "mins")
head(all_trips$ride_length)
# Inspect the structure of the columns
str(all_trips)
# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
#Remove NA
colSums(is.na(all_trips_v2))#Check the missing values in the dataset
# remove na
all_trips_v3 <- all_trips_v2[!(is.na(all_trips_v2$start_station_id) | is.na(all_trips_v2$end_station_id) | is.na(all_trips_v2$member_casual) | is.na(all_trips_v2$end_station_name)),]
table(all_trips_v3$member_casual)
colSums(is.na(all_trips_v3))
#================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#================
summary(all_trips_v3$ride_length) # ride length in seconds
# Compare members and casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = mean)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = median)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = max)
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual, FUN = min)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v3$day_of_week <- ordered(all_trips_v3$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)
# analyze ridership data by type and weekday
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n()	#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday)# sorts

#SHARE
# Let's visualize the number of rides by rider type
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%           
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") +
  scale_fill_manual("Member_casual",values = c('#00D1F8', '#F8EF00'))+
  ggtitle( "Number Of Rides By Rider Type")

# Let's create a visualization for average duration
all_trips_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  scale_fill_manual("Member_casual",values = c('#00D1F8', '#F8EF00'))+
  ggtitle( "Average Cyclistic Bike-Share Rides")
glimpse(all_trips_v3)
#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
counts <- aggregate(all_trips_v3$ride_length ~ all_trips_v3$member_casual + all_trips_v3$day_of_week, FUN = mean)
glimpse(counts)
write.csv(counts, file = 'avg_ride_length.csv')

#=================
#Aggregating the file
#=================


















