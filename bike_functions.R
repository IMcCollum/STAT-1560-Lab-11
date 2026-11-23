# bike_functions.R

#Function #1: Simulate NHPP arrivals

#LOAD DATA 

raw_df <- read.csv("/Users/ianzmccollum/Desktop/course_files_export/Data/sample_bike.csv")

#LOAD PACKAGES 

library(tidyr)
library(dplyr)
library(lubridate)

#CLEAN BIKE DATA 

#Remove X column 
df <- raw_df[,-1]

df <- df |>
  
  #Convert to start and end station to character values 
  mutate(
    start_station = as.character(start_station),
    end_station = as.character(end_station)) |>
  pivot_longer(cols = c(start_station,end_station,start_time,end_time), 
               names_to = c("inventory_change", ".value"), names_sep = "_")

#clean data

#if station is R, get rid of rows because not actually station 

df <- df |>
  filter(station != "R")

#convert all start and end time to +1 and -1 

df <- df |>
  mutate(inventory_change = case_when( 
    inventory_change == "start" ~ -1L,
    inventory_change == "end" ~ +1L))

#change event type None --> R and Subscriber & One-Time --> C

event_table <- df |>
  mutate(event_type = case_when(
    customer_type == "None" ~ "R",
    customer_type == "One-time" ~ "C",
    customer_type == "Subscriber" ~ "C"
  ))

#Change column ordering to match above format
event_table <- event_table |>
  select(station, time, inventory_change, event_type)

#Create event table
#identify the unique stations and create a vector for this  
station_id <- unique(event_table$station)

#Determine how long the current_inventory has to be 
length(unique(event_table$station))

#Set all stations equal to initial value of 0 bikes. 
current_inventory_tracker <- rep(0L, length(station_id))

#Attach the 0 values to the station ids vector we created above
names(current_inventory_tracker) <- station_id

#Create loop 

for (i in 1:nrow(event_table)) {
  sn <- event_table$station[i] #index to select the station name 
  #add value of bike to station 
  event_table$current_inventory[i] <- event_table$inventory_change[i] + 
    current_inventory_tracker[sn] 
  #update cumulative inventory to match current 
  current_inventory_tracker[sn] <- event_table$current_inventory[i]
}

#FIND ALPHA (Per Dr. Paul New Directions)

#Ensure that time is in POSIXct format
event_table$time <- as.POSIXct(event_table$time)

#Group by station and arrange by time 
event_table <- event_table |>
  arrange(time, .by_group = TRUE) 

#Find time for each station with inventory 0 and then divide by total time 
#which is 6 days or 6*24*60*60 seconds (518400 seconds)

alpha <- function(current_inventory, time) {
  #Set total to zero to start
  total <- 0
  #To prevent overwriting of loop need to create no_bikes to track when station 
  #has no bikes
  no_bikes <- FALSE
  for (i in 1:length(current_inventory)) {
    if (current_inventory[i] == 0 & no_bikes == FALSE) {
      start_nobike <- time[i]
      #change no_bikes to true to ensure isn't overwritten 
      no_bikes <- TRUE
    } else if (current_inventory[i] > 0 & no_bikes == TRUE) {
      total <- total + (time[i] - start_nobike)
      no_bikes <- FALSE
    }
  }
  
  #Catches the rest of the minutes if no_bikes == TRUE until midnight on 22  
  if (no_bikes) {
    total <- total + as.POSIXct("2022-02-23 00:00:00", 
                                tz = tz(time)) - start_nobike
  }
  #return percentage of the minutes with at least one bike
  return(as.numeric(1 - (total/518400)))
}

#Find alpha values by applying the alpha function
alpha_table <- event_table |>
  group_by(station) |>
  summarize(alpha = alpha(current_inventory, time),.groups = "drop")

#DISREAGRD (old attampt to find alpha old directions) 

#Add column with just the hour 

#event_table$hour <- hour(as.POSIXct(event_table$time))

#Create column with just the date and time 
#event_table$time <- as.POSIXct(event_table$time)

# set timezone equal to timezone used in column 
#event_table$date <- as.Date(event_table$time, tz = tz(event_table$time))
#event_table$minutes <- format(event_table$time, format = "%M.%S")
#event_table$minutes <- as.numeric(event_table$minutes)

#Create loop function that can be used to calculate minutes below 0 

#alpha <- function(current_inventory, minutes) {
  #Set total to zero to start
 # total <- 0
  #To prevent overwriting of loop need to create no_bikes to track when station has no bikes
  #no_bikes <- FALSE
  #For loop iterates automatically so will keep checking if current_inventory dips below 0
 # for (i in 1:length(current_inventory)) {
 #   if (current_inventory[i] == 0 & no_bikes == FALSE) {
  #    start_nobike <- minutes[i]
      #change no_bikes to true to ensure isn't overwritten 
   #   no_bikes <- TRUE
      #check, would I get a different answer if it was > 0 vs. == 1?
    #} else if (current_inventory[i] > 0 & no_bikes == TRUE) {
    #  total <- total + (minutes[i] - start_nobike)
   #   no_bikes <- FALSE
 #   }
#  }
  
  #Catches the rest of the minutes if there no_bikes == TRUE until 60 min mark. 
#  if (no_bikes) {
#    total <- total + (60 - start_nobike)
#  }
  #return the percentage of the minutes with at least one bike 
 # return(1 - (total/60))
#}

#Use alpha to create summary table 

#alpha_table <- event_table |>
 # group_by(station, date, hour) |>
  #summarize(alpha = alpha(current_inventory, minutes),.groups = "drop")

#Average all the alphas across days to get on entry per hour per station 

#alpha_table <- alpha_table |>
 # group_by(station, hour) |>
 # summarize(alpha = sum(alpha)/n())

#Create an empty data frame to merge with alpha table to have complete hours 

#Station id is all unique stations 

#station <- station_id
#hour <- c(0:23)

#Create grid of all possible hours for each station 

#alpha_empty <- expand_grid(station = station, hour = hour)

#Use a left merge to match 

#alpha_table <- left_join(alpha_empty, alpha_table, 
                        #  by = c("station" = "station", "hour" = "hour"))

#Replace all NA values with 0 -- NAs aren't necessarily 0 confused here!

#alpha_table <- alpha_table |>
#  mutate(alpha = 
         #  ifelse(is.na(alpha),0,alpha))

#FIND X HAT

#limit raw_df to only customer trips 
df4 <- raw_df[!(raw_df$start_station == "R"),]

#Check if there is a difference between start and end times -- there is none

df4$time_date_match <- df4$start_time == df4$end_time
sum(df4$time_date_match == FALSE)

#Convert start_time to hour and station_id to character 
df4$start_hour <- hour(df4$start_time)
df4$start_station <- as.character(df4$start_station)

xhat <- df4|>
  group_by(start_station, end_station, start_hour)|>
  #divide by six complete observation days 
  summarize(xhat = n()/6, .groups = "drop")

#Expand the grid using existing combinations to show missing hours 

xhat <- xhat |>
  #complete finds every combination of start_hour and the combined start_station
  #and end_station 
  complete(nesting(start_station, end_station), start_hour = 0:23) |>
  #Because xhat is the number of observed customer departures replace NAs w/ 0
  mutate(xhat = ifelse(is.na(xhat), 0L, xhat))


#FIND MU

#Merge xhat and alpha data tables to get unified table 

combined <- left_join(xhat,alpha_table, by = join_by(start_station == station))

#Calculate Mu (rate) by dividing across 

combined$mu <- combined$xhat / combined$alpha

#If availability is 0 change Mu to NA 

combined <- combined |>
  mutate(mu = ifelse(alpha == 0, NA_real_, mu))
  
#Rename columns 

combined <- combined |>
  rename("origin" =  "start_station",
         "hour" = "start_hour",
         "destination" = "end_station",
         "observed_rate" = "xhat",
         "avalibility" = "alpha",
         "rate" = "mu")

#Create function that filters combined data frame

#' Estimated bike user request rate  
#' 
#' @description Creates a data frame between given times showing the average 
#' request rate for each origin and destination pair. 
#'
#' @param start_window the starting time to estimate in H:M:S format 
#' @param end_window the ending time to estimate in H:M:S format 
#' 
#' @return data frame 

estimate_arrival_rate <- function(window_start, 
                                  window_end) {
  
#Create stops 
  #strptime mandates format of input to specification 
  if(is.na(strptime(window_start, format = "%H:%M:%S")) || 
     is.na(strptime(window_end, format = "%H:%M:%S"))) {
    stop("window_start and window_end must be valid times in the H:M:S format")
  }
  
 #Convert inputs (H:M:S) to lubridate hms to use hour() later
  window_start <- hms(window_start)
  window_end <- hms(window_end)
 
  #Filter the function 
  combined <- combined |>
    mutate( 
  #Ensure hour and origin are numeric 
      hour = as.numeric(hour),
      origin = as.numeric(origin)) |>
      #filer hour by the assigned hour on the dates 
    filter(hour >= hour(window_start),
           hour <= hour(window_end)) |>
    select(hour, 
           origin, 
           destination, 
           observed_rate, 
           avalibility, 
           rate) |>
    arrange(hour, origin)
    #return data frame 
  return(combined)
}

#Combined doesn't have any availability equal to 0. Therefore, no NAs are added
#to the function 
sum(combined$avalibility == 0)

#Example call 

output1 <- estimate_arrival_rate("5:2:3", "6:2:4")
output1


#' Function #2: Simulate NHPP arrivals
#' 
#' @description Simulates customer requests during half-open interval 
#' 
#' @param rates Data frame of origin-destination pairs and corresponding 
#' availability and hourly rates
#' @param start_time Customer start time
#' @param end_time Customer end time
#' 
#' @result Data frame of sorted arrival times

simulate_arrivals<- function(rates, start_time, end_time) {
  arrivals <- data.frame(origin = rates$origin[FALSE], 
                         destination = rates$destination[FALSE], 
                         arrival_time = start_time[FALSE])

    combined <- rates

  # Group to get origin and destination pairs 
  origin_dest_pairs <- rates %>%
    group_by(origin, destination) %>% 

    #Split each pair into distinct data frames  
    group_split()
  
  ## looping through pairs, find max, generating candidate times adjusted by 
  ## exponential wait times
  for (vec in origin_dest_pairs) {
    M <- max(vec$rate)
    
    if (M == 0) {
      next
    }
    
    ## making empty candidates, adjusting candidate times by wait times
    candidate_times_list <- list()
    candidate_time <- start_time
    
    while (candidate_time < end_time) {
      wait_time <- rexp(n = 1, rate = M)
      wait_time_secs <- as.difftime((wait_time * 3600), unit = "secs")
      
      candidate_time <- candidate_time + wait_time_secs
      
      if (candidate_time < end_time) {
        candidate_times_list <- append(candidate_times_list, list(candidate_time))
      }
    }

    ## comparing candidates to acceptance probabilities
    for (u in candidate_times_list) {
      candidate_hour <- hour(u)
      hourly_rate <- vec$rate[vec$hour == candidate_hour]
      
      acceptance_probability <- hourly_rate / M
      UU <- runif(1, min = 0, max = 1) 
      
      if (UU < acceptance_probability) {
        arrivals <- rbind(arrivals, data.frame(origin = vec$origin[1], 
                                               destination = vec$destination[1], 
                                               arrival_time = u))
      }
    }
  }
  
  arrivals <- arrivals[order(arrivals$arrival_time), ]
  return(arrivals)
}

output2 <- simulate_arrivals(combined, as.POSIXct("2026-11-21 14:05:00"), 
                  as.POSIXct("2026-11-21 16:55:00")) 


## ATTEMPT 2
# 
# arrivals <- data.frame()
# 
# combined <- combined %>%
#   group_by(start_date, start_hour, origin, destination) %>%    # may need to change grouping, add hour+date?
#   mutate(M = max(rate))    # should i use summarize?
# 
# candidate_times_list <- list()
# 
# start_time_test <- 300
# end_time_test <- 350
# combined$candidate_time <- start_time_test
# 
# for (i in 1:nrow(combined)) {
#   wait_time <- rexp(n = 1, rate = combined$M[i])
# 
#   ## need to convert candidate time & end time to seconds
#   combined$candidate_time <- combined$candidate_time + wait_time
# 
#   if (combined$candidate_time[i] >= end_time_test) {
#     break
#   }
# 
#   candidate_times_list <- append(candidate_times_list, combined$candidate_time)
#   
#   for (u in candidate_times_list) {
#     candidate_hour <- hour(u)
#     hourly_rate <- combined$rate[combined$hour == candidate_hour]
#     
#     acceptance_probability <- hourly_rate / combined$M[i]
#     UU <- runif(1, min = 0, max = 1)
#     
#     if (UU < acceptance_probability) {
#       arrivals <- rbind(arrivals, data.frame(origin = rates$origin, 
#                                              destination = rates$destination, 
#                                              arrival_time = u))
#     }
#     
#   }
#   
# }


## ATTEMPTED PSEUDOCODE FOR PROBLEM 6
## still need to add an outer for loop to loop through combined table

# simulate_arrivals(rates, start_time, end_time) {
#   arrivals <- table()
#   max_rate <= combined_table$rate[1]
#   
#   ## METHOD 2 (for finding max rate per pair):
#   combined_table %>%
#     group_by(ORIGIN, DESTINATION) %>%    # may need to change grouping, add hour+date?
#     mutate(M = max(RATE))    # should i use summarize?
#   
#   candidate_times_list <- list()
#   
#   combined_table$candidate_time <- start_time
#   
#   for (i in nrow(combined_table)) {
#     wait_time <- rexp(1, n = M)
#     
#     ## need to convert candidate time & end time to seconds
#     candidate_time <- candidate_time + wait_time
#     
#     if (candidate_time >= end_time) {
#       break
#     }
#     
#     candidate_times_list <- append(candidate_times_list, candidate_time)
#   }
#   
#   for (u in candidate_times_list) {
#     candidate_hour <- hour(candidate_times_list[u])
#     hourly_rate <- combined_table$rate[combined_table$hour == candidate_hour]
#     
#     acceptance_probability <- hourly_rate / M
#     UU <- Uniform(0,1)
#     
#     if (UU < acceptance_probability) {
#       arrivals <- rbind(origin, destination, u)
#     }
#     
#   }

