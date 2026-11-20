# bike_functions.R

#Function #1: Simulate NHPP arrivals


#LOAD DATA 
<<<<<<< HEAD
raw_df <- read.csv("/Users/ianzmccollum/Desktop/course_files_export/Data/sample_bike.csv")
=======
df <- read.csv("~/STAT1560/Data_export/sample_bike.csv")
>>>>>>> 3e039a49e79a44a88a6dcc8a5a6e81dace19b8a8

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

#Clean data

#If start station is R, then get rid of these rows because nothing is taken away 
# from R. Then convert all inventory_change "start" to subtract and "end" to add. 

df <- df[!(df$inventory_change == "start" & df$station == "R"),]

#Convert all start and end time to +1 and -1 

df <- df |>
  mutate(inventory_change = case_when( 
    inventory_change == "start" ~ -1L,
    inventory_change == "end" ~ +1L))

#Change event type -- None --> R and Subscriber & One-Time --> C

event_table <- df |>
  mutate(event_type = case_when(
    customer_type == "None" ~ "R",
    customer_type == "One-time" ~ "C",
    customer_type == "Subscriber" ~ "C"
  ))

#Change column ordering to match above format
event_table <- event_table |>
  select(station, time, inventory_change, event_type)

#CREATE EVENT TABLE 
#identify the unique stations and create a vector for this  
station_id <- unique(event_table$station)

#Determine how long the current_inventory has to be 
length(unique(event_table$station))

#Set all stations equal to initial value of 0 bikes. 
current_inventory <- rep(0,22)

#Attach the 0 values to the station ids vector we created above
names(current_inventory) <- station_id
#Create loop 

for (i in 1:nrow(event_table)) {
  sn <- event_table$station[i] #index to select the station name 
  #add value of bike to station 
  event_table$current_inventory[i] <- event_table$inventory_change[i] + 
    current_inventory[sn] 
  #update cumulative inventory to match current 
  current_inventory[sn] <- event_table$current_inventory[i]
}

#FIND ALPHA

#Add column with just the hour 

event_table$hour <- hour(as.POSIXct(event_table$time))

#Create column with just the date and time 

event_table$time <- as.POSIXct(event_table$time)

# set timezone equal to timezone used in column 
event_table$date <- as.Date(event_table$time, tz = tz(event_table$time))
event_table$minutes <- format(event_table$time, format = "%M.%S")
event_table$minutes <- as.numeric(event_table$minutes)

  
#Create loop function that can be used to calculate minutes below 0 

alpha <- function(current_inventory, minutes) {
  #Set total to zero to start
  total <- 0
  #To prevent overwriting of loop need to create no_bikes to track when station has no bikes
  no_bikes <- FALSE
  #For loop iterates automatically so will keep checking if current_inventory dips below 0
  for (i in 1:length(current_inventory)) {
    if (current_inventory[i] == 0 & no_bikes == FALSE) {
      start_nobike <- minutes[i]
      #change no_bikes to true to ensure isn't overwritten 
      no_bikes <- TRUE
      #check, would I get a different answer if it was > 0 vs. == 1?
    } else if (current_inventory[i] == 1 & no_bikes == TRUE) {
      total <- total + (minutes[i] - start_nobike)
      no_bikes <- FALSE
    }
  }
  
  #Catches the rest of the minutes if there no_bikes == TRUE until 60 min mark. 
  if (no_bikes) {
    total <- total + (60 - start_nobike)
  }
  #return the percentage of the minutes with at least one bike 
  return(1 - (total/60))
}

#Use alpha to create summary table 

alpha_table <- event_table |>
  group_by(station, date, hour) |>
  summarize(alpha = alpha(current_inventory, minutes),.groups = "drop")



#FIND X HAT

#limit raw_df to only customer trips 
df4 <- raw_df[!(raw_df$start_station == "R"),]

#Check if there is a difference between start and end times -- there is none

df4$time_date_match <- df4$start_time == df4$end_time
sum(df4$time_date_match == FALSE)


#Separate start_time into date and time 

# set timezone equal to timezone used in column 
df4$start_date <- as.Date(df4$start_time, tz = tz(df4$start_time))
df4$start_hour <- hour(df4$start_time)

xhat <- df4|>
  group_by(start_date, start_hour, start_station, end_station)|>
  #divide by six complete observation days 
  summarize(xhat = n()/6, .groups = "drop")
#Checked the sum in summarize and the values make sense. 

#FIND MU

#Merge xhat and alpha data tables to get unified table for calculations

combined <- left_join(xhat,alpha_table, by = join_by(start_date == date, 
                                   start_station == station,
                                   start_hour == hour))

#Calculate Mu (rate) by dividing across 

combined$mu <- combined$xhat / combined$alpha
  
#Rename columns 

combined <- combined |>
  rename("orgin" =  "start_station", 
         "destination" = "end_station",
         "observed_rate" = "xhat",
         "avalibility" = "alpha",
         "rate" = "mu")

#Create wrapper function that simply filters the combined function!
Generalize that code into a function such as estimate_arrival_rates(data, window_start, window_end) that takes trip data and an observation window and returns the estimated hourly rates.

#convert time
event_table$time <- as.POSIXct(event_table$time)
event_table$hour <- hour(event_table$time)

event_table <- event_table %>%
  group_by(station)


#Your function should return a table with columns origin, destination, hour (0--23), observed_rate, availability, and rate. 
#Calculate rate using the formula above. When availability is zero, return NA for rate: demand cannot be estimated from that group, even if its observed count is zero. 
#Document arguments and return values with roxygen-style comments. State how you handle any input data checks.


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
#   
# }


