# bike_functions.R

#Function #1: Simulate NHPP arrivals


#LOAD DATA 
df <- read.csv("/Users/ianzmccollum/Desktop/course_files_export/Data/sample_bike.csv")

#CLEAN BIKE DATA 

#Remove X column 
df <- df[,-1]

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

#FIND X HAT 

#Filter by departures 
event_table <- event_table[event_table$inventory_change == -1L,]

#Delete the event_type column 
event_table <- event_table[, -4]

#Remove inventory change and rename to departures table 

departures_table <- event_table[, -3]



#Remove customer type 



#estimate_arrival_rates(data, window_start, window_end)

#Your function should return a table with columns origin, destination, hour (0--23), observed_rate, availability, and rate. 
#Calculate rate using the formula above. When availability is zero, return NA for rate: demand cannot be estimated from that group, even if its observed count is zero. 
#Document arguments and return values with roxygen-style comments. State how you handle any input data checks.

