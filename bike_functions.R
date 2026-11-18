# bike_functions.R

#Function #1: 

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

head(event_table)

#Check if any station counts are negative 

sum(event_table$current_inventory < 0)
```

estimate_arrival_rates(data, window_start, window_end)

Your function should return a table with columns origin, destination, hour (0--23), observed_rate, availability, and rate. 
Calculate rate using the formula above. When availability is zero, return NA for rate: demand cannot be estimated from that group, even if its observed count is zero. 
Document arguments and return values with roxygen-style comments. State how you handle any input data checks.