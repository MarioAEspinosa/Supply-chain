library(lubridate)
library(dplyr)
library(tidyr)

# Creating a dataset with depots using letters from A to J
depot_letters <- LETTERS[1:10]  # A to J

# Creating a data frame with depot letters
depot_data <- data.frame(Depot_Letter = rep(depot_letters, each = 100))

# Creating a vector of vehicle numbers for each depot
vehicle_numbers <- rep(1:10, each = 10)

# Adding columns for vehicle numbers and store IDs in the data frame
depot_data$Vehicle <- paste0(depot_data$Depot_Letter, vehicle_numbers)

# Creating unique store IDs with the format "DepotLetterVehicleNumberOwnLetter"
store_letters <- rep(letters[1:10], each = 1)
store_letters_group <- rep(store_letters, each = 1)

# concatenate the characters from Vehicle and store_letters_group
depot_data$Store_ID <- paste0(depot_data$Depot_Letter, depot_data$Vehicle, store_letters_group)


# Renaming columns in the data frame
colnames(depot_data) <- c("depot_id", "vehicle_id", "store_id")

# Displaying the resulting dataset
head(depot_data, 15)


# Function to generate random data for a given number of days
generate_random_data <- function(days) {
  data <- data.frame(
    date = rep(seq(Sys.time(), by = "-1 day", length.out = days), each = nrow(depot_data)),
    depot_id = rep(depot_data$depot_id, each = days),
    vehicle_id = rep(depot_data$vehicle_id, each = days),
    store_id = rep(depot_data$store_id, each = days)
  )
  return(data)
}

# Set the number of days to simulate
num_days <- 10

# Generate random data for 10 days for each store
simulated_data <- generate_random_data(num_days)

# Print the first 15 rows of the simulated data
head(simulated_data, 15)

# Generate coordinates and store them in another df
coord_storage <- as.vector.data.frame(depot_letters)

# api key
key_api <- "API HERE"



# Define a function to generate random locations within a bounding box
generate_random_locations <- function(n, bbox) {
  lon <- runif(n, bbox[1], bbox[2])
  lat <- runif(n, bbox[3], bbox[4])
  data.frame(origin_lat = lat, origin_lon = lon)
}

# Define bounding boxes for California, Arizona, and New Mexico
california_bbox <- c(-124.4096, -114.1308, 32.5341, 35.8086)
arizona_bbox <- c(-114.8183, -109.0452, 31.3322, 37.0043)
new_mexico_bbox <- c(-109.0452, -103.0022, 31.3322, 37.0004)

# Generate 10 random locations for each state
california_locations <- generate_random_locations(4, california_bbox)
arizona_locations <- generate_random_locations(3, arizona_bbox)
new_mexico_locations <- generate_random_locations(3, new_mexico_bbox)

# Combine the generated locations
all_locations <- rbind(california_locations, arizona_locations, new_mexico_locations)
coord_storage

# Display the generated locations
all_locations

# Create a new column depot_id in 'all_locations'
all_locations$depot_id <- rep(coord_storage, each = 1)

# Join the data frames based on 'depot_id'
joined_data <- simulated_data %>%
  left_join(all_locations, by = "depot_id")

# Print the first 15 rows of the joined data
head(joined_data, 15)
# now we can make the store lon and lat using the depot lat and lon 


# Function to generate random coordinates within a certain radius
generate_random_coordinates <- function(lat, lon, num_points, max_distance_km) {
  # Generate random distances within the specified radius
  distances <- runif(num_points, 0, max_distance_km)
  
  # Generate random angles
  angles <- runif(num_points, 0, 2 * pi)
  
  # Calculate new coordinates based on the random distances and angles
  new_lat <- lat + distances * sin(angles)
  new_lon <- lon + distances * cos(angles)
  
  return(data.frame(destination_lat = new_lat, destination_lon = new_lon))
}

# Specify the number of points and maximum distance in kilometers
num_points <- 100
max_distance_km <- 50  

# Apply the function to generate random coordinates for each row
joined_data <- joined_data %>%
  group_by(origin_lat, origin_lon) %>%
  do(generate_random_coordinates(.$origin_lat[1], .$origin_lon[1], num_points, max_distance_km)) %>%
  ungroup() %>%
  tidyr::unnest_wider(new_coordinates)

# Print the updated data frame
print(head(joined_data, 15))



