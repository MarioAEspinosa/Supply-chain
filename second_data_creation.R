library(lubridate)
library(dplyr)
library(tidyr)
library(mapsapi)

# api key
key_api <- "hiden api"

# seed
set.seed(8956)

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
    date = rep(seq(Sys.time(), by = "-1 day", length.out = days * nrow(depot_data)), each = nrow(depot_data)),
    depot_id = rep(depot_data$depot_id, each = days),
    vehicle_id = rep(depot_data$vehicle_id, each = days),
    store_id = rep(depot_data$store_id, each = days)
  )
  
  # Add random hours to the date column for each entry
  data$date <- data$date + runif(nrow(data), 0, 24) * 3600  # 3600 seconds in an hour
  
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
  destination_lat <- lat + distances * sin(angles)
  destination_lon <- lon + distances * cos(angles)
  
  return(data.frame(destination_lat, destination_lon))
}


# Specify the number of points and maximum distance in kilometers
num_points <- 100
max_distance_km <- 50  

# Apply the function to generate random coordinates for each row
joined_data <- joined_data %>%
  group_by(origin_lat, origin_lon) %>%
  mutate(random_coords = list(generate_random_coordinates(origin_lat[1], origin_lon[1], num_points, max_distance_km))) %>%
  unnest(cols = c(random_coords)) %>%
  ungroup()

# Print the updated data frame 
head(joined_data)


# Filter depots and stores separately
depots_data <- joined_data[joined_data$l]


# Extract unique coordinates from joined_data
unique_origins <- unique(joined_data[, c("origin_lat", "origin_lon")])
unique_destinations <- unique(joined_data[, c("destination_lat", "destination_lon")])

# Combine unique coordinates into a matrix for origins and destinations
all_coordinates <- cbind(
  c(unique_origins$origin_lon, unique_destinations$destination_lon),
  c(unique_origins$origin_lat, unique_destinations$destination_lat)
)

# Initialize an empty list to store geocoding information
geocode_responses <- list()

# Iterate through each coordinate pair and perform geocoding
for (i in seq_len(nrow(all_coordinates))) {
  # Create a string in the format "latitude,longitude"
  coordinate_str <- paste(all_coordinates[i, ], collapse = ",")
  
  # Query the Geocoding API
  geocode_response <- mp_geocode(
    addresses = coordinate_str,
    key = key_api,
    quiet = TRUE
  )
  
  # Append the geocoding response to the list
  geocode_responses[[i]] <- geocode_response
}

