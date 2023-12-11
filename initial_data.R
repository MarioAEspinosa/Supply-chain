library(dplyr)
library(googleway)

# Set seed for reproducibility
set.seed(123)

# Function to generate random dates within a given range
generate_random_date <- function(n) {
  start_date <- as.Date("2020-01-01")
  end_date <- as.Date("2023-12-31")
  random_dates <- sample(seq(start_date, end_date, by="1 day"), n, replace=TRUE)
  return(random_dates)
}


# Function to get location name from Google Maps API based on latitude and longitude
get_location_name <- function(lat, lon) {
  # Replace with your Google Maps API key
  api_key <- "GOOGLE_MAPS_API_KEY_HERE"
  
  # Make the API call
  result <- googleway::revgeocode(c(lon, lat), key = api_key)
  
  # Extract the location name (you may need to adapt this based on the actual structure of the API response)
  location_name <- result$results$address_components$formatted_address
  
  return(location_name)
}


# Function to generate random latitude and longitude within a state
generate_random_lat_lon <- function(state) {
  # Replace this with your logic to generate random lat/lon in the specified state
  center_lat_lon <- c(38.0, -97.0)  # Example centroid for the United States
  random_lat <- runif(1, center_lat_lon[1] - 1, center_lat_lon[1] + 1)
  random_lon <- runif(1, center_lat_lon[2] - 1, center_lat_lon[2] + 1)
  return(c(random_lat, random_lon))
}


# Function to estimate transportation distance (replace with actual calculation)
estimate_distance <- function(Origin_lat_lon, Destination_lat_lon) {
  googleway::google_distance_matrix(origins = Origin_lat_lon, destinations = Destination_lat_lon)$rows$elements$distance$value
  return(runif(1, 50, 500))  # Example random distance between 50 and 500 km
}

# Function to generate fake data for 10,000 rows
generate_fake_data <- function() {
  tibble(
    BookingID = 1:10000,
    Booking_date = generate_random_date(10000),
    vehicle_no = sample(paste0(sample(letters, 100, replace=TRUE), sample(1:999, 100, replace=TRUE)), 10000, replace=TRUE),
    Origin_location = sapply(1:10000, function(x) get_location_name(new_df$Org_lat_lon[x, 1], new_df$Origin_lat_lon[x, 2])),
    Destination_location = sapply(1:10000, function(x) get_location_name(new_df$Dest_lat_lon[x, 1], new_df$Destination_lat_lon[x, 2])),
    Origin_lat_lon = replicate(10000, generate_random_lat_lon("Example State")),  # Replace "Example State" with the actual state
    Destination_lat_lon = replicate(10000, generate_random_lat_lon("Example State")),
    Planned_ETA = NA, # NA for now, the prediction will be here latter 
    trip_start_date = Booking_date,
    Curr_lat_lon = replicate(10000, c(runif(1, Origin_lat_lon[1], Destination_lat_lon[1]), runif(1, Origin_lat_lon[2], Destination_lat_lon[2]))),
    trip_end_date = NA, # NA for now, may be useful latter  
    Transportation_distance_in_km = mapply(function(org, dest) estimate_distance(org, dest), new_df$Origin_lat_lon, new_df$Destination_lat_lon),
    custumerID = sample(1:100, 10000, replace=TRUE)
  )
}

# Generate fake data
new_df <- generate_fake_data()