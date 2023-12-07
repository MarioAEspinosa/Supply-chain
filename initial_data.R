library(readxl)
library(knitr)
library(lubridate)
library(dplyr)



# how will it be 
# Generate fake data for 10,000 rows
""" new_df <- tibble(
  BookingID =  BookingID: from 1 to 10,000
  Booking_date = random, date in range from 2020 to 2023 in order
  vehicle_no = random letter and int 100 unique
  Origin_location =  names taken from googleways using lan and lon
  Destination_location = name taken from googleways using lan and lon
  Org_lat_lon = random latitude and longitude from a place in a state
  Dest_lan_lon = random latitude and longitude from a place in the state
  Planned_ETA = left empty, calculationg this use the api
  trip_start_date = for now lets use the booking date, latter lets calculate the best date for the trip in the next few days or not, maybe set one or two days after the booking date
  Curr_lat_lon = fake gps tracking for the dashboard (lan lot in range from org_lan_lon to dest_lan_lon from now)
  trip_end_date = empty for now(more on this later), estimate the time using the api or maybe our own model 
  Transportation_distance_in_km = km from the origin_location to destination_location
  custumerID = custumerID = random int from 1 to 100
)"""



# Create a function to generate random dates within a given range
generate_random_date <- function(n) {
  start_date <- as.Date("2020-01-01")
  end_date <- as.Date("2023-12-31")
  random_dates <- sample(seq(start_date, end_date, by="1 day"), n, replace=TRUE)
  return(random_dates)
}

# Create a function to generate random vehicles 
generate_vehicle_numbers <- function(num_vehicles = 100) {
  # Create a vector of letters from 'A' to 'Z'
  letters_vector <- LETTERS
  
  # Sample 100 unique letters from the vector
  selected_letters <- sample(letters_vector, num_vehicles, replace = FALSE)
  
  # Generate random numbers for each letter
  selected_numbers <- sample(1:999, num_vehicles, replace = FALSE)
  
  # Combine letters and numbers to create vehicle numbers
  vehicle_numbers <- paste0(selected_letters, selected_numbers)
  
  return(vehicle_numbers)
}


# Generate fake data for 10,000 rows
df <- tibble(
  BookingID = 1:10000,
  Booking_date = generate_random_date(10000),
  vehicle_no = sample(generate_vehicle_numbers(), 10000, replace = TRUE),
  custumerID = sample(1000:9999, 10000, replace=TRUE)
)
head(df)





library(dplyr)
library(lubridate)
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

# Function to generate random latitude and longitude within a state
generate_random_lat_lon <- function(state) {
  # Replace this with your logic to generate random lat/lon in the specified state
  # For example, you can use the centroid of the state and add random values
  center_lat_lon <- c(38.0, -97.0)  # Example centroid for the United States
  random_lat <- runif(1, center_lat_lon[1] - 1, center_lat_lon[1] + 1)
  random_lon <- runif(1, center_lat_lon[2] - 1, center_lat_lon[2] + 1)
  return(c(random_lat, random_lon))
}

# Function to get location name from Google Maps API based on latitude and longitude
get_location_name <- function(lat, lon) {
  # Replace this with your logic to get location name from Google Maps API
  # Example: googleway::revgeocode(c(lon, lat))
  return("Example Location Name")
}

# Function to estimate transportation distance (replace with actual calculation)
estimate_distance <- function(org_lat_lon, dest_lat_lon) {
  # Replace this with your logic to estimate distance (e.g., Google Maps Distance Matrix API)
  # Example: googleway::google_distance_matrix(origins = org_lat_lon, destinations = dest_lat_lon)$rows$elements$distance$value
  return(runif(1, 50, 500))  # Example random distance between 50 and 500 km
}

# Function to generate fake data for 10,000 rows
generate_fake_data <- function() {
  tibble(
    BookingID = 1:10000,
    Booking_date = generate_random_date(10000),
    vehicle_no = sample(paste0(sample(letters, 100, replace=TRUE), sample(1:999, 100, replace=TRUE)), 10000, replace=TRUE),
    Origin_location = sapply(1:10000, function(x) get_location_name(new_df$Org_lat_lon[x, 1], new_df$Org_lat_lon[x, 2])),
    Destination_location = sapply(1:10000, function(x) get_location_name(new_df$Dest_lat_lon[x, 1], new_df$Dest_lat_lon[x, 2])),
    Org_lat_lon = replicate(10000, generate_random_lat_lon("Example State")),  # Replace "Example State" with the actual state
    Dest_lat_lon = replicate(10000, generate_random_lat_lon("Example State")),
    Planned_ETA = NA,
    trip_start_date = Booking_date,
    Curr_lat_lon = replicate(10000, c(runif(1, Org_lat_lon[1], Dest_lat_lon[1]), runif(1, Org_lat_lon[2], Dest_lat_lon[2]))),
    trip_end_date = NA,
    Transportation_distance_in_km = mapply(function(org, dest) estimate_distance(org, dest), new_df$Org_lat_lon, new_df$Dest_lat_lon),
    custumerID = sample(1:100, 10000, replace=TRUE)
  )
}

# Generate fake data
new_df <- generate_fake_data()

# Display the first few rows of the new dataframe
head(new_df)







