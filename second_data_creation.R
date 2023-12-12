# Set seed for reproducibility
set.seed(123)

# Function to generate random dates within a given range
generate_random_date <- function(n) {
  start_date <- as.Date("2020-01-01")
  end_date <- as.Date("2023-12-31")
  random_dates <- sample(seq(start_date, end_date, by = "1 day"), n, replace = TRUE)
  return(random_dates)
}

# Function to generate fake data for 10,000 rows
generate_initial_fake_data <- function() {
  # Generate random dates
  booking_dates <- generate_random_date(10000)
  
  # Generate depot and vehicle details
  depot_ids <- rep(1:10, each = 10)
  vehicle_ids <- rep(1:100, each = 10)
  origin_depots <- rep(paste("Depot", 1:10), each = 10)
  origin_coords <- rep(NA, 10000)
  
  # Generate store details
  store_ids <- 1:1000
  store_coords <- matrix(runif(2000, 35, 40), ncol = 2)  # Random store coordinates
  
  # Create the initial fake dataset
  fake_data_set <- tibble(
    BookingID = 1:10000,
    Booking_date = booking_dates,
    vehicule_ID = rep(vehicle_ids, each = 10),
    Origin_Depot = origin_depots,
    Origin_Cord = origin_coords,
    Store_ID = rep(store_ids, each = 10),
    Store_Cord = rep(store_coords, each = 10),
    Planned_ETA = NA,
    Transport_Distance = NA,
    Trip_end_date = NA
  )
  
  return(fake_data_set)
}

# Generate initial fake dataset
fake_data_set <- generate_initial_fake_data()