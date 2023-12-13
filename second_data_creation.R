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

library(lubridate)
library(dplyr)

# Function to generate random data for a given number of days
generate_random_data <- function(days) {
  data <- data.frame(
    date = rep(seq(Sys.time(), by = "-1 day", length.out = days), each = nrow(depot_data)),
    depot_id = rep(depot_data$depot_id, each = days),
    vehicle_id = rep(depot_data$vehicle_id, each = days),
    store_id = rep(depot_data$store_id, each = days)
  )
  # Add additional columns with simulated data (you can customize this part)
  data$quantity_sold <- rpois(nrow(data), lambda = 20)
  data$revenue <- data$quantity_sold * runif(nrow(data), min = 10, max = 50)
  return(data)
}

# Set the number of days to simulate
num_days <- 10

# Generate random data for 10 days for each store
simulated_data <- generate_random_data(num_days)

# Print the first 15 rows of the simulated data
head(simulated_data, 15)

