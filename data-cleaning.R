# Load required packages
library(tidyverse)

# Define constants
START_YEAR <- 1980
END_YEAR <- 2022
TRAIN_RATIO <- 0.8
ROOT_PATH <- "Your-local-path-here" # modify this
RAW_PATH <- file.path(ROOT_PATH, "datasets", "raw")
PROCESSED_PATH <- file.path(ROOT_PATH, "datasets", "processed")

# Helper function to load and process data
load_and_process_data <- function(year) {
  file_path <- file.path(RAW_PATH, paste0("daily_LEAD_", year, ".csv"))
  
  read.csv(file_path) %>%
    filter(Sample.Duration == "24 HOUR") %>%
    rename(Date = Date.Local, Value = Arithmetic.Mean) %>%
    mutate(Date = lubridate::as_date(Date)) %>%
    select(Longitude, Latitude, Date, Value)
}

# Load and process data
data <- START_YEAR:END_YEAR %>%
  map_dfr(load_and_process_data)

# Split data into train and test sets
train_indices <- sample(seq_len(nrow(data)), 
                        size = floor(TRAIN_RATIO * nrow(data)))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Add ID column
train_data <- train_data %>%
  mutate(ID = row_number()) %>%
  select(ID, everything())

test_data <- test_data %>%
  mutate(ID = row_number()) %>%
  select(ID, everything())

# Write train and test data to csv
write.csv(train_data, file.path(PROCESSED_PATH, "train.csv"), 
          row.names = FALSE)
write.csv(test_data %>% select(-Value), file.path(PROCESSED_PATH, "test.csv"), 
          row.names = FALSE)

# Create sample_submission and solution files
sample_submission <- test_data %>%
  transmute(ID, Value = runif(n(), 0, 10))

solution <- test_data %>%
  select(ID, Value) %>%
  mutate(Usage = sample(c("Public", "Private"), size = n(), replace = TRUE))

# Write sample_submission and solution to csv
write.csv(sample_submission, file.path(PROCESSED_PATH, "sample_submission.csv"), 
          row.names = FALSE)
write.csv(solution, file.path(PROCESSED_PATH, "solution.csv"), 
          row.names = FALSE)
