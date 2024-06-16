library(dplyr)
library(readr)

# Define the function to import and merge CSV files
collate_data <- function(directory) {
    # Get the list of CSV files in the directory
    files <- list.files(path=directory, pattern = "\\.csv$", full.names = TRUE)

    # Function to read a CSV file with suppressed messages
    read_csv_suppress <- function(file) {
        suppressMessages(read_csv(file))
    }

    # Read and combine all CSV files into one data frame
    combined_data <- files %>%
        lapply(read_csv_suppress) %>%
        bind_rows()

    return(combined_data)
}



