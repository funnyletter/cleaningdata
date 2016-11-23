library(dplyr)
library(tidyr)

data_dir <- 'UCI HAR Dataset'

# A function that takes the path to x, y, and subject data and returns a cleaned data frame of that
# data.
data_ingester <- function(x_path, y_path, subject_path) {
  # Read x and y data
  x <- read.table(x_path, stringsAsFactors = FALSE)
  y <- read.table(y_path, stringsAsFactors = FALSE)
  
  # Read in files containing subject and label data, and give them descriptive column names
  subject <- read.table(subject_path, stringsAsFactors = FALSE)
  colnames(subject) <- "subject"
  labels <- read.table('UCI HAR Dataset/activity_labels.txt', stringsAsFactors = FALSE)
  colnames(labels) <- c('position_id', 'activity')
  
  # Get feature names and set these to column names for x
  features <- read.table('UCI HAR Dataset/features.txt', stringsAsFactors = FALSE)
  colnames(x) <- features$V2
  
  # Select columns in x only for mean and standard deviation; exclude all other measurements
  x <- x[, grep("(std|mean)\\(", colnames(x))]
  colnames(y) <- "position_id"
  
  # Combine x with y data using cbind
  combo <- cbind(subject, x) %>%
    cbind(y) %>%
    # use position_id data to add descriptive labels to activities
    inner_join(labels, by = "position_id") %>%
    # Convert data from wide to long form
    gather(measurementType, value, `tBodyAcc-mean()-X`:`fBodyBodyGyroJerkMag-std()`) %>%
    # Remove position_id column, since it's no longer necessary now that we have descriptive
    # position labels
    select(-position_id)
  combo$activity <- tolower(combo$activity)
  return(combo)
}

# A function that takes a data path and creates a combined tidy data set containing both train and 
# test data. This function takes a few seconds because it has to read some large text files.
combine_train_test <- function(data_path) {
  train <- data_ingester(paste(data_path, "/train/x_train.txt", sep = ""),
                         paste(data_path, "/train/y_train.txt", sep = ""),
                         paste(data_path, "/train/subject_train.txt", sep = ""))
  
  test <- data_ingester(paste(data_path, "/test/x_test.txt", sep = ""),
                        paste(data_path, "/test/y_test.txt", sep = ""),
                        paste(data_path, "/test/subject_test.txt", sep = ""))
  
  all_data <- rbind(train, test)
  
  all_data$measurement_type <- factor(all_data$measurementType)
  all_data$position <- factor(all_data$activity)
  return(all_data)
}

har_data <- combine_train_test(data_dir)

# Generate separate tidy data set containing means of each measurement for each subject and activity.
data_means <- har_data %>%
  group_by(subject, activity, measurementType) %>%
  summarize(average = mean(value))

# Write set of means to CSV
write.table(data_means, 'cleaningdata_means.txt', row.names = FALSE)
