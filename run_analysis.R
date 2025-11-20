run_analysis <- function() {
  data_dir <- "UCI HAR Dataset"
  if (!dir.exists(data_dir)) {
    stop("Directory 'UCI HAR Dataset' not found in working directory.")
  }
  
  features <- read.table(
    file.path(data_dir, "features.txt"),
    col.names = c("index", "feature")
  )
  
  activity_labels <- read.table(
    file.path(data_dir, "activity_labels.txt"),
    col.names = c("activityId", "activity")
  )
  
  subject_train <- read.table(
    file.path(data_dir, "train", "subject_train.txt"),
    col.names = "subject"
  )
  
  x_train <- read.table(
    file.path(data_dir, "train", "X_train.txt")
  )
  
  y_train <- read.table(
    file.path(data_dir, "train", "y_train.txt"),
    col.names = "activityId"
  )
  
  subject_test <- read.table(
    file.path(data_dir, "test", "subject_test.txt"),
    col.names = "subject"
  )
  
  x_test <- read.table(
    file.path(data_dir, "test", "X_test.txt")
  )
  
  y_test <- read.table(
    file.path(data_dir, "test", "y_test.txt"),
    col.names = "activityId"
  )
  
  colnames(x_train) <- features$feature
  colnames(x_test)  <- features$feature
  
  train <- cbind(subject_train, y_train, x_train)
  test  <- cbind(subject_test,  y_test,  x_test)
  
  all_data <- rbind(train, test)
  
  mean_std_cols <- grepl("mean\\(\\)|std\\(\\)", features$feature)
  
  cols_to_keep <- c(TRUE, TRUE, mean_std_cols)
  
  mean_std_data <- all_data[, cols_to_keep]
  
  mean_std_data <- merge(
    mean_std_data,
    activity_labels,
    by = "activityId",
    all.x = TRUE
  )
  
  mean_std_data <- mean_std_data[order(mean_std_data$subject, mean_std_data$activityId), ]
  mean_std_data$activityId <- NULL
  
  col_order <- c("subject", "activity", setdiff(names(mean_std_data), c("subject", "activity")))
  names(mean_std_data)[names(mean_std_data) == "activity"] <- "activity"
  mean_std_data <- mean_std_data[, col_order]
  
  names(mean_std_data) <- gsub("\\(\\)", "", names(mean_std_data))          # remove ()
  names(mean_std_data) <- gsub("-", "", names(mean_std_data))              # remove dashes
  names(mean_std_data) <- gsub("^t", "Time", names(mean_std_data))         # t -> Time
  names(mean_std_data) <- gsub("^f", "Frequency", names(mean_std_data))    # f -> Frequency
  names(mean_std_data) <- gsub("Acc", "Accelerometer", names(mean_std_data))
  names(mean_std_data) <- gsub("Gyro", "Gyroscope", names(mean_std_data))
  names(mean_std_data) <- gsub("Mag", "Magnitude", names(mean_std_data))
  names(mean_std_data) <- gsub("BodyBody", "Body", names(mean_std_data))
  
  tidy_data <- aggregate(
    . ~ subject + activity,
    data = mean_std_data,
    FUN = mean
  )
  
  tidy_data <- tidy_data[order(tidy_data$subject, tidy_data$activity), ]
  
  output_file <- "tidy_data.txt"
  write.table(tidy_data, file = output_file, row.name = FALSE)
  
  message("Tidy dataset written to: ", output_file)
  invisible(tidy_data)
}
