library(dplyr)

# read test data
test_subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
test_x_test <- read.table("./UCI HAR Dataset/test/X.txt")
test_y_test <- read.table("./UCI HAR Dataset/test/y.txt")

# read train data
train_subject_test <- read.table("./UCI HAR Dataset/train/subject_test.txt")
train_x_test <- read.table("./UCI HAR Dataset/train/X_test.txt")
train_y_test <- read.table("./UCI HAR Dataset/train/y_test.txt")

#step 1:Merges the training and the test sets to create one data set
subject_total <- rbind(test_subject_test, train_subject_test)
x_total <- rbind(test_x_test, train_x_test)
y_total <- rbind(test_y_test, train_y_test)

#step 2:Extracts only the measurements on the mean and standard deviation for each measurement
selected_var <- variable_names[grep("mean\\(\\)|std\\(\\)",variable_names[,2]),]
x_total <- x_total[,selected_var[,1]]

#step 3:Uses descriptive activity names to name the activities in the data set
colnames(y_total) <- "activity"
y_total$activitylabel <- factor(y_total$activity, labels = as.character(activity_labels[,2]))
activitylabel <- y_total[,-1]

#step 4:Appropriately labels the data set with descriptive variable names
colnames(x_total) <- variable_names[selected_var[,1],2]

#step 5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
colnames(subject_total) <- "subject"
total <- cbind(x_total, activitylabel, subject_total)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
write.table(total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)

