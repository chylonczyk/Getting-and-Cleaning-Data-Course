Getting and Cleaning Data Peer Assessment

Tasks:
You should create one R script called run_analysis.R that does the following.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names.
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Requirements:
1. R installed (RStudio can be beneficial).
2. The script needs two additional packages, data.table and reshape2 to operate. They are auto-installed using the package function.
3. Data downloaded and stored in the folder called "UCI HAR Dataset" in the working directory.

To run:
1. If it is not there, put the R scipt run_analysis.R into the folder UCI HAR Dataset.
2. Open the cript in R (Rstudio) and run it (in RStudio by clicking the source button).
3. The script should create a file "tidy_data.txt" in your folder containing a tidy data set with the average of each variable for each activity and each subject.  

