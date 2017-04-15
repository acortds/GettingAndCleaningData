---
Title: "Getting and Cleaning Data"
Author: "Adrian Cortinas"
---

# Description
This repository is for John Hopkins' Getting and Cleaning Data Course project assignment files. 

For this assignment we have to create an R script that performs the following:
1. Merge two or more data sets to create one data set.
2. Extract only the measurements on the mean and standard deviation for each measurement.
3. Use descriptive activity names to name the activities in the data set
4. Appropriately label the data set with descriptive variable names.
5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Requirements
The script needs dplyr and tidyr libraries installed in order for it to work. You can install them by executing this command:

```r
install.packages("tidyr","dplyr")
```

You may need to restart R or R Studio after this is done.

# Contents
| File Name | Description                                         |
|-----------|-----------------------------------------------------|
| README.md | This file.|
| CodeBook.md | Description of all elements used in the R script.|
| run_analysis.R | R Script to run and perform the data analysis.|

# Downloaded Contents (during execution of the program)
| File Name | Description                                         |
|-----------|-----------------------------------------------------|
| projectiles FUCI HAR Dataset.zip | Data Sets|

# Generated Files
| File Name | Description                                         |
|-----------|-----------------------------------------------------|
| RawData.csv | All individual files merged into one single data set. |
| TidyData.csv | File with the average of each dimension (variable) for each activity and each subject. |

# Execution
run_analysis.R contains all the instructions to download, merge and produce aforementioned files. All you need to do is execute (source) run_analysis.R and it will create the two files, RawData.csv and TidyData.csv.

```r
source("path_to_file/run_analysis.R")
```
