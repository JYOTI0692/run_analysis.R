Coursera---Getting-and-Cleaning-Data-Week-4-Assignment
Submissions for the Week 4 Assignment of the Getting and Cleaning Course

This repository contains my submitted files for the Week 4 Assignment of the Coursera Getting and Cleaning Data Course. It contains the following files:

R Script: run_analysis.R
The prerequistite for running this script is that the working directory has already been set.

Executing the run_analysis.R script completes the following actions:

Downloads and unzips the source data files into the set working directory.
Creates vectors containing the features and activity label information which are used to add descriptors later in the script.
Reads into seperate tables the training set, the associated activity IDs and subject IDs.
Reads into seperate tables the test set, the associated activity IDs and subject IDs.
Merges the training and the test sets to create one data set.
Merges the activity ID and subject ID tables for the training and test sets.
Labels the variables in the columns of the merged dataset with descriptive names using the created features vector. The variables are now easier to interpret than the original 'V_' descriptors.
Add two 2 columns to the merged dataset so an activity ID and subject ID are assigned to each row. This links the recorded measurements to the invidividual subject and the activity they were undertaking.
Extracts only the variables relating to the mean and standard deviation values for each measurement. The measurements have been selected where suffixed with mean() or std() which are the mean and standard deivation measurements for the signals.
Uses descriptive activity names to label the activities in the data set rather than the original activity number. This makes the data easier to interpret. The newly added columns are ordered so they are columns 1 and 2 in the data set.
Creates 'wide' tidy data set called TidyData which contains a single row for each activity and subject combination and a single column for each variable mean value. The data set is ordered by subject ID in ascending order.
Creates a text file of TidyData so it can be exported and uploaded onto GitHub.
CodeBook.md
A code book that describes the variables, the data, and transformations used to tidy the data.

TidyData
A text file containing the tidied data set. In order to view the data in R please use the following script: read.table("TidyData.txt", header=TRUE)
