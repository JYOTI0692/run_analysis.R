Code Book
This is the code book for the TidyData set created when the run_analysis_R script is run

Data source
The source data for the project is the 'Human Activity Recognition Using Smartphones Data Set which was built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensors. Each person (subject) performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity measurements at a constant rate of 50Hz were captured.

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz.

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals).

The raw source data files can be found here: Data

Citation: Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. A Public Domain Dataset for Human Activity Recognition Using Smartphones. 21th European Symposium on Artificial Neural Networks, Computational Intelligence and Machine Learning, ESANN 2013. Bruges, Belgium 24-26 April 2013.

Transformations carried out
Once the raw data sets were downloaded and loaded into tables, the following transformations were carried out in order to generate the tidy data set.

1 - The training and test data sets were merged into one data set (source files = X_train and X_test). 2 - The activity reference tables for the training and test data sets were merged into a single table (source files = y_train and y_test).

3 - The subject reference tables for the training and test data sets were merged into a single table (source files = subject_train and subject_test).

4 - Descriptive labels detailing the measurement type were added to the column variables in the merged data set using the features file. The measurements are now clearly labelled in the datset rather than the orginial 'V_' descriptors used.

5 - The activity and subject reference tables created in transformations 2 and 3 were used to add an activty ID and subject ID to each row of the merged data set. The measurements were now linked to one of the 6 activities and one of the 30 subjects.

6 - Only measurements relating to mean and standard deviation calculations were extracted from the merged data set. These were identified where the measurement name had either mean() or std() contained in the description. The remaining columns were excluded.

7 - The activty IDs were replaced with the appropriate descriptors e.g. WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)

8 -A 'wide' tidy data set was created where there is a single row for each activty and subject combination and a single column for each selected measurement average (mean) value. The data set is ordered by subject in ascending order.

Data Set Variables
Generated fields
SubjectID (integer) - ID of subject performing the activity. Ranges from 1-30 (integer)

Activity (factor)- Activity the subject is performing. 6 possible values: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING.

Measurement variables
Each variable is the mean value of all measurements recorded for that variable for each subject and activity. The values are bounded between -1 and 1.

tBodyAcc-mean()-X (num)

tBodyAcc-mean()-Y (num)

tBodyAcc-mean()-Z (num)

tBodyAcc-std()-X (num)

tBodyAcc-std()-Y (num)

tBodyAcc-std()-Z (num)

tGravityAcc-mean()-X (num)

tGravityAcc-mean()-Y (num)

tGravityAcc-mean()-Z (num)

tGravityAcc-std()-X (num)

tGravityAcc-std()-Y (num)

tGravityAcc-std()-Z (num)

tBodyAccJerk-mean()-X (num)

tBodyAccJerk-mean()-Y (num)

tBodyAccJerk-mean()-Z (num)

tBodyAccJerk-std()-X (num)

tBodyAccJerk-std()-Y (num)

tBodyAccJerk-std()-Z (num)

tBodyGyro-mean()-X (num)

tBodyGyro-mean()-Y (num)

tBodyGyro-mean()-Z (num)

tBodyGyro-std()-X (num)

tBodyGyro-std()-Y (num)

tBodyGyro-std()-Z (num)

tBodyGyroJerk-mean()-X (num)

tBodyGyroJerk-mean()-Y (num)

tBodyGyroJerk-mean()-Z (num)

tBodyGyroJerk-std()-X (num)

tBodyGyroJerk-std()-Y (num)

tBodyGyroJerk-std()-Z (num)

tBodyAccMag-mean() (num)

tBodyAccMag-std() (num)

tGravityAccMag-mean() (num)

tGravityAccMag-std() (num)

tBodyAccJerkMag-mean() (num)

tBodyAccJerkMag-std() (num)

tBodyGyroMag-mean() (num)

tBodyGyroMag-std() (num)

tBodyGyroJerkMag-mean() (num)

tBodyGyroJerkMag-std() (num)

fBodyAcc-mean()-X (num)

fBodyAcc-mean()-Y (num)

fBodyAcc-mean()-Z (num)

fBodyAcc-std()-X (num)

fBodyAcc-std()-Y (num)

fBodyAcc-std()-Z (num)

fBodyAccJerk-mean()-X (num)

fBodyAccJerk-mean()-Y (num)

fBodyAccJerk-mean()-Z (num)

fBodyAccJerk-std()-X (num)

fBodyAccJerk-std()-Y (num)

fBodyAccJerk-std()-Z (num)

fBodyGyro-mean()-X (num)

fBodyGyro-mean()-Y (num)

fBodyGyro-mean()-Z (num)

fBodyGyro-std()-X (num)

fBodyGyro-std()-Y (num)

fBodyGyro-std()-Z (num)

fBodyAccMag-mean() (num)

fBodyAccMag-std() (num)

fBodyBodyAccJerkMag-mean() (num)

fBodyBodyAccJerkMag-std() (num)

fBodyBodyGyroMag-mean() (num)

fBodyBodyGyroMag-std() (num)

fBodyBodyGyroJerkMag-mean() (num)

fBodyBodyGyroJerkMag-std() (num)
