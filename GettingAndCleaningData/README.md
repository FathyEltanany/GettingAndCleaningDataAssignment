**Getting And Cleaning Data Code Book**
-------------
run.analysis.R Function :-
1) Read X_train.txt, y_train.txt and subject_train.txt from the train folder and store them in trainData1, trainData2 and trainSubject.
2)Read X_test.txt, y_test.txt and subject_test.txt from the test folder and store them in testData1, testData2 and testsubject.
3)Combine testData1 to trainData1 to generate a 10299x561 data frame, and combine testData2 to trainData2 to generate a 10299x1 data frame, and combine testSubject to trainSubject to generate a 10299x1 data frame.
4)Read the features.txt file and store the data in a variable called features, and only extract the measurements on the mean and standard deviation.
5)Clean the column names by removing the "()" and "-" symbols in the names, as well as make the first letter of "mean" and "std" a capital letter "M" and "S".
6)Clean the activity names in the second column of activity. We first make all names to lower cases then If the name has "underscore" between letters, it is removed and capitalize the letter after the "underscore", then transform the values of joinData2 according to the activity dataframe.
7)Combine the joinSubject, joinData2 and joinData1 by column to get a new cleaned 10299x68 data frame named cleanedData.
8)From the data set  create a second, independent tidy data set with the average of each variable for each activity and each subject in a new text file named "AvgData"