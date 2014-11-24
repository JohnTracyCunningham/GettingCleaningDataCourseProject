## run_analysis.R
## John Tracy Cunningham
## 23 November 2014

## 1. Read in the eight data files.

activities <- read.table("activity_labels.txt", stringsAsFactors = FALSE)
features <- read.table("features.txt", stringsAsFactors = FALSE)
subject_test <- read.table("subject_test.txt", stringsAsFactors = FALSE)
x_test <- read.table("X_test.txt", stringsAsFactors = FALSE)
y_test <- read.table("y_test.txt", stringsAsFactors = FALSE)
subject_train <- read.table("subject_train.txt", stringsAsFactors = FALSE)
x_train <- read.table("X_train.txt", stringsAsFactors = FALSE)
y_train <- read.table("y_train.txt", stringsAsFactors = FALSE)

## 2. Remove the extraneous first columns from activities and features.

activities[, 1] <- NULL
features[, 1] <- NULL

## 3. Combine the training and test files for each of subject, x, and y.

subject <- rbind(subject_train, subject_test)
x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)

## 4. Modify the activities text fields by reducing to lower case.

activities[, 1] <- tolower(activities[, 1])

## 5. Replace the activity numbers in the "y" file with corresponding text from "activities".
## Do this by creating an empty column in "y", then do the substitution with a fore loop.
## Then eliminate the redundant first column in "y", leaving a column of text.

y$text <- ""
for(n in 1:6) y$text[y[, 1] == n] <- activities[n, 1]
y[, 1] <- NULL

## 6. Now combine subject, y, and x into the har data frame.

har <- cbind(subject, y, x)

## 7. Now attach column names to the har data frame.  Use "subject", "activity", and the
## features data frame.

colnames(har) <- c("subject", "activity", features[, 1])

## 8. Eliminate duplicate columns.

har <- har[, unique(colnames(har))]

## 9. Load the dplyr package and select only those columns whose names contain "subject",
## "activity", "mean", or "std".

library(dplyr)
har <- select(har, grep(c("subject|activity|mean|std"), colnames(har)))

## 10. Clean up the column names by (a) eliminating parentheses; (2) replacing hyphens with
## underscores; and (3) removing "Body", as it appears in all names.

colnames(har) <- gsub("\\()", "", colnames(har))
colnames(har) <- gsub("-", "_", colnames(har))
colnames(har) <- gsub("Body", "", colnames(har))

## 11. Sort the entire har data frame by subject and activity.  (Perhaps unnecessary.)

har <- arrange(har, subject, activity)

## 12. Load the data.table package and convert har to a data table.

library(data.table)
hardt <- as.data.table(har)

## 13. Finally, group hardt by subject and activity, and take the mean of each group.

har_final <- hardt[, lapply(.SD, mean), by = list(subject, activity)]

#14. Write har_final out to tidy_har.txt.

write.table(har_final, "tidy_har.txt", row.names = FALSE)
