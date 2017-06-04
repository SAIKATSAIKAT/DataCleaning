#R Version : 3.3
# OS       : Windows 8.1

library(readr)
library(dplyr)

########## Loading the data into R  from working directory ##################
#Get working directory
working_dir <- getwd()

#derive the file paths for the input files

file_path_X_train <- paste(working_dir,"/X_train.txt", sep = "")
file_path_y_train <- paste(working_dir,"/y_train.txt", sep = "")
file_path_subject_train <- paste(working_dir,"/subject_train.txt", sep = "")

file_path_X_test <- paste(working_dir,"/X_test.txt", sep = "")
file_path_y_test <- paste(working_dir,"/y_test.txt", sep = "")
file_path_subject_test <- paste(working_dir,"/subject_test.txt", sep = "")

file_path_activity_labels <- paste(working_dir,"/activity_labels.txt", sep = "")
file_path_features <- paste(working_dir,"/features.txt", sep = "")

 #Training data

        # Load the training data into R
        X_train <- read_table(file_path_X_train,col_names = FALSE)
        
        y_train <- read_table(file_path_y_train,col_names = FALSE)
        
        subject_train <- read_table(file_path_subject_train,col_names = FALSE)
 
 #Test data        
               
        # Load the test data into R
        X_test <- read_table(file_path_X_test ,col_names = FALSE)
        
        y_test <- read_table(file_path_y_test,col_names = FALSE)
        
        subject_test <- read_table(file_path_subject_test ,col_names = FALSE)
        
 #Activity data      
        
        #Load activity data into R
        activity_labels <- read_table(file_path_activity_labels,col_names = FALSE)
        
 #Features data         
        
        #Load features data into R
        features <- read_csv(file_path_features,col_names = FALSE)
        
        ########## Tidying Features data ##################        
        
        # features data has the following faults with respect to tidy data
        #  1) column index and the column name both are sitting within one column.
        #  2) few columns have same name in common.
        
        # create a table clear_features by removing all the defects mentioned above.
        
        clear_features <- features %>% 
                             separate(X1,c("X1","X2"), sep = " ") %>% # separates column index from column name
                                group_by(X2) %>% 
                                   mutate(cnt = row_number(X2)) %>% 
                                      unite( X3, X2,cnt)              #removes the duplicate names by appending
                                                                      #occurance at the end and stores them in 
                                                                      #column X3
        
########## Joining of activities with its label ##################        
                        
          # y_train contains training activities corresponds to x_train data set.
          #However, its labels are there in the activitiy_lables data set.
                                  
          # create a data set training_activity_lables to contain activities and their descriptions 
                                  
          training_activity_lables <- left_join(y_train,activity_labels, by = "X1")  
          
          #similarly, create a data set test_activity_lables to contain activities and their descriptions 
          test_activity_lables <- left_join(y_test,activity_labels, by = "X1")  

#Data is now ready to perform the assignment tasks.
# I change the order of execution with my convenience

#starting with task 4 first
########## TASK 4 ##################
#Appropriately labels the data set with descriptive variable names.

          #Label the training data with appropriate labels
          
          names(X_train) <- clear_features$X3 
          
          #Label the test data with appropriate labels
          
          names(X_test) <- clear_features$X3 

########## TASK 2 ##################

#Extracts only the measurements on the mean and standard deviation for each measurement.

          # extract training data and store it to data_extract_training
          data_extract_training <- select(X_train, matches("mean|std"))
          
          # extract test data and store it to data_extract_test
          data_extract_test <- select(X_test, matches("mean|std"))

########## TASK 3 ##################

#Uses descriptive activity names to name the activities in the data set
          
    #Training data
        
        #Binding activities    

          #Add descriptve activity names to the training dataset and rename it to 'activity'
          
          data_extract_training <- bind_cols(as.data.frame(training_activity_lables$X2),data_extract_training)
          
          names(data_extract_training)[1] <- 'activity'
          
        #Binding subjects     
          
          #Add subject number to the training dataset and rename it to 'subject'
          
          data_extract_training <- bind_cols(as.data.frame(subject_train$X1),data_extract_training)
          
          names(data_extract_training)[1] <- 'subject'
          
    #Test data  
          
        #Binding activities       
          
          #Add descriptve activity names to the test dataset and rename it to 'activity'
          
          data_extract_test <- bind_cols(as.data.frame(test_activity_lables$X2),data_extract_test)
          
          names(data_extract_test)[1] <- 'activity'
          
        #Binding subjects     
          
          #Add subject number to the training dataset and rename it to 'subject'
          
          data_extract_test <- bind_cols(as.data.frame(subject_test$X1),data_extract_test)
          
          names(data_extract_test)[1] <- 'subject'

########## TASK 1 ##################
#Merges the training and the test sets to create one data set.

          #create tidy_data_extract by merging data_extract_test and data_extract_training.
          
          tidy_data_extract <- bind_rows(data_extract_test,data_extract_training)

########## TASK 5 ##################
#From the data set in step 4, creates a second, independent tidy data set with the average of 
          #each variable for each activity and each subject.    
          
          # perform the operation and store it into each_val_avg
          each_val_avg <- tidy_data_extract %>%
                             group_by(activity,subject) %>%
                                summarise_each(funs(mean))


########## WRITING THE OUTPUT TO A FILE ##################
file_path_output <- paste(working_dir,"/tidy_data_set.txt", sep = "")
          
          write.table(each_val_avg, file = file_path_output, row.names = FALSE)

