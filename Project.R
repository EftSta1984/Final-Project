###################################################################
########################## Final project ##########################
###################################################################

    ## Load caret and ggplot2 packages
    library(caret); library(ggplot2)
    
    ######## Data Loading ########
    setwd("~/Week 4/")
    # Load training and test data. Set the entries "NA" and #DIV/0! to NA
    input_train <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!"))
    input_test <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!")) 
    
    table(input_train$classe)/length(input_train$classe) # Check the levels of the column of interest
    
    
    ######## Preprocessing stage ########
    # Discard the first columns that are not related to the output
    input_train <- input_train[,-c(1:7)]; input_test = input_test[,-c(1:7)]
    
    # Find the columns whose missing values constitute more than 90% of the column size. 
    high_num_NAs <- sapply(input_train, function(x){mean(is.na(x))>0.9}) 
    
    # Discard these columns 
    input_train <- input_train[,high_num_NAs == FALSE] ; input_test <- input_test[,high_num_NAs == FALSE]
    
    
    ######## Choice of validation and training set ########
    inBuild <- createDataPartition(input_train$classe, p=0.7, list=FALSE) # Split the input data
    validation_data <- input_train[-inBuild,] # Create the validation set
    train_data <- input_train[inBuild,] # Create the train set

    
    ######## Model fitting, validation and testing ########
    ## Boosting algorithm
    modFit_gb <- train(classe ~ ., method="gbm", data=train_data, verbose=FALSE)
    confusionMatrix(validation_data$classe, predict(modFit_gb, validation_data))
    
    ## Random Forest
    modFit_rf <- train(classe ~ ., method="rf", data=train_data)
    confusionMatrix(validation_data$classe, predict(modFit_rf, validation_data))
    
    ## Prediction phase
    pred_gb <- predict(modFit_gb, input_test)
    pred_rf <- predict(modFit_rf, input_test)
    pred_frame <- data.frame(pred_gb, pred_rf)