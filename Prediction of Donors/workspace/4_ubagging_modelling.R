
##############################
#  4 - U-bagging Modelling   #
##############################

# Load-in the libraries required
library(caret)
library(caTools)
library(e1071)
library(ROCR)
library(dplyr)

# create_output_file_path - Function to create output path
create_output_file_path <- function(cv_id,cvrun_id,under_rate){
  fileTrainPathPred <-
    paste0("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/output_predicted/",
           "unb2_cv",cv_id,
           "train",cvrun_id,
           "under",under_rate,"_p.csv")
  
  fileTestPathPred <-
    paste0("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/output_predicted/",
           "unb2_cv",cv_id,
           "test",cvrun_id,
           "under",under_rate,"_p.csv")
  
  filePathAUC <-
    paste0("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/output_auc/",
           "unb2_cv",cv_id,
           "run",cvrun_id,
           "under",under_rate,"_AUC.csv")
  
  filePath <- data.frame("fileTrainPathPred" = fileTrainPathPred,
                         "fileTestPathPred" = fileTestPathPred,
                         "filePathAUC"=filePathAUC)
  filePath$fileTrainPathPred <- as.character(filePath$fileTrainPathPred)
  filePath$fileTestPathPred <- as.character(filePath$fileTestPathPred)
  filePath$filePathAUC <- as.character(filePath$filePathAUC)
  return(filePath)
}

# random_under_sample_list_generator - Function to create random under sample list
random_under_sample_list_generator <- function(iters, dsn_train_0, dsn_train_1, under){
  
  if(nrow(dsn_train_0) > under){
    under_sample <- rbind(dsn_train_1,dsn_train_0[sample(1:nrow(dsn_train_0),under,replace=F),])
  } else{
    under_sample <- rbind(dsn_train_1,dsn_train_0[sample(1:nrow(dsn_train_0),under,replace=T),])
  }
  return(under_sample)
}

# calculate_auc - Function to calculate AUC
calculate_auc <- function(under_obs, test_p){
  
  pred <- prediction(test_p[,under_obs],test_p[,"dependant"])
  auc<- performance(pred,"auc")
  auc <- unlist(slot(auc, "y.values"))
  
  return(auc)
}

# calculate_performance_params - Function to calculate Accuracy, Sensitivity, Specficity,
# Top10Lift, Top20Lift, Top30Lift, Top40Lift, Top50Lift
calculate_performance_params <- function(under_obs, test_p){
  
  predicted <- as.numeric(test_p[,under_obs]>0.5)
  reference <- test_p[,"dependant"]
  u <- union(predicted, reference)
  xtab <- table(factor(predicted, u), factor(reference, u))
  acc <- confusionMatrix(xtab)$overall[[1]]
  Sensitivity <- confusionMatrix(xtab)$byClass[[1]]
  Specificity <- confusionMatrix(xtab)$byClass[[2]]
  
  predicted <- test_p[,under_obs]
  
  Top10Lift <- calculate_lift(predicted, reference, 10)
  Top20Lift <- calculate_lift(predicted, reference, 20)
  Top30Lift <- calculate_lift(predicted, reference, 30)
  Top40Lift <- calculate_lift(predicted, reference, 40)
  Top50Lift <- calculate_lift(predicted, reference, 50)
  
  df <- data.frame("acc" = acc, "Sensitivity" = Sensitivity, "Specificity"= Specificity, 
                   "Top10Lift" = Top10Lift, "Top20Lift" = Top20Lift,
                   "Top30Lift" = Top30Lift, "Top40Lift" = Top40Lift,
                   "Top50Lift" = Top50Lift)
  
  return(df)
}

# calculate_lift- For calculating Lift
calculate_lift <- function(predicted, reference, val){
  
  if (is.factor(reference)) 
    reference <- as.integer(as.character(reference))
  
  lift_val <- data.frame(predicted, reference)
  lift_val <- lift_val[order(-lift_val[, 1]), ]
  lift_val <- lift_val[1:floor(nrow(lift_val)*val/100),]
  lift_val$predicted <- ifelse(lift_val$predicted > 0.5,1,0)
  lift_val$result <- ifelse(lift_val$predicted == lift_val$reference & lift_val$predicted == 1, 1,0)
  
  res <- as.numeric(mean(lift_val$result)/mean(reference))
  return(res)
}

# 5 * 2 folds cross validation
for(cv_id in c(1:5)){
  for(cvrun_id in c(1:2)){
    for(under_rate in seq(0.4,0.5, by = 0.1)){
      
      print(paste0("under_rate",under_rate,"_cv_id",cv_id,"_cvrun_id",cvrun_id))
      start_time <- Sys.time()
      
      # Create a vector with only required variables
      unwanted_cols <- c("donorID","amount_reactivation")
      wanted_cols <- names(train)[!names(train) %in% unwanted_cols]
      
      # Subset the data on required variables
      dsn_train <- train[,wanted_cols]
      dsn_test <- test[,wanted_cols]
      
      # Subset the data on dependant 0 or 1
      dsn_train_0 <- subset(dsn_train, dependant == 0)
      dsn_train_1 <- subset(dsn_train, dependant == 1)
      
      # Calculate the undersampling rate
      under <- round(nrow(dsn_train_1)/(1-under_rate) - nrow(dsn_train_1))
      
      # Create a final data frame based on under-sampling rate
      dsn_train_f <- lapply(1:20,random_under_sample_list_generator, dsn_train_0, dsn_train_1, under)
      
      # Perform normal Logistic regression
      mylogit <- glm(dependant ~ ., data = dsn_train, family = binomial)
      
      dsn_test_p <- data.frame(dependant= dsn_test$dependant)
      dsn_train_p <- data.frame(dependant= dsn_train$dependant)
      
      # Predict normal Logistic regression
      dsn_train_p$p_o_1 <- predict(mylogit, newdata = dsn_train, type="response")
      dsn_test_p$p_o_1 <- predict(mylogit, newdata = dsn_test, type="response")
      
      # Create a Logistic regression model based on the data created above to perform bagging
      for(i in 1:20){
        mylogit <- glm(dependant ~ ., data = dsn_train_f[[i]], family = binomial)
        dsn_test_p[ ,paste0("u_1_",i)] <- predict(mylogit, newdata = dsn_test, type="response")
        dsn_train_p[ ,paste0("u_1_",i)] <- predict(mylogit, newdata = dsn_train, type="response")
      }
      
      # Take a vote of all the 20 predictions by averaging-out the predicted probabilities
      dsn_test_p$p_1 <- rowMeans(dsn_test_p[, grepl("^u",colnames(dsn_test_p))])
      dsn_train_p$p_1 <- rowMeans(dsn_train_p[, grepl("^u",colnames(dsn_train_p))])
      
      # Calculate AUC
      test_auc <- as.numeric(sapply(colnames(dsn_test_p)[-1], calculate_auc, dsn_test_p))
      train_auc <- as.numeric(sapply(colnames(dsn_train_p)[-1], calculate_auc, dsn_train_p))
      
      # Calculate other performance parameters like: Accuracy, Sensitivity, Specficity,
      # Top10Lift, Top20Lift, Top30Lift, Top40Lift, Top50Lift
      test_perf_params <- lapply(colnames(dsn_test_p)[-1], calculate_performance_params, dsn_test_p)
      test_perf_params <- bind_rows(test_perf_params)
      
      train_perf_params <- lapply(colnames(dsn_train_p)[-1], calculate_performance_params, dsn_train_p)
      train_perf_params <- bind_rows(train_perf_params)
      
      # Create resulting performance data frame
      dsn_auc <- data.frame("id" = colnames(dsn_test_p)[-1],
                            "under_rate" = under_rate,
                            "test_auc" = test_auc,
                            "test_acc" = test_perf_params$acc,
                            "test_sens" = test_perf_params$Sensitivity,
                            "test_spec" = test_perf_params$Specificity,
                            "test_Top10Lift" = test_perf_params$Top10Lift,
                            "test_Top20Lift" = test_perf_params$Top20Lift,
                            "test_Top30Lift" = test_perf_params$Top30Lift,
                            "test_Top40Lift" = test_perf_params$Top40Lift,
                            "test_Top50Lift" = test_perf_params$Top50Lift,
                            "train_auc" = train_auc,
                            "train_acc" = train_perf_params$acc,
                            "train_sens" = train_perf_params$Sensitivity,
                            "train_spec" = train_perf_params$Specificity,
                            "train_Top10Lift" = train_perf_params$Top10Lift,
                            "train_Top20Lift" = train_perf_params$Top20Lift,
                            "train_Top30Lift" = train_perf_params$Top30Lift,
                            "train_Top40Lift" = train_perf_params$Top40Lift,
                            "train_Top50Lift" = train_perf_params$Top50Lift)
      
      # Creating file output path based on each CV and Run
      fileOutputPath <- create_output_file_path(cv_id,cvrun_id,under_rate)
      
      # Write the predictions and performance parameters separately
      write.csv(dsn_test_p,file=fileOutputPath$fileTestPathPred,row.names = F)
      write.csv(dsn_train_p,file=fileOutputPath$fileTrainPathPred,row.names = F)
      write.csv(dsn_auc,file=fileOutputPath$filePathAUC,row.names = F)
      
      end_time <- Sys.time()
      time_taken <- end_time - start_time
      print(paste0("Time Taken: ", time_taken))
      
      # Remove unwanted parameters from the environment
      rm(list=setdiff(ls(), c("create_file_path","create_output_file_path",
                              "random_under_sample_list_generator",
                              "random_bagging_list_generator",
                              "calculate_auc","under_rate",
                              "cv_id","cvrun_id","dsn_id","train","test",
                              "calculate_performance_params","calculate_lift")))
      # Garbage Collection
      gc() 
      gc()
    }
  }
}
