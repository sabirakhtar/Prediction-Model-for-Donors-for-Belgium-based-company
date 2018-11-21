
###################################
#  5 - Mixed Sampling Modelling   #
###################################

# Load-in the libraries required
library(caret)
library(caTools)
library(e1071)
library(ROCR)
library(dplyr)
library(ROSE)

# create_output_file_path - Function to create output path
create_output_file_path <- function(cv_id,cvrun_id,under_rate){
  fileTrainPathPred <-
    paste0("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/output_predicted - Mixed/",
           "unb2_cv",cv_id,
           "train",cvrun_id,
           "_p.csv")
  
  fileTestPathPred <-
    paste0("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/output_predicted - Mixed/",
           "unb2_cv",cv_id,
           "test",cvrun_id,
           "_p.csv")
  
  filePathAUC <-
    paste0("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/output_auc - Mixed/",
           "unb2_cv",cv_id,
           "run",cvrun_id,
           "_AUC.csv")
  
  filePath <- data.frame("fileTrainPathPred" = fileTrainPathPred,
                         "fileTestPathPred" = fileTestPathPred,
                         "filePathAUC"=filePathAUC)
  filePath$fileTrainPathPred <- as.character(filePath$fileTrainPathPred)
  filePath$fileTestPathPred <- as.character(filePath$fileTestPathPred)
  filePath$filePathAUC <- as.character(filePath$filePathAUC)
  return(filePath)
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
    
    print(paste0("cv_id",cv_id,"_cvrun_id",cvrun_id))
    
    # Over sample donors and under sample non-donors to reach in the 
    # central position where the probability between donors and non-donors is 0.5
    data_balanced_both <- ovun.sample(dependant ~ ., data = train, method = "both", p=0.5,N=nrow(train))$data
    
    # Create a vector with only required variables
    unwanted_cols <- c("donorID","amount_reactivation")
    wanted_cols <- names(train)[!names(train) %in% unwanted_cols]
    
    # Subset the data on required variables
    dsn_train <- data_balanced_both[,wanted_cols]
    dsn_test <- test[,wanted_cols]
    
    # Perform Logistic regression
    mylogit <- glm(dependant ~ ., data = dsn_train, family = binomial)

    dsn_test_p <- data.frame(dependant= dsn_test$dependant)
    dsn_train_p <- data.frame(dependant= dsn_train$dependant)
    
    # Predict Logistic regression
    dsn_train_p$p_1 <- predict(mylogit, newdata = dsn_train, type="response")
    dsn_test_p$p_1 <- predict(mylogit, newdata = dsn_test, type="response")
    
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