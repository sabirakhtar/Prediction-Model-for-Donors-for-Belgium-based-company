
##############################
#      6 - Benchmarking      #
##############################

# Load-in the libraries required
library(pROC)
library(pbapply)
library(data.table)
library(dplyr)
library(ROCR)

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
  
  auc_val <- auc(test_p[,"dependant"],test_p[,under_obs])
  
  df <- data.frame("auc" = auc_val, "acc" = acc, 
                   "Sensitivity" = Sensitivity, "Specificity"= Specificity, 
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

# Custom function to calculate AUC:
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}

# List all the Predictions files for both the algorithms
pred_files <- list.files("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/output_predicted/")
pred_files <- paste0("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/output_predicted/", pred_files)

pred_files_mixed <- list.files("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/output_predicted - Mixed/")
pred_files_mixed <- paste0("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/output_predicted - Mixed/", pred_files_mixed)

# Take only training files
pred_files_train <- pred_files[grepl(".train.",pred_files)]
pred_files_mixed_train <- pred_files_mixed[grepl(".train.",pred_files_mixed)]

# Take only testing files
pred_files <- pred_files[grepl(".test.",pred_files)]
pred_files_mixed <- pred_files_mixed[grepl(".test.",pred_files_mixed)]

# input_data - For reading data
input_data <- function(path){
  df <- read.csv(path, stringsAsFactors = F)
  df <- df[colnames(df) %in% c("dependant","p_1")]
  df$index <- 1:nrow(df)
  if(grepl("*under0.4_p.csv",path)){
    df$under_rate <- 0.4
  }
  if(grepl("*under0.5_p.csv",path)){
    df$under_rate <- 0.5
  }
  return(df)
  
}

# Read all the Predictions from both the algorithms - Test
myfiles <- pblapply(pred_files, input_data)
myfilesmixed <- pblapply(pred_files_mixed, input_data)

# Read all the Predictions from both the algorithms - Train
myfiles_train <- pblapply(pred_files_train, input_data)
myfilesmixed_train <- pblapply(pred_files_mixed_train, input_data)

# Create Data Frame
pred_data <- bind_rows(myfiles)
pred_data_mixed <- bind_rows(myfilesmixed)

pred_data_train <- bind_rows(myfiles_train)
pred_data_mixed_train <- bind_rows(myfilesmixed_train)

# Take a vote of all the 5*2 by averaging-out the predicted probabilities
pred_data <- pred_data %>%
  group_by(under_rate, index) %>%
  summarise(dependant = mean(dependant),
            p_1 = mean(p_1))

pred_data_mixed <- pred_data_mixed %>%
  group_by(index) %>%
  summarise(dependant = mean(dependant),
            p_1 = mean(p_1))

pred_data_train <- pred_data_train %>%
  group_by(under_rate, index) %>%
  summarise(dependant = mean(dependant),
            p_1 = mean(p_1))

pred_data_mixed_train <- pred_data_mixed_train %>%
  group_by(index) %>%
  summarise(dependant = mean(dependant),
            p_1 = mean(p_1))

pred_data_0.4 <- subset(pred_data, under_rate == 0.4)
pred_data_0.5 <- subset(pred_data, under_rate == 0.5)

pred_data_0.4_train <- subset(pred_data_train, under_rate == 0.4)
pred_data_0.5_train <- subset(pred_data_train, under_rate == 0.5)

# Create a final Data frame with all the predictions
final_predictions_test <- data.frame("dependant" = test$dependant,
                                "amount_reactivation" = test$amount_reactivation,
                                "Undersampling_0.4" = pred_data_0.4$p_1,
                                "Undersampling_0.5" = pred_data_0.5$p_1,
                                "Mixed_Sampling" = pred_data_mixed$p_1)

final_predictions_train <- data.frame("dependant" = train$dependant,
                                     "amount_reactivation" = train$amount_reactivation,
                                     "Undersampling_0.4" = pred_data_0.4_train$p_1,
                                     "Undersampling_0.5" = pred_data_0.5_train$p_1,
                                     "Mixed_Sampling" = pred_data_mixed_train$p_1)
write.csv(final_predictions_test, file="C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/final_predictions_test.csv", row.names=F)
write.csv(final_predictions_train, file="C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/final_predictions_train.csv", row.names=F)

# Calculating performance parameters - Test
df_0.4_test <- calculate_performance_params("Undersampling_0.4",final_predictions_test)
df_0.5_test <- calculate_performance_params("Undersampling_0.5",final_predictions_test)

df_mixed_test<- calculate_performance_params("Mixed_Sampling",final_predictions_test)

df_0.4_test$under_rate <- 0.4
df_0.5_test$under_rate <- 0.5
df_mixed_test$under_rate <- NA

# Calculating performance parameters - Train
df_0.4_train <- calculate_performance_params("Undersampling_0.4",final_predictions_train)
df_0.5_train <- calculate_performance_params("Undersampling_0.5",final_predictions_train)

df_mixed_train <- calculate_performance_params("Mixed_Sampling",final_predictions_train)

df_0.4_train$under_rate <- 0.4
df_0.5_train$under_rate <- 0.5
df_mixed_train$under_rate <- NA

# Create a final Data frame with all the performance parameters
final_performance_test <- rbind(df_0.4_test,df_0.5_test,df_mixed_test)
final_performance_test$Data_Type <- "Test"
final_performance_train <- rbind(df_0.4_train,df_0.5_train,df_mixed_train)
final_performance_train$Data_Type <- "Train"
final_performance <- rbind(final_performance_test,final_performance_train)
write.csv(final_performance, file="C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/final_performance1.csv", row.names=F)

# Create Gains and Lift Curve
pred_0.4 <- prediction(final_predictions_test$Undersampling_0.4, final_predictions_test$dependant)
pred_0.5 <- prediction(final_predictions_test$Undersampling_0.5, final_predictions_test$dependant)
pred_Mixed <- prediction(final_predictions_test$Mixed_Sampling, final_predictions_test$dependant)

gain_0.4 <- performance(pred_0.4, "tpr", "rpp")
gain_0.5 <- performance(pred_0.5, "tpr", "rpp")
gain_Mixed <- performance(pred_Mixed, "tpr", "rpp")

plot(gain_0.4, main = "Gain Chart", col="red")
plot(gain_0.5, add = T,main = "Gain Chart", col="blue")
plot(gain_Mixed, add = T,main = "Gain Chart", col="green")
abline(v=0.1)
abline(v=0.2)
abline(v=0.3)
abline(v=0.4)

lifty_0.4 <- performance(pred_0.4, "lift", "rpp")
lifty_0.5 <- performance(pred_0.5, "lift", "rpp")
lifty_Mixed <- performance(pred_Mixed, "lift", "rpp")

plot(lifty_0.4, main = "Lift Chart", col="red")
plot(lifty_0.5, add = T, main = "Lift Chart", col="blue")
plot(lifty_Mixed, add = T, main = "Lift Chart", col="green")
abline(v=0.1)
abline(v=0.2)
abline(v=0.3)
abline(v=0.4)

