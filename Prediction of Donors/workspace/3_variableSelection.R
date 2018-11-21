
##########################
# 3 - Variable Selection #
##########################


# Load-in the libraries required
library(pROC)
library(rpart)
library(randomForest)

# Create a duplicate training data with only the required columns for modelling
train_2 <- train
# Remove donorID because all values are unique
train_2$donorID <- NULL
# Remove amount_reactivation as it is used for the creation of target variable
train_2$amount_reactivation <- NULL

# Convert dependant in numeric data type to perform pearson correlation
train_2$dependant = as.numeric(as.character(train_2$dependant))

# Custom function to calculate AUC:
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}

#FILTER (PEARSON CORRELATION)

# Remove dependant variable
vars = names(train_2)[-19]
selected = c()

for(v in vars){
  print(v)
  cortest = cor.test(train_2[,c(v)],train_2[,c("dependant")],method="pearson")
  pvalue = cortest$p.value
  print(pvalue)
  if(pvalue<0.001){
    selected = c(selected,v)
  }
}

f = paste("dependant~", paste(selected,collapse="+"))
lrmodel = glm(as.formula(f),data=train_2,family="binomial")
predictions_train = predict(lrmodel,newdata=train,type="response")
predictions_test = predict(lrmodel,newdata=test,type="response")
auc(train_2$dependant,predictions_train)
auc(train_2$dependant,predictions_test)
# Auc train 0.6001
# Auc train 0.5014

predicted <- as.numeric(predictions_test>0.5)
reference <- test$dependant
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)
# We are unable to predict any donors



# STEPWISE LOGISTIC REGRESSION

# All possible variables:
variables = names(train_2)[-19]
variablesorder = c()


# Construct a logistic regression model with no variables
model = glm(dependant ~ 1,data=train_2,family=binomial)

# Construct a formula with all the variables
formula<-formula(paste("dependant","~",paste(variables,collapse="+")))

# Stepwise procedure
for(i in c(1:length(variables))){
  #calculate AIC of each model
  info = add1(model,scope=formula,data=train_2)
  print(info)
  #get variable with lowest AIC
  orderedvariables = rownames(info[order(info$AIC),])
  v = orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder = append(variablesorder,v)
  formulanew = formula(paste("dependant","~",paste(variablesorder,collapse = "+")))
  model = glm(formulanew,data=train_2,family=binomial)
  print(v)
}

auctrain_2 = rep(0,length(variablesorder))
auctest = rep(0,length(variablesorder))
for(i in c(1:(length(variablesorder)))){
  vars = variablesorder[1:i]
  print(vars)
  formula<-paste("dependant","~",paste(vars,collapse="+"))
  model<-glm(formula,data=train_2,family="binomial")	
  predicttrain_2<-predict(model,newdata=train_2,type="response")
  predicttest<-predict(model,newdata=test,type="response")
  auctrain_2[i] = auc(train_2$dependant,predicttrain_2)
  auctest[i] = auc(test$dependant,predicttest)
} 

#SOLUTION
plot(auctrain_2, main="AUC", col="red", ylim=c(0.55,0.75))
par(new=TRUE)
plot(auctest,col="blue",add=TRUE, ylim=c(0.55,0.75))

finalvariables = variablesorder[c(0:12)]
formula<-paste("dependant","~",paste(finalvariables,collapse="+"))
model<-glm(formula,data=train_2,family="binomial")	
predicttrain<-predict(model,newdata=train_2,type="response")
predicttest<-predict(model,newdata=test,type="response")
auctrain = auc(train_2$dependant,predicttrain)
auctest = auc(test$dependant,predicttest)
auctrain
auctest

# Auc train 0.6391
# Auc train 0.6408

predicted <- as.numeric(predicttest>0.5)
reference <- test$dependant
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)
# Better AUC than filter but again We are unable to predict any donors

# Decision tree

ctrl = rpart.control(minsplit = 10,cp = 0.0001, minbucket = 25,maxdepth = 10) 
tree <- rpart(dependant ~ .,data = train_2, method="class", control = ctrl)

predictions_test <- predict(object = tree, newdata = test, type = "class")
predictions_train <- predict(object = tree, newdata = train_2, type = "class")

auc(train_2$dependant,predictions_train)
auc(test$dependant,predictions_test)
# AUC for both train and test is 0.5

# Random Forest
rForest <- randomForest(dependant ~ .,data = train_2, ntree=200)

predictions_test <- predict(object = rForest, newdata = test, type = "class")
predictions_train <- predict(object = rForest, newdata = train_2, type = "class")

auc(train_2$dependant,predictions_train)
auc(test$dependant,predictions_test)

# Auc train 0.9657
# Auc train 0.6065

predicted <- as.numeric(predictions_test>0.5)
reference <- test$dependant
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)
# Better AUC but again We are unable to predict any donors

table(test$dependant)
#   0      1 
# 25493   152

# The class is unbalanced, we need to balance it so as to improve the accuracy