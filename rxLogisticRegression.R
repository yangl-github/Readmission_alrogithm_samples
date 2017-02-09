# rxLogisticRegression model
library(MicrosoftML)


vars <- paste(colnames(Readmission)[-25],collapse='+')#all column names except the 25th variable
form <- paste('unplanned_readmit_28_Days~',vars)
form

#build model
logistic_regression <- rxLogisticRegression(form, data=Readmission.Train)
logistic_regression1 <- rxLogisticRegression(form, data=data_balanced_both)
summary(logistic_regression)

#test trained model: no difference between the following two methods
scorelogistic_regression <- rxPredict(logistic_regression, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days")
scorelogistic_regression1 <- rxPredict(logistic_regression1, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days")
# scorebtree1 <- rxPredict(dforest, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days",type="response")

# Plot the ROC curve
roc <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "PredictedLabel",
             data = scorelogistic_regression)
plot(roc)
roc1 <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "PredictedLabel",
             data = scorelogistic_regression1)
plot(roc1)

#confusion matrix
confusion.matrixft<-table(Actural=scorelogistic_regression1$unplanned_readmit_28_Days,Predicted=scorelogistic_regression1$PredictedLabel)
confusion.matrixft
mean(scorelogistic_regression1$unplanned_readmit_28_Days!=scorelogistic_regression1$PredictedLabel)#misclassification rate
