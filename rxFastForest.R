# rxFastForest model
library(MicrosoftML)


vars <- paste(colnames(Readmission)[-25],collapse='+')#all column names except the 25th variable
form <- paste('unplanned_readmit_28_Days~',vars)
form

#build model
fast_forest <- rxFastForest( form,data = Readmission.Train)
fast_forest1 <- rxFastForest( form,data = data_balanced_both)
summary(fast_forest)

#test trained model
scorefast_forest <- rxPredict(fast_forest, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days")
scorefast_forest1 <- rxPredict(fast_forest1, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days")

# Plot the ROC curve
roc <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "Probability",
             data = scorefast_forest)
plot(roc)
roc1 <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "Probability",
             data = scorefast_forest1)
plot(roc1)

#confusion matrix
confusion.matrixft<-table(Actural=scorefast_forest1$unplanned_readmit_28_Days,Predicted=scorefast_forest1$PredictedLabel)
confusion.matrixft
mean(scorefast_forest1$unplanned_readmit_28_Days!=scorefast_forest1$PredictedLabel)#misclassification rate
