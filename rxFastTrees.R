# rxFastTree model
library(MicrosoftML)

#source("Prepare data set.R") first

vars <- paste(colnames(Readmission)[-25],collapse='+')#all column names except the 25th variable
form <- paste('unplanned_readmit_28_Days~',vars)
form

#build model
fast_tree <- rxFastTrees( form,data = Readmission.Train)
fast_tree1 <- rxFastTrees( form,data = data_balanced_rose)
summary(fast_tree)

#test trained model
scorefast_tree <- rxPredict(fast_tree, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days")
scorefast_tree1 <- rxPredict(fast_tree1, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days")

# Plot the ROC curve
roc <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "Probability",
             data = scorefast_tree1)
plot(roc)

#confusion matrix
confusion.matrix<-table(Actural=scorefast_tree$unplanned_readmit_28_Days,Predicted=scorefast_tree$PredictedLabel)
confusion.matrix
mean(scorefast_tree1$unplanned_readmit_28_Days!=scorefast_tree1$PredictedLabel)#misclassification rate
