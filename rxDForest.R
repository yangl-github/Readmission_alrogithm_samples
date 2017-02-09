# rxDForest model
library(RevoScaleR)


vars <- paste(colnames(Readmission)[-25],collapse='+')#all column names except the 25th variable
form <- paste('unplanned_readmit_28_Days~',vars)
form

#build model
d_forest <- rxDForest(form, data=Readmission.Train)
d_forest1 <- rxDForest(form, data=data_balanced_both)
summary(d_forest)

#test trained model
scored_forest <- rxPredict(d_forest, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days")
scored_forest1 <- rxPredict(d_forest1, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days")

# Plot the ROC curve
roc <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "unplanned_readmit_28_Days_Pred",
             data = scored_forest)
plot(roc)
roc1 <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "unplanned_readmit_28_Days_Pred",
             data = scored_forest1)
plot(roc1)

#confusion matrix
scored_forest1$unplanned_readmit_28_Days_Pred <- ifelse(scored_forest1$unplanned_readmit_28_Days_Pred >0.5, TRUE, FALSE)#convert probabilities to ligical
confusion.matrix<-table(Actural=scored_forest1$unplanned_readmit_28_Days,Predicted=scored_forest1$unplanned_readmit_28_Days_Pred)
confusion.matrix
mean(scored_forest1$unplanned_readmit_28_Days!=scored_forest1$unplanned_readmit_28_Days_Pred)#misclassification rate

#clean up files
rm(confusion.matrix)
rm(roc)
rm(roc1)
rm(d_forest)
rm(d_forest1)
