#rxDTree model
library(RevoScaleR)

vars <- paste(colnames(Readmission)[-25],collapse='+')#all column names except the 25th variable
form <- paste('unplanned_readmit_28_Days~',vars)
form

#build model
d_tree <- rxDTree(form,data= Readmission.Train)
d_tree1 <- rxDTree(form,data= data_balanced_both)


#test trained model
predd_tree <- rxPredict(d_tree, Readmission.Test,extraVarsToWrite = 'unplanned_readmit_28_Days')
predd_tree1 <- rxPredict(d_tree1, Readmission.Test,extraVarsToWrite = 'unplanned_readmit_28_Days')

# Plot the ROC curve
roc <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "TRUE_prob",
             data = predd_tree)
plot(roc)
roc1 <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "unplanned_readmit_28_Days_Pred",
              data = predd_tree1)
plot(roc1)

#confusion matrix
predd_tree1$unplanned_readmit_28_Days_Pred <- as.logical(predd_tree1$unplanned_readmit_28_Days_Pred)
confusion.matrix<-table(Actural=predd_tree1$unplanned_readmit_28_Days,Predicted=predd_tree1$unplanned_readmit_28_Days_Pred)
confusion.matrix
mean(predd_tree1$unplanned_readmit_28_Days!=predd_tree1$unplanned_readmit_28_Days_Pred)#misclassification rate

#clean up files
rm(confusion.matrix)
rm(roc)
rm(roc1)
