# rxBTrees model
library(RevoScaleR)


vars <- paste(colnames(Readmission)[-25],collapse='+')#all column names except the 25th variable
form <- paste('unplanned_readmit_28_Days~',vars)
form

#build model
b_tree <- rxBTrees(form, data=Readmission.Train)
b_tree1 <- rxBTrees(form, data=data_balanced_both)
summary(b_tree)

#test trained model
scoreb_tree <- rxPredict(b_tree, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days")
scoreb_tree1 <- rxPredict(b_tree1, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days")

# Plot the ROC curve
roc <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "unplanned_readmit_28_Days_prob",
             data = scoreb_tree)
plot(roc)
roc1 <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "unplanned_readmit_28_Days_prob",
             data = scoreb_tree1)
plot(roc1)

#confusion matrix: 
scoreb_tree1$unplanned_readmit_28_Days_prob <- ifelse(scoreb_tree1$unplanned_readmit_28_Days_prob >0.5, TRUE, FALSE)#convert probabilities to ligical
confusion.matrix<-table(Actural=scoreb_tree1$unplanned_readmit_28_Days,Predicted=scoreb_tree1$unplanned_readmit_28_Days_prob)
confusion.matrix
mean(scoreb_tree1$unplanned_readmit_28_Days!=scoreb_tree1$PredictedLabel)#misclassification rate
