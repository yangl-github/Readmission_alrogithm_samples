# rxFastTree model
library(MicrosoftML)


vars <- paste(colnames(Readmission)[-25],collapse='+')#all column names except the 25th variable
form <- paste('unplanned_readmit_28_Days~',vars)
form

#build model
neural_net <- rxNeuralNet( form,data = Readmission.Train,numIterations=50)
neural_net1 <- rxNeuralNet( form,data = data_balanced_both,numIterations=50)
# summary(neural_net1)# this will leads to R session interupted, better not doing summary()

#test trained model
scoreneural_net <- rxPredict(neural_net, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days")
scoreneural_net1 <- rxPredict(neural_net1, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days")

# Plot the ROC curve
roc <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "Probability",
             data = scoreneural_net)
plot(roc)
roc1 <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "Probability",
             data = scoreneural_net1)
plot(roc1)

#confusion matrix
confusion.matrixft<-table(Actural=scoreneural_net1$unplanned_readmit_28_Days,Predicted=scoreneural_net1$PredictedLabel)
confusion.matrixft
mean(scoreneural_net1$unplanned_readmit_28_Days!=scoreneural_net1$PredictedLabel)#misclassification rate
