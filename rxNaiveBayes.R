# rxNaiveBayes model
library(RevoScaleR)


# vars <- paste(colnames(Readmission)[-25],collapse='+')#all column names except the 25th variable
# form <- paste('unplanned_readmit_28_Days~',vars)
# form

#convert response variable to factor type
Readmission.Train$unplanned_readmit_28_Days <- as.factor(Readmission.Train$unplanned_readmit_28_Days)
Readmission.Test$unplanned_readmit_28_Days <- as.factor(Readmission.Test$unplanned_readmit_28_Days)
data_balanced_both$unplanned_readmit_28_Days <- as.factor(data_balanced_both$unplanned_readmit_28_Days)
data_balanced_rose$unplanned_readmit_28_Days <- as.factor(data_balanced_rose$unplanned_readmit_28_Days)

#build model
naive_bayes <- rxNaiveBayes(unplanned_readmit_28_Days~ 
                              ï..Episodes_Medical.Officer.Code.1+
                              Source.of.Referral.Desc+Facility.ID+
                              DRG.Description+DschWard+AdmWard+Funded+
                              Mode.of.Separation.Desc+SRG.Desc+LastEpisode+
                              AgeGroup+ED.Mode.of.Arrival.Desc+PPH.Category+
                              Episodes_PDxDesc+HadPPH+HadMH+Complications+
                              Postcode+Medicare.Eligibilty.Status+Suburb+
                              Sex+LOS+Aboriginality.Description+
                              Intention_to_readmit, data=Readmission.Train,
                             smoothingFactor = 1)
# naive_bayes
naive_bayes1 <- rxNaiveBayes(unplanned_readmit_28_Days~ 
                              ï..Episodes_Medical.Officer.Code.1+
                              Source.of.Referral.Desc+Facility.ID+
                              DRG.Description+DschWard+AdmWard+Funded+
                              Mode.of.Separation.Desc+SRG.Desc+LastEpisode+
                              AgeGroup+ED.Mode.of.Arrival.Desc+PPH.Category+
                              Episodes_PDxDesc+HadPPH+HadMH+Complications+
                              Postcode+Medicare.Eligibilty.Status+Suburb+
                              Sex+LOS+Aboriginality.Description+
                              Intention_to_readmit, data=data_balanced_both,
                            smoothingFactor = 1)


#test trained model
scorenaive_bayes <- rxPredict(naive_bayes, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days",type = "prob")
scorenaive_bayes1 <- rxPredict(naive_bayes1, data = Readmission.Test,extraVarsToWrite = "unplanned_readmit_28_Days",type = "prob")

#some manipulation on prediction output data set
scorenaive_bayes$class_Pred <- ifelse(scorenaive_bayes$TRUE_prob >0.5, 1, 0)
scorenaive_bayes$unplanned_readmit_28_Days <- ifelse(scorenaive_bayes$unplanned_readmit_28_Days ==TRUE, 1, 0)

scorenaive_bayes1$class_Pred <- ifelse(scorenaive_bayes1$TRUE_prob >0.5, 1, 0)
scorenaive_bayes1$unplanned_readmit_28_Days <- ifelse(scorenaive_bayes1$unplanned_readmit_28_Days ==TRUE, 1, 0)

# Plot the ROC curve
roc <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "TRUE_prob",
             data = scorenaive_bayes)
plot(roc)
roc1 <- rxRoc(actualVarName = "unplanned_readmit_28_Days", predVarNames = "TRUE_prob",
             data = scorenaive_bayes1)
plot(roc1)

#confusion matrix

confusion.matrix<-table(Actural=scorenaive_bayes1$unplanned_readmit_28_Days,Predicted=scorenaive_bayes1$class_Pred)
confusion.matrix
mean(scorenaive_bayes1$unplanned_readmit_28_Days!=scorenaive_bayes1$class_Pred)#misclassification rate

#  more details about confusion matrix and statistics
library(caret)
library(e1071)
confusionMatrix(confusion.matrix,positive = "1")
