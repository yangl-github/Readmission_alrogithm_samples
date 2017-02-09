# # install.packages('RODBC')
# library(RODBC)
# # connStrSql <- "Driver=SQL Server;Server=localhost;Database=Test;Uid=sa;Pwd=pass@word1"
# 
# ch <- odbcConnect("Readmission", uid = "sa", pwd = "pass@word1")
# p<-sqlQuery(ch,"SELECT [Episodes_Medical Officer Code 1]
#             ,[Source of Referral Desc]
#             ,[Facility ID]
#             ,[DRG Description]
#             ,[DschWard]
#             ,[AdmWard]
#             ,[Funded]
#             ,[Mode of Separation Desc]
#             ,[SRG Desc]
#             ,[LastEpisode]
#             ,[AgeGroup]
#             ,[ED Mode of Arrival Desc]
#             ,[PPH Category]
#             ,[Episodes_PDxDesc]
#             ,[HadPPH]
#             ,[HadMH]
#             ,[Complications]
#             ,[Postcode]
#             ,[Medicare Eligibilty Status]
#             ,[Suburb]
#             ,[Sex]
#             ,[LOS]
#             ,[Aboriginality Description]
#             ,[Intention_to_readmit]
#             ,[unplanned_readmit_28_Days]
#             FROM [Test].[dbo].[Readmissions]where[Facility ID]='R205'")
# # close(ch)
# 
# #store dataset from SQL to 
# SQL_Readmission <- p
# str(SQL_Readmission)


#import dataset
Readmission <- read.csv('C:/Users/Yang/Desktop/R project/BICG/Output.CSV')
# str(Readmission)
Readmission$unplanned_readmit_28_Days <- as.factor(Readmission$unplanned_readmit_28_Days)
Readmission$LOS<-as.integer(Readmission$LOS)
str(Readmission)
# names(Readmission)

#build tree model with rxDTREE
library(RevoScaleR)


#creat decision tree model 
vars <- paste(colnames(Readmission)[-25],collapse='+')#all column names except the 25th variable
vars
form <- paste('unplanned_readmit_28_Days~',vars)
form

#Split Between Training and Testing
set.seed(11)
ss <- sample(1:nrow(Readmission),size = 0.9*nrow(Readmission))
Readmission.Train <- Readmission[ss,]
Readmission.Test <- Readmission[-ss,]
# 
# #run model on imbalanced dataset
# FirstTree <- rxDTree(form,data= Readmission.Train)
# PredTree <- rxPredict(FirstTree, Readmission.Test,extraVarsToWrite = 'unplanned_readmit_28_Days',type="class")
# # PPredTree <- rxPredict(FirstTree, Readmission.Test,extraVarsToWrite = 'unplanned_readmit_28_Days')
# # xtabs(~Readmission.Test$unplanned_readmit_28_Days+PredTree$unplanned_readmit_28_Days_Pred, PredTree)
# confusion.matrix<-table(Actural=Readmission.Test$unplanned_readmit_28_Days,Predicted=PredTree[,1])


#check data balance
# install.packages("ROSE")
library(ROSE)

table(Readmission.Train$unplanned_readmit_28_Days)
prop.table(table(Readmission.Train$unplanned_readmit_28_Days))

# ##balance the data with both oversampling and undersampling
# data_balanced_both <- ovun.sample( unplanned_readmit_28_Days~ ., data = Readmission.Train, method = "both", p=0.5,N=626556, seed = 1)$data
# table(data_balanced_both$unplanned_readmit_28_Days)
# 
# ##model on under&over
# BalancedTree1 <- rxDTree(form,data=data_balanced_both)
# PredBTree1 <- rxPredict(BalancedTree1,Readmission.Test,extraVarsToWrite = 'unplanned_readmit_28_Days',type="class")
# confusion.matrixBT1<-table(Actural=Readmission.Test$unplanned_readmit_28_Days,Predicted=PredBTree1[,1])
# confusion.matrixBT1


#balance the data with synthetic data generation
data_balanced_rose <- ROSE(unplanned_readmit_28_Days ~ ., data = Readmission.Train, seed = 11)$data
table(data_balanced_rose$unplanned_readmit_28_Days)

#model on ROSE
BalancedTree2 <- rxDTree(form,data=data_balanced_rose)
PredBTree2 <- rxPredict(BalancedTree2,Readmission.Test,extraVarsToWrite = 'unplanned_readmit_28_Days',type = "class")
confusion.matrixBT2<-table(Actural=Readmission.Test$unplanned_readmit_28_Days,Predicted=PredBTree2[,1])
confusion.matrixBT2
BalancedTree2$variable.importance

#get rules use package rattle
library(rattle)
asRules(as.rpart(BalancedTree2))

#get rules use path.rpart
RTree<-rpart(unplanned_readmit_28_Days~.,data=data_balanced_rose,method = "class")
# RTree1<-rpart(unplanned_readmit_28_Days~AdmWard+Episodes_PDxDesc+DschWard+DRG.Description+?..Episodes_Medical.Officer.Code.1+SRG.Desc+Mode.of.Separation.Desc+Source.of.Referral.Desc+LastEpisode+PPH.Category+Intention_to_readmit+Suburb,data=data_balanced_rose,method = "class")
RTree$variable.importance
RTree$frame
summary(RTree1)

terminal_nodes = rownames(RTree1$frame)[RTree1$frame$var =="<leaf>"]
t<-path.rpart(RTree1 ,nodes=terminal_nodes)


#get rules use rattle()
rattle()