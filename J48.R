# Due to memory limit, we need very small data set to test J48 algorithm
Readmission <- read.csv('C:/Users/Yang/Desktop/R project/BICG/Work/Output.CSV')
str(Readmission)
Readmission$unplanned_readmit_28_Days <- as.factor(Readmission$unplanned_readmit_28_Days)
Readmission$LOS<-as.integer(Readmission$LOS)
str(Readmission)

#We use records from only one facility
N_Readmission<-Readmission[Readmission$Facility.ID=='R205',]
# n_Re<-subset(Readmission,Facility.ID=='R205')
str(N_Readmission)

#Split Between Training and Testing
# set.seed(1)
s <- N_Readmission[sample(1:nrow(N_Readmission),size =nrow(N_Readmission) , replace = FALSE),]
ss <- sample(1:nrow(s),size = 0.9*nrow(s))
N_Readmission.Train <- s[ss,]
N_Readmission.Test <- s[-ss,]
table(N_Readmission.Train$unplanned_readmit_28_Days)

# Balance data set
library(ROSE)
data_balanced_rose <- ROSE(unplanned_readmit_28_Days ~ ., data = N_Readmission.Train, N=102867,seed = 1)$data
table(data_balanced_rose$unplanned_readmit_28_Days)

data_balanced_both <- ovun.sample( unplanned_readmit_28_Days~., data = N_Readmission.Train, method = "under",p=0.5, seed = 1)$data
table(data_balanced_both$unplanned_readmit_28_Days)

#Build the J48 model and validate
library(RWeka)
# install.packages("partykit")
library(partykit)
# install.packages("party")
library(party)
JTree <- J48(unplanned_readmit_28_Days~.,data_balanced_both) 
PTree <- predict(JTree, N_Readmission.Test)

confusion.matrixJT<-table(N_Readmission.Test$unplanned_readmit_28_Days,PTree)
confusion.matrixJT

#output the rules
JTree
write.table(JTree,file = "C:/Users/Yang/Desktop/R project/BICG/J48 output rules.txt")


#plot the tree
library("partykit")
pres <- as.party(JTree)
plot(JTree)


# # library(rpart.plot)
# # p<-rpart.plot(JTree)






