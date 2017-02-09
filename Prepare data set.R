# manipulate data set

#read dataset
RawReadmission <- read.csv('C:/Users/Yang/Desktop/R project/BICG/Work/Output.CSV')
Readmission <- RawReadmission
rxGetVarInfo(Readmission)

#replace blanks with NA
levels(Readmission$Facility.ID)[1] <- NA
levels(Readmission$Aboriginality.Description)[1] <- NA
levels(Readmission$Postcode)[1] <- NA
Readmission$Intention_to_readmit <-as.integer(as.character(Readmission$Intention_to_readmit))

#remove space between and after
library(stringr)
Readmission$Source.of.Referral.Desc <- str_trim(Readmission$Source.of.Referral.Desc)
Readmission$ED.Mode.of.Arrival.Desc <- str_trim(Readmission$ED.Mode.of.Arrival.Desc)
Readmission$Postcode <- str_trim(Readmission$Postcode)
str(Readmission)

#replace space with "-"
Readmission$Source.of.Referral.Desc <- gsub("[[:space:]]", "-", Readmission$Source.of.Referral.Desc)
Readmission$DRG.Description <- gsub("[[:space:]]", "-", Readmission$DRG.Description)
Readmission$Funded <- gsub("[[:space:]]", "-", Readmission$Funded)
Readmission$Mode.of.Separation.Desc <- gsub("[[:space:]]", "-", Readmission$Mode.of.Separation.Desc)
Readmission$SRG.Desc <- gsub("[[:space:]]", "-", Readmission$SRG.Desc)
Readmission$AgeGroup <- gsub("[[:space:]]", "-", Readmission$AgeGroup)
Readmission$ED.Mode.of.Arrival.Desc <- gsub("[[:space:]]", "-", Readmission$ED.Mode.of.Arrival.Desc)
Readmission$PPH.Category <- gsub("[[:space:]]", "-", Readmission$PPH.Category)
Readmission$Episodes_PDxDesc <- gsub("[[:space:]]", "-", Readmission$Episodes_PDxDesc)
Readmission$Postcode <- gsub("[[:space:]]", "-", Readmission$Postcode)
Readmission$Suburb <- gsub("[[:space:]]", "-", Readmission$Suburb)
Readmission$Aboriginality.Description <- gsub("[[:space:]]", "-", Readmission$Aboriginality.Description)
str(Readmission)

#replace NULL values with NA
Readmission$ï..Episodes_Medical.Officer.Code.1 <- gsub("NULL", NA, Readmission$ï..Episodes_Medical.Officer.Code.1)
Readmission$Source.of.Referral.Desc <- gsub("NULL", NA, Readmission$Source.of.Referral.Desc)
Readmission$Facility.ID <- gsub("NULL", NA, Readmission$Facility.ID)
Readmission$DRG.Description <- gsub("NULL", NA, Readmission$DRG.Description)
Readmission$DschWard <- gsub("NULL", NA, Readmission$DschWard)
Readmission$AdmWard <- gsub("NULL", NA, Readmission$AdmWard)
Readmission$Funded <- gsub("NULL", NA, Readmission$Funded)
Readmission$Mode.of.Separation.Desc <- gsub("NULL", NA, Readmission$Mode.of.Separation.Desc)
Readmission$SRG.Desc <- gsub("NULL", NA, Readmission$SRG.Desc)
Readmission$LastEpisode <- gsub("NULL", NA, Readmission$LastEpisode)
Readmission$AgeGroup <- gsub("NULL", NA, Readmission$AgeGroup)
Readmission$ED.Mode.of.Arrival.Desc <- gsub("NULL", NA, Readmission$ED.Mode.of.Arrival.Desc)
Readmission$PPH.Category <- gsub("NULL", NA, Readmission$PPH.Category)
Readmission$Episodes_PDxDesc <- gsub("NULL", NA, Readmission$Episodes_PDxDesc)
Readmission$HadPPH <- gsub("NULL", NA, Readmission$HadPPH)
Readmission$HadMH <- gsub("NULL", NA, Readmission$HadMH)
Readmission$Complications <- gsub("NULL", NA, Readmission$Complications)
Readmission$Postcode <- gsub("NULL", NA, Readmission$Postcode)
Readmission$Medicare.Eligibilty.Status <- gsub("NULL", NA, Readmission$Medicare.Eligibilty.Status)
Readmission$Suburb <- gsub("NULL", NA, Readmission$Suburb)
Readmission$Sex <- gsub("NULL", NA, Readmission$Sex)
Readmission$LOS <- gsub("NULL", NA, Readmission$LOS)
Readmission$Aboriginality.Description <- gsub("NULL", NA, Readmission$Aboriginality.Description)
Readmission$Intention_to_readmit <- gsub("NULL", NA, Readmission$Intention_to_readmit)
str(Readmission)

#convert column type
Readmission$ï..Episodes_Medical.Officer.Code.1 <- as.factor(Readmission$ï..Episodes_Medical.Officer.Code.1)
Readmission$Source.of.Referral.Desc <- as.factor(Readmission$Source.of.Referral.Desc)
Readmission$Facility.ID <- as.factor(Readmission$Facility.ID)
Readmission$DRG.Description <- as.factor( Readmission$DRG.Description)
Readmission$DschWard <- as.factor( Readmission$DschWard)
Readmission$AdmWard <- as.factor( Readmission$AdmWard)
Readmission$Funded <- as.factor( Readmission$Funded)
Readmission$Mode.of.Separation.Desc <- as.factor( Readmission$Mode.of.Separation.Desc)
Readmission$SRG.Desc <- as.factor(Readmission$SRG.Desc)
Readmission$LastEpisode <- as.factor( Readmission$LastEpisode)
Readmission$AgeGroup <- as.factor( Readmission$AgeGroup)
Readmission$ED.Mode.of.Arrival.Desc <- as.factor( Readmission$ED.Mode.of.Arrival.Desc)
Readmission$PPH.Category <- as.factor( Readmission$PPH.Category)
Readmission$Episodes_PDxDesc <- as.factor( Readmission$Episodes_PDxDesc)
Readmission$HadPPH <- as.factor( Readmission$HadPPH)
Readmission$HadMH <- as.factor( Readmission$HadMH)
Readmission$Complications <- as.factor(Readmission$Complications)
Readmission$Postcode <- as.factor(Readmission$Postcode)
Readmission$Medicare.Eligibilty.Status <- as.factor( Readmission$Medicare.Eligibilty.Status)
Readmission$Suburb <- as.factor( Readmission$Suburb)
Readmission$Sex <- as.factor( Readmission$Sex)
Readmission$LOS <- as.integer( Readmission$LOS)
Readmission$Aboriginality.Description <- as.factor( Readmission$Aboriginality.Description)
Readmission$Intention_to_readmit <- as.factor( Readmission$Intention_to_readmit)
Readmission$unplanned_readmit_28_Days <- as.logical(Readmission$unplanned_readmit_28_Days)
rxGetVarInfo(Readmission)

# Readmission$unplanned_readmit_28_Days <- as.factor(Readmission$unplanned_readmit_28_Days)

#split training data set and test data set
set.seed(11)
ss <- sample(1:nrow(Readmission),size = 0.7*nrow(Readmission))
Readmission.Train <- Readmission[ss,]
Readmission.Test <- Readmission[-ss,]
rxGetVarInfo(Readmission.Train)
rxGetVarInfo(Readmission.Test)

#Balance training data set
library(ROSE)
table(Readmission.Train$unplanned_readmit_28_Days)
Readmission.Train$unplanned_readmit_28_Days <-as.factor(Readmission.Train$unplanned_readmit_28_Days)
str(Readmission.Train)#convert response variable to factor to do sampling

data_balanced_rose <- ROSE(unplanned_readmit_28_Days ~ ., data = Readmission.Train, N=nrow(Readmission.Train),seed = 11)$data
table(data_balanced_rose$unplanned_readmit_28_Days)
data_balanced_both <- ovun.sample(unplanned_readmit_28_Days ~ ., data = Readmission.Train, method="both",N=nrow(Readmission.Train),p=0.5,seed = 1)$data
table(data_balanced_both$unplanned_readmit_28_Days)
# #sample size too small for under and over methods
# data_balanced_under <- ovun.sample(unplanned_readmit_28_Days ~ ., data = Readmission.Train, method="under",p=0.5,seed = 1)$data
# table(data_balanced_under$unplanned_readmit_28_Days)
# data_balanced_over <- ovun.sample(unplanned_readmit_28_Days ~ ., data = Readmission.Train, method="over",p=0.5,seed = 1)$data
# table(data_balanced_over$unplanned_readmit_28_Days)

data_balanced_rose$unplanned_readmit_28_Days <- as.logical(data_balanced_rose$unplanned_readmit_28_Days)#conver response variable back to ligical to perform machine learning algorithms
data_balanced_both$unplanned_readmit_28_Days <- as.logical(data_balanced_both$unplanned_readmit_28_Days)
# data_balanced_under$unplanned_readmit_28_Days <- as.logical(data_balanced_under$unplanned_readmit_28_Days)
# data_balanced_over$unplanned_readmit_28_Days <- as.logical(data_balanced_over$unplanned_readmit_28_Days)
Readmission.Train$unplanned_readmit_28_Days <- as.logical(Readmission.Train$unplanned_readmit_28_Days)