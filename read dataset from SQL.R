# read dataset from SQL
# conncet to SQL server
install.packages('RODBC')
library(RODBC)
# connStrSql <- "Driver=SQL Server;Server=localhost;Database=Test;Uid=sa;Pwd=pass@word1"
ch <- odbcConnect("Readmission", uid = "sa", pwd = "pass@word1")
p<-sqlQuery(ch,"SELECT [Episodes_Medical Officer Code 1]
            ,[Source of Referral Desc]
            ,[Facility ID]
            ,[DRG Description]
            ,[DschWard]
            ,[AdmWard]
            ,[Funded]
            ,[Mode of Separation Desc]
            ,[SRG Desc]
            ,[LastEpisode]
            ,[AgeGroup]
            ,[ED Mode of Arrival Desc]
            ,[PPH Category]
            ,[Episodes_PDxDesc]
            ,[HadPPH]
            ,[HadMH]
            ,[Complications]
            ,[Postcode]
            ,[Medicare Eligibilty Status]
            ,[Suburb]
            ,[Sex]
            ,[LOS]
            ,[Aboriginality Description]
            ,[Intention_to_readmit]
            ,[unplanned_readmit_28_Days]
            FROM [Test].[dbo].[Readmissions]")
Readmission <- p
str(Readmission)

#convert to factor variables
Readmission$`Medicare Eligibilty Status` <- as.factor(Readmission$`Medicare Eligibilty Status`)
Readmission$Intention_to_readmit <- as.factor(Readmission$Intention_to_readmit)
Readmission$unplanned_readmit_28_Days <- as.factor(Readmission$unplanned_readmit_28_Days)
str(Readmission)
n<-names(Readmission)