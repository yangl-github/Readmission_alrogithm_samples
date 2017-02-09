#remove spaces or replace spaces in attribute domain values (charactor vector)

#read dataset
Readmission <- read.csv('C:/Users/Yang/Desktop/R project/BICG/Output.CSV')
#replace blank with NA
Readmission_NA <- read.csv('C:/Users/Yang/Desktop/R project/BICG/Output.CSV',na.strings = c(""," ","NA"))
a<-Readmission

#rename all the variables in the dataset
names(a)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y")

#replace space in domian values with "~"
install.packages("stringr")
library(stringr)
#remove space before and after
c<-a
c$B<-str_trim(c$B)

 #remove space between
b<-c
b$B<-gsub("[[:space:]]", "~", b$B)
b$D<-gsub("[[:space:]]", "~", b$D)
b$G<-gsub("[[:space:]]", "~", b$G)
b$H<-gsub("[[:space:]]", "~", b$H)
b$I<-gsub("[[:space:]]", "~", b$I)
b$K<-gsub("[[:space:]]", "~", b$K)
b$L<-gsub("[[:space:]]", "~", b$L)
b$N<-gsub("[[:space:]]", "~", b$N)
b$T<-gsub("[[:space:]]", "~", b$T)
b$W<-gsub("[[:space:]]", "~", b$W)

#save new dataset to .csv file
library(utils)
write.csv(b,file = "C:/Users/Yang/Desktop/R project/BICG/no space.csv")
# write.table(b,file = "C:/Users/Yang/Desktop/R project/BICG/no space.txt")
c <- read.csv('C:/Users/Yang/Desktop/R project/BICG/no space.CSV')

########important:check the variable type after this!