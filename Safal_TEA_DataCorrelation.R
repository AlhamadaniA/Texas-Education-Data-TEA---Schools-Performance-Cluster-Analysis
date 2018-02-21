###***TEA Data_2017***=======================================================================================================================
install.packages('factoextra')
install.packages("GGally")
library('fpc')
library('factoextra')
library(dplyr)
library('magrittr')
require(gridExtra)
require(caret)
library(missForest)
library(GGally)
library(ggplot2)

set.seed(1234)
####Read data
TEA_Data<-read.csv("TEA_2017_Accountability_Data.csv",header = T,na.strings = c(" ", "NA"))
head(TEA_Data)
dim(TEA_Data)
str(TEA_Data)
any(is.na(TEA_Data))
is.na(TEA_Data)
# identify location of NAs in vector
which(is.na(TEA_Data))
# identify count of NAs in each column
colSums(is.na(TEA_Data))
any(is.na(TEA_Data))
is.factor(TEA_Data$C_RATING)
print(table(TEA_Data$C_RATING))
print(prop.table(table(TEA_Data$C_RATING)))
###Specify Target Variable
Campus2017Rating<-TEA_Data$C_RATING
target<-TEA_Data$C_RATING

num_data <- sapply(TEA_Data, is.numeric) #seperate numeric variables
TEA_Data <- TEA_Data[ , num_data] #keep only numeric variables
head(TEA_Data)
dim(TEA_Data)
str(TEA_Data)
length(TEA_Data)
colnames(TEA_Data)
str(TEA_Data)
TEA_Data$C_UPDATE<-NULL ###Omit Campus 2017 Final Rating Update
TEA_Data$Campus<-NULL ###Omit Campus 2017 Final Rating Update
###Impute missing values with Miss Forest Using Random Forest:(Predicting missing values with high accuracy)
imputationResults <- missForest(TEA_Data)
dataMissForestImputed <- imputationResults$ximp

###Impute missing values with zero
TEA_Data[is.na(TEA_Data)] <- 0

###Find correlation
df2 = cor(TEA_Data)
cor(TEA_Data$CI1,TEA_Data$CI3)
hc = findCorrelation(df2, cutoff=0.6) #Set value as a "cutoff" 
hc = sort(hc)
reduced_Data = TEA_Data[,c(hc)] ###Keep only correlation values with .6 or higher
dim(reduced_Data)
cor(reduced_Data)
head(reduced_Data)
colnames(reduced_Data)
reduced_Data<-cbind(target,reduced_Data)
####Saved High Correlated Variables:
write.csv(reduced_Data, file = "TEA_2017_Accountability_DataReduced_Data.csv", row.names = FALSE)
reduced_Data<-read.csv("TEA_2017_Accountability_DataReduced_Data.csv",header=T)
###Heat Map visualie correlation
ggcorr(reduced_Data, label = TRUE,hjust = 0.75,size = 4)

####Make subset data for only important variables
reduced_Data<-subset(reduced_Data, select=c("CI1","CI2" ,"CI3","CI4" ,"CPEMALLT","CPETALLC","CPETECHP","CPETECOC","CPETLEPC"   ))

###Heat Map visualie correlation
ggcorr(reduced_Data, label = TRUE,hjust = 0.75,size = 4)

###Visualize Class distribution
Box <- ggplot(reduced_Data, aes(target, CI1, fill = target)) + geom_boxplot() 
Box

###==============================================================================================================


