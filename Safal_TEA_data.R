###***TEA Data_2017***=======================================================================================================================
library('fpc')
install.packages('factoextra')
library('factoextra')
library(dplyr)
library('magrittr')
require(gridExtra)
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
TEA_Data[is.na(TEA_Data)] <- 0
any(is.na(TEA_Data))
is.factor(TEA_Data$C_RATING)
print(table(TEA_Data$C_RATING))
print(prop.table(table(TEA_Data$C_RATING)))

TEA_Data<-subset(TEA_Data, CI1>0) #exclude values with CI1=0
TEA_Data<-subset(TEA_Data, CI2>0) #exclude values with CI2=0
TEA_Data<-subset(TEA_Data, CI3>0) #exclude values with CI3=0
TEA_Data<-subset(TEA_Data, CI4>0) #exclude values with CI4=0

Campus2017Rating<-TEA_Data$C_RATING
target<-TEA_Data$C_RATING

num_data <- sapply(TEA_Data, is.numeric) #seperate numeric variables
TEA_Data <- TEA_Data[ , num_data] #keep only numeric variables
head(TEA_Data)
str(TEA_Data)
length(TEA_Data)
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
} #create a normalization function

TEA_Data$C_UPDATE<-NULL ###Omit Campus 2017 Final Rating Update
length(TEA_Data)
head(TEA_Data)
for_clust_data  <- as.data.frame(lapply(TEA_Data[ ,c(8,12,16,20)], normalize)) #normalize data , keep only 4 variables
head(for_clust_data)
km1 = kmeans(for_clust_data, 4, nstart=500)

df=for_clust_data
m=as.matrix(cbind(df$CI1, df$CI2),ncol=2)
km1$size
km1$withinss
df$cluster=factor(km1$cluster)
centers<-as.data.frame(km1$centers)
for_clust_data<-cbind(for_clust_data,target)
###Plot Clusters VS. Original Rating
plot1<-ggplot(data=df, aes(x=CI2, y=CI1, color=cluster )) + geom_text(aes(label=ifelse(Campus2017Rating=="Z",'Z', "")),hjust=0,colour = "black")+geom_text(aes(label=ifelse(Campus2017Rating=="X",'X', "")),hjust=0,colour = "black")+geom_text(aes(label=ifelse(Campus2017Rating=="T",'T', "")),hjust=0,colour = "black")+
  geom_point()+ geom_point(data=centers, aes(x=CI2,y=CI1, color='Center',show.legend=F)) +
  geom_point(data=centers, aes(x=CI2,y=CI1, color='Center'), show.legend=F,size=52, alpha=.3)
plot2<-ggplot(for_clust_data, aes(CI2, CI1, color = Campus2017Rating)) + geom_text(aes(label=ifelse(target=="Z",'Z', "")),hjust=0,colour = "black")+geom_text(aes(label=ifelse(target=="X",'X', "")),hjust=0,colour = "black")+geom_text(aes(label=ifelse(target=="T",'T', "")),hjust=0,colour = "black")+geom_point()

grid.arrange(plot1, plot2, ncol=2) 

pdf("TEA.pdf")
grid.arrange(plot1, plot2)
dev.off()
Box <- ggplot(for_clust_data, aes(target, CI1, fill = target)) + geom_boxplot() 


###==============================================================================================================


