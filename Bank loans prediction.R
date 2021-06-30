library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(caTools)
library(Metrics)
library(caret)


#importing the document
status<-read.csv("C:\\Users\\Sharath KT\\Documents\\train.csv",header = T, stringsAsFactors = F)
head(status)
str(status)
summary(status)
  
#preprocessing the data 
colSums(is.na(status))

status$Credit.Score<-ifelse(is.na(status$Credit.Score),median(status$Credit.Score,na.rm = T),status$Credit.Score)

status$Annual.Income<-ifelse(is.na(status$Annual.Income),mean(status$Annual.Income,na.rm = T),status$Annual.Income)

status$Months.since.last.delinquent<-ifelse(is.na(status$Months.since.last.delinquent),min(status$Months.since.last.delinquent,na.rm = T),status$Months.since.last.delinquent)

status<-na.omit(status)

colSums(is.na(status))


#some visualizations
#loans based on house ownership
ggplot(status,aes(x=Loan.Status,group=Home.Ownership))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~Home.Ownership)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Loan Status",y="Percentage",title="Annual Inc vs. Loan status")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("frequencies") +
  scale_fill_brewer(palette="Set1")


#loan based on terms
ggplot(status,aes(x=Loan.Status,group=Term))+
  geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
  facet_grid(~Term)+
  theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
  labs(x="Loan Status",y="Percentage",title="Annual Inc vs. Loan status")+
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
  scale_y_continuous(labels=scales::percent) +
  ylab("frequencies") +
  scale_fill_brewer(palette="Set2")


#removing columns which are not required
status1<-status

status1<-status1[,!(names(status1)%in% c("Loan.ID","Years.of.Credit.History"))]

str(status1)

#Transformation
#converting certain columns to fators as they are catagorical 
status1$Term<-as.factor(status1$Term)
status1$Home.Ownership<-as.factor(status1$Home.Ownership)
status1$Bankruptcies<-as.factor(status1$Bankruptcies)
status1$Loan.Status<-as.factor(status1$Loan.Status)
status1$Purpose<-as.factor(status1$Purpose)
str(status1)

#modelling
#split the data into test and train with a ratio of 70:30

set.seed(555)
S<-sample.split(status1$Loan.Status, SplitRatio = 0.70)
train<-subset(status1,S==T)
test<-subset(status1,S==F)
nrow(train)
nrow(test)

#logistic Model

X<-glm(Loan.Status~., data = train, family = binomial)
summary(X)

pred<-predict(X, newdata = test, type = "response")

range(pred)

#evaluation 
Y<-table(test$Loan.Status,pred>0.25)
Y

#accuracy
A<-(850+13824)/(850+3529+13824+0)
A
