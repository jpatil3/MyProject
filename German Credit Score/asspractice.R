#Correlation plot 

#install.packages("pacman")
#install.packages("funModeling")
#install.packages("reshape2")
#install.packages("corrplot")
#install.packages("apaTables")
pacman::p_load(pacman,party,psych,rio,tidyverse)
library(reshape2)
library(apaTables)
library(tidyselect)
library(dplyr)
library(pacman)
library(rio)
library(party)
library(ggplot2)
library(readxl)
library(corrplot)

#Importing data from excel

gCred <- read_excel("German Credit.xls")
View(gCred)



summary(gCred)
head(gCred)
names(gCred)
dim(gCred)
str(gCred)



sd(gCred$DURATION)
sd(gCred$AMOUNT)
sd(gCred$INSTALL_RATE)
sd(gCred$AGE)
sd(gCred$NUM_CREDITS)
sd(gCred$NUM_DEPENDENTS)

mean(gCred$DURATION)   
mean(gCred$AMOUNT)
mean(gCred$INSTALL_RATE)
mean(gCred$AGE)
mean(gCred$NUM_CREDITS)

## Good case bad case

gCred$RESPONSE <- as.factor(ifelse(gCred$RESPONSE == 1, "Good" , "Bad"))
table1 <- table(gCred$RESPONSE)
table_pt <- cbind(table1,round(prop.table(table1)*100,2))
colnames(table_pt)<- c('Count','Percentage')
knitr::kable(table_pt)

## Removing redundent datae


gCred$NEW_CAR <-  replace(gCred$NEW_CAR, gCred$NEW_CAR == 1 , "NEW_CAR" )
gCred$NEW_CAR <-  replace(gCred$NEW_CAR, gCred$NEW_CAR == 0 , "" )
gCred$USED_CAR <-  replace(gCred$USED_CAR, gCred$USED_CAR == 1 , "USED_CAR" )
gCred$USED_CAR <-  replace(gCred$USED_CAR, gCred$USED_CAR == 0 , "" )
gCred$FURNITURE <-  replace(gCred$FURNITURE, gCred$FURNITURE == 1 , "FURNITURE" )
gCred$FURNITURE <-  replace(gCred$FURNITURE, gCred$FURNITURE == 0 , "" )
gCred$`RADIO/TV` <-  replace(gCred$`RADIO/TV`, gCred$`RADIO/TV` == 1 , "RADIO/TV" )
gCred$`RADIO/TV` <-  replace(gCred$`RADIO/TV`, gCred$`RADIO/TV` == 0 , "" )
gCred$EDUCATION <-  replace(gCred$EDUCATION, gCred$EDUCATION == 1 , "EDUCATION" )
gCred$EDUCATION <-  replace(gCred$EDUCATION, gCred$EDUCATION == 0 , "" )
gCred$RETRAINING <-  replace(gCred$RETRAINING, gCred$RETRAINING == 1 , "RETRAINING")
gCred$RETRAINING <-  replace(gCred$RETRAINING, gCred$RETRAINING == 0 , "")
gCred$PURPOSE <- paste(gCred$NEW_CAR,gCred$USED_CAR,gCred$FURNITURE,gCred$`RADIO/TV`,gCred$EDUCATION,gCred$RETRAINING)
gCred$PURPOSE <- trimws(gCred$PURPOSE)


gCred<- gCred [ , -which(names(gCred) %in% c("NEW_CAR" , "USED_CAR" , "FURNITURE", "RADIO/TV","EDUCATION","RETRAINING","TELEPHONE","FOREIGN"))]


##Graphs


gCred$CHK_ACCT <- as.numeric(gCred$CHK_ACCT)
gCred$HISTORY <- as.numeric(gCred$HISTORY)
gCred$SAV_ACCT <- as.numeric(gCred$SAV_ACCT)
gCred$EMPLOYMENT <- as.numeric(gCred$EMPLOYMENT)
gCred$RESPONSE <- as.numeric(gCred$RESPONSE)
gCred$PRESENT_RESIDENT <- as.numeric(gCred$PRESENT_RESIDENT)
gCred$JOB <- as.numeric(gCred$JOB)

df1 <- gCred %>% select(c(CHK_ACCT,HISTORY,SAV_ACCT,EMPLOYMENT,PRESENT_RESIDENT,JOB,RESPONSE))
df1
df.cor = cor(df1)
corPlot(df.cor)
apa.cor.table(df1,"APA correlation Table.doc")



attach(gCred)
plot(RESPONSE, AGE, main="Age vs Response",
     xlab="Age ", ylab="Response", pch=19)

attach(gCred)
plot(RESPONSE, CHK_ACCT, main="CHK_ACCT vs Response",
     xlab="Response ", ylab="CHK_ACCT", pch=19)



### Plotting the graphs##########

p<-ggplot(data=gCred, aes(x=JOB, y=RESPONSE, fill=RESPONSE)) +
          geom_bar(stat="identity")
p

p1<-ggplot(data=gCred, aes(x=CHK_ACCT, y=RESPONSE, fill=RESPONSE)) +
          geom_bar(stat="identity")
p1

p2<-ggplot(data=gCred, aes(x=HISTORY, y=RESPONSE, fill=RESPONSE)) +
          geom_bar(stat="identity")
p2

p3<-ggplot(data=gCred, aes(x=SAV_ACCT, y=RESPONSE, fill=RESPONSE)) +
          geom_bar(stat="identity")
p3

p4<-ggplot(data=gCred, aes(y=EMPLOYMENT, x=RESPONSE, fill=EMPLOYMENT)) +
          geom_bar(stat="identity")
p4

p5<-ggplot(data=gCred, aes(x=PRESENT_RESIDENT, y=RESPONSE, fill=RESPONSE)) +
          geom_bar(stat="identity")
























