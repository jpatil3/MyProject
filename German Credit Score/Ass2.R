install.packages("pacman")
pacman::p_load(pacman,party,psych,rio,tidyverse)
library(tidyselect)
library(pacman)
library(rio)
library(party)
library(readxl)

#Importing data from excel

gCred <- read_excel("German Credit.xls")
View(gCred)

#Exploring the various variables from dataset

summary(gCred)

head(gCred)

names(gCred)

dim(gCred)

str(gCred)



#converting to categorical variables

gCred$`OBS#` <- as.factor(gCred$`OBS#`)
gCred$CHK_ACCT <- as.factor(gCred$CHK_ACCT)
gCred$HISTORY <- as.factor(gCred$HISTORY)
gCred$NEW_CAR <- as.factor(gCred$NEW_CAR)
gCred$USED_CAR <- as.factor(gCred$USED_CAR)
gCred$FURNITURE <- as.factor(gCred$FURNITURE)
gCred$`RADIO/TV` <- as.factor(gCred$`RADIO/TV`)
gCred$EDUCATION <- as.factor(gCred$EDUCATION)
gCred$RETRAINING <- as.factor(gCred$RETRAINING)
gCred$SAV_ACCT <- as.factor(gCred$SAV_ACCT)
gCred$EMPLOYMENT <- as.factor(gCred$EMPLOYMENT)
gCred$MALE_DIV <- as.factor(gCred$MALE_DIV)
gCred$MALE_SINGLE <- as.factor(gCred$MALE_SINGLE)
gCred$MALE_MAR_or_WID <- as.factor(gCred$MALE_MAR_or_WID)
gCred$`CO-APPLICANT` <- as.factor(gCred$`CO-APPLICANT`)
gCred$GUARANTOR <- as.factor(gCred$GUARANTOR)
gCred$PRESENT_RESIDENT <- as.factor(gCred$PRESENT_RESIDENT)
gCred$REAL_ESTATE <- as.factor(gCred$REAL_ESTATE)
gCred$PROP_UNKN_NONE <- as.factor(gCred$PROP_UNKN_NONE)
gCred$OTHER_INSTALL <- as.factor(gCred$OTHER_INSTALL)
gCred$RENT <- as.factor(gCred$RENT)
gCred$OWN_RES <- as.factor(gCred$OWN_RES)
gCred$JOB <- as.factor(gCred$JOB)
gCred$TELEPHONE <- as.factor(gCred$TELEPHONE)
gCred$FOREIGN <- as.factor(gCred$FOREIGN)
gCred$RESPONSE <- as.factor(gCred$RESPONSE)

#standard deviations

sd(gCred$DURATION)
sd(gCred$AMOUNT)
sd(gCred$INSTALL_RATE)
sd(gCred$AGE)
sd(gCred$NUM_CREDITS)
sd(gCred$NUM_DEPENDENTS
   
mean(gCred$DURATION)   
mean(gCred$AMOUNT)
mean(gCred$INSTALL_RATE)
mean(gCred$AGE)
mean(gCred$NUM_CREDITS)

#Converting good and bad cases

gCred$RESPONSE <- as.factor(ifelse(gCred$RESPONSE == 1, "Good" , "Bad"))
tbl <- table(gCred$RESPONSE)
tbl_pct <- cbind(tbl,round(prop.table(tbl)*100,2))
colnames(tbl_pct)<- c('Count','Percentage')
knitr::kable(tbl_pct)









