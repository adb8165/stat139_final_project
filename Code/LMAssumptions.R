library(ggplot2)
library(nortest)
library(car)
library(boot)
library(rpart)
library(rpart.plot)
library(e1071)

# Import the data
bostondata = read.csv("/Users/Avery/Dropbox/Harvard/Stat139/stat139_final_project/Data/Property_Assessment_2014.csv",header = T)

# CLEANING
# Remove unused variables
bostondata = within(bostondata, rm(AV_LAND, AV_BLDG, GROSS_TAX))
bostondata = within(bostondata, rm(Parcel_ID, CM_ID))
bostondata = within(bostondata, rm(Owner_MAIL_ADDRESS, Owner_MAIL_CS, Owner_MAIL_ZIPCODE))
bostondata = within(bostondata, rm(full_address))

# Clean up the U_CORNER field
bostondata$U_CORNER_CLEAN <- (bostondata$U_CORNER == 'Y')
bostondata = within(bostondata, rm(U_CORNER))

# Clean U_ORIENT
bostondata$U_ORIENT_T <- (bostondata$U_ORIENT == 'T')
bostondata$U_ORIENT_F <- (bostondata$U_ORIENT == 'F')
bostondata$U_ORIENT_A <- (bostondata$U_ORIENT == 'A')
bostondata$U_ORIENT_B <- (bostondata$U_ORIENT == 'B')
bostondata$U_ORIENT_C <- (bostondata$U_ORIENT == 'C')
bostondata$U_ORIENT_M <- (bostondata$U_ORIENT == 'M')
bostondata = within(bostondata, rm(U_ORIENT))

# Clean U_HEAT_TYP
bostondata$U_HEAT_TYP_W <- (bostondata$U_HEAT_TYP == 'W')
bostondata$U_HEAT_TYP_F <- (bostondata$U_HEAT_TYP == 'F')
bostondata$U_HEAT_TYP_E <- (bostondata$U_HEAT_TYP == 'E')
bostondata$U_HEAT_TYP_P <- (bostondata$U_HEAT_TYP == 'P')
bostondata = within(bostondata, rm(U_HEAT_TYP))

head(bostondata)
names(bostondata)

fit = lm(AV_TOTAL~U_BDRMS+U_FPLACE+U_HALF_BTH+U_FULL_BTH+U_TOT_RMS+U_CORNER_CLEAN+
           U_ORIENT_T+U_ORIENT_F+U_ORIENT_A+U_ORIENT_B+U_ORIENT_C+U_ORIENT_M+
           U_HEAT_TYP_W+U_HEAT_TYP_F+U_HEAT_TYP_E+U_HEAT_TYP_P
           
           ,data=bostondata)
summary(fit)