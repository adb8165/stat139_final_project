library(ggplot2)
library(nortest)
library(car)
library(boot)
library(rpart)
library(e1071)
library(nnet)
library(parallel)
library(randomForest)
#library(neuralnet)
#library(Metrics)
library(MASS)
library(caret)
library(pROC)


# Import the data
bostondata = read.csv("../Data/Property_Assessment_2014.csv",header = T)

# CLEANING
# Remove unused variables
bostondata = within(bostondata, rm(AV_LAND, AV_BLDG, GROSS_TAX))
bostondata = within(bostondata, rm(Parcel_ID, CM_ID))
bostondata = within(bostondata, rm(Owner_MAIL_ADDRESS, Owner_MAIL_CS, Owner_MAIL_ZIPCODE))
bostondata = within(bostondata, rm(full_address))

# Not sure if there is anything useful in this guy
bostondata = within(bostondata, rm(OWNER))
bostondata = within(bostondata, rm(S_UNIT_COM, S_UNIT_RC))

# Clean up silly entries
bostondata$AV_TOTAL[bostondata$AV_TOTAL==0|bostondata$AV_TOTAL> 50000000] = NA

bostondata$LU_CLEAN = factor(bostondata$LU,levels=c('R1','R2','R3','R4','RL','A',"RC","CM","CD","CP","CC","AH","C","CL","I","E","EA"))
bostondata = within(bostondata, rm(LU))

bostondata$U_ORIENT_CLEAN = factor(bostondata$U_ORIENT,levels=c('T','F','A','B','C','M'))
bostondata = within(bostondata, rm(U_ORIENT))

# BROKEN ROWS
#bostondata$R_BLDG_STYL_CLEAN = factor(bostondata$R_BLDG_STYL,levels=c('CL','TF','DK','CV','RM','SD','CP','RN','RE','BW','DX','RR','SL','VT'))
#bostondata = within(bostondata, rm(R_BLDG_STYL))

#bostondata$STRUCTURE_CLASS_CLEAN = factor(bostondata$STRUCTURE_CLASS,levels=c('R','C','D','B','A'))
#summary(bostondata$STRUCTURE_CLASS)
#bostondata = within(bostondata, rm(STRUCTURE_CLASS))

#bostondata$R_ROOF_TYP_CLEAN = factor(bostondata$R_ROOF_TYP,levels=c('G','F','H','M','L'))
#summary(bostondata$R_ROOF_TYP)
#bostondata = within(bostondata, rm(R_ROOF_TYP))

#bostondata$R_EXT_FIN_CLEAN = factor(bostondata$R_EXT_FIN,levels=c('M', 'W','B','F','A','P','S', 'NA'), exclude = NA)
#summary(bostondata$R_EXT_FIN_CLEAN)
#bostondata = within(bostondata, rm(R_EXT_FIN))
unique(bostondata$PTYPE)
bostondata$PTYPE_CLEAN = factor(bostondata$PTYPE,levels=c(102,101,104,105,995,132,111,985,108,902,13,112,390,130,106,125,357,31,986,358,907,319,320,337,905,977,908,332))
bostondata = within(bostondata, rm(PTYPE))

bostondata$OWN_OCC_CLEAN = factor(bostondata$OWN_OCC, levels = c("Y","N"))
bostondata = within(bostondata, rm(OWN_OCC))

bostondata$GROSS_AREA[bostondata$GROSS_AREA == 0] = NA
bostondata$LIVING_AREA[bostondata$LIVING_AREA == 0] = NA

bostondata$NUM_FLOORS_CLEAN = factor(bostondata$NUM_FLOORS, levels=c('1','1.5','2','2.5','3','3.5','4','4.5','5','6'))
bostondata = within(bostondata, rm(NUM_FLOORS))

bostondata$LAND_SF[bostondata$LAND_SF == 0| bostondata$LAND_SF > 10000000] = NA
bostondata$YR_BUILT[bostondata$YR_BUILT < 1000 | bostondata$YR_BUILT == 0 | bostondata$YR_BUILT>2014] = NA
bostondata$YR_REMOD[bostondata$YR_REMOD < 1000 | bostondata$YR_REMOD == 0 | bostondata$YR_REMOD>2014] = 1994

bostondata = na.omit(bostondata)
lengths(bostondata)

# Clean up ST_NUM
bostondata$ST_NUM_15 <- (bostondata$ST_NUM == 15)
bostondata$ST_NUM_1 <- (bostondata$ST_NUM == 1)
bostondata$ST_NUM_10 <- (bostondata$ST_NUM == 10)
bostondata$ST_NUM_9 <- (bostondata$ST_NUM == 9)
bostondata$ST_NUM_2 <- (bostondata$ST_NUM == 2)
bostondata$ST_NUM_11 <- (bostondata$ST_NUM == 11)
bostondata$ST_NUM_8 <- (bostondata$ST_NUM == 8)
bostondata$ST_NUM_6 <- (bostondata$ST_NUM == 6)
bostondata$ST_NUM_7 <- (bostondata$ST_NUM == 7)
bostondata$ST_NUM_12 <- (bostondata$ST_NUM == 12)
bostondata$ST_NUM_5 <- (bostondata$ST_NUM == 5)
bostondata$ST_NUM_19 <- (bostondata$ST_NUM == 19)
bostondata$ST_NUM_21 <- (bostondata$ST_NUM == 21)
bostondata$ST_NUM_20 <- (bostondata$ST_NUM == 20)
bostondata$ST_NUM_16 <- (bostondata$ST_NUM == 16)
bostondata = within(bostondata, rm(ST_NUM))

# Clean up UNIT_NUM
bostondata$UNIT_NUM_1 <- (bostondata$UNIT_NUM == 1)
bostondata$UNIT_NUM_2 <- (bostondata$UNIT_NUM == 2)
bostondata$UNIT_NUM_3 <- (bostondata$UNIT_NUM == 3)
bostondata$UNIT_NUM_4 <- (bostondata$UNIT_NUM == 4)
bostondata$UNIT_NUM_5 <- (bostondata$UNIT_NUM == 5)
bostondata$UNIT_NUM_6 <- (bostondata$UNIT_NUM == 6)
bostondata$UNIT_NUM_7 <- (bostondata$UNIT_NUM == 7)
bostondata$UNIT_NUM_8 <- (bostondata$UNIT_NUM == 8)
bostondata$UNIT_NUM_9 <- (bostondata$UNIT_NUM == 9)
bostondata = within(bostondata, rm(UNIT_NUM))

# Clean up ST_NAME
bostondata$ST_NAME_COMMONWEALTH <- (bostondata$ST_NAME == 'COMMONWEALTH')
bostondata$ST_NAME_BEACON <- (bostondata$ST_NAME == 'BEACON')
bostondata$ST_NAME_WASHINGTON <- (bostondata$ST_NAME == 'WASHINGTON')
bostondata$ST_NAME_TREMONT <- (bostondata$ST_NAME == 'TREMONT')
bostondata$ST_NAME_DORCHESTER <- (bostondata$ST_NAME == 'DORCHESTER')
bostondata$ST_NAME_MARLBOROUGH <- (bostondata$ST_NAME == 'MARLBOROUGH')
bostondata$ST_NAME_CENTRE <- (bostondata$ST_NAME == 'CENTRE')
bostondata$ST_NAME_PARK <- (bostondata$ST_NAME == 'PARK')
bostondata$ST_NAME_HAWTHORNE <- (bostondata$ST_NAME == 'HAWTHORNE')
bostondata$ST_NAME_MASSACHUSETTS <- (bostondata$ST_NAME == 'MASSACHUSETTS')
bostondata$ST_NAME_COLUMBUS <- (bostondata$ST_NAME == 'COLUMBUS')
bostondata$ST_NAME_ADAMS <- (bostondata$ST_NAME == 'ADAMS')
bostondata$ST_NAME_HYDE_PARK <- (bostondata$ST_NAME == 'HYDE PARK')
bostondata$ST_NAME_BOYLSTON <- (bostondata$ST_NAME == 'BOYLSTON')
bostondata$ST_NAME_COMMERCIAL <- (bostondata$ST_NAME == 'COMMERCIAL')
bostondata$ST_NAME_NEWBURY <- (bostondata$ST_NAME == 'NEWBURY')
bostondata$ST_NAME_SOUTH <- (bostondata$ST_NAME == 'SOUTH')
bostondata$ST_NAME_HARVARD <- (bostondata$ST_NAME == 'HARVARD')
bostondata$ST_NAME_MT_VERNON <- (bostondata$ST_NAME == 'MT VERNON')
bostondata$ST_NAME_CHESTNUT <- (bostondata$ST_NAME == 'CHESTNUT')
bostondata$ST_NAME_RIVER <- (bostondata$ST_NAME == 'RIVER')
bostondata$ST_NAME_E_INDIA <- (bostondata$ST_NAME == 'E INDIA')
bostondata$ST_NAME_SARATOGA <- (bostondata$ST_NAME == 'SARATOGA')
bostondata$ST_NAME_EIGHTH <- (bostondata$ST_NAME == 'EIGHTH')
bostondata$ST_NAME_WARREN <- (bostondata$ST_NAME == 'WARREN')
bostondata$ST_NAME_BLUE_HILL <- (bostondata$ST_NAME == 'BLUE HILL')
bostondata$ST_NAME_HARRISON <- (bostondata$ST_NAME == 'HARRISON')
bostondata$ST_NAME_HUNTINGTON <- (bostondata$ST_NAME == 'HUNTINGTON')
bostondata$ST_NAME_SHAWMUT <- (bostondata$ST_NAME == 'SHAWMUT')
bostondata$ST_NAME_E_BROADWAY <- (bostondata$ST_NAME == 'E BROADWAY')
bostondata$ST_NAME_WHITTIER <- (bostondata$ST_NAME == 'WHITTIER')
bostondata$ST_NAME_BENNINGTON <- (bostondata$ST_NAME == 'BENNINGTON')
bostondata = within(bostondata, rm(ST_NAME))

# Clean up ST_NAME_SUF
bostondata$ST_NAME_SUF_AV <- (bostondata$ST_NAME_SUF == 'AV')
bostondata$ST_NAME_SUF_BL <- (bostondata$ST_NAME_SUF == 'BL')
bostondata$ST_NAME_SUF_CI <- (bostondata$ST_NAME_SUF == 'CI')
bostondata$ST_NAME_SUF_CT <- (bostondata$ST_NAME_SUF == 'CT')
bostondata$ST_NAME_SUF_DR <- (bostondata$ST_NAME_SUF == 'DR')
bostondata$ST_NAME_SUF_HW <- (bostondata$ST_NAME_SUF == 'HW')
bostondata$ST_NAME_SUF_LA <- (bostondata$ST_NAME_SUF == 'LA')
bostondata$ST_NAME_SUF_PK <- (bostondata$ST_NAME_SUF == 'PK')
bostondata$ST_NAME_SUF_PL <- (bostondata$ST_NAME_SUF == 'PL')
bostondata$ST_NAME_SUF_PW <- (bostondata$ST_NAME_SUF == 'PW')
bostondata$ST_NAME_SUF_RD <- (bostondata$ST_NAME_SUF == 'RD')
bostondata$ST_NAME_SUF_SQ <- (bostondata$ST_NAME_SUF == 'SQ')
bostondata$ST_NAME_SUF_ST <- (bostondata$ST_NAME_SUF == 'ST')
bostondata$ST_NAME_SUF_TE <- (bostondata$ST_NAME_SUF == 'TE')
bostondata$ST_NAME_SUF_WY <- (bostondata$ST_NAME_SUF == 'WY')
bostondata = within(bostondata, rm(ST_NAME_SUF))

# Clean up the U_CORNER field
bostondata$U_CORNER_CLEAN <- (bostondata$U_CORNER == 'Y')
bostondata = within(bostondata, rm(U_CORNER))

# Clean U_HEAT_TYP
bostondata$U_HEAT_TYP_W <- (bostondata$U_HEAT_TYP == 'W')
bostondata$U_HEAT_TYP_F <- (bostondata$U_HEAT_TYP == 'F')
bostondata$U_HEAT_TYP_E <- (bostondata$U_HEAT_TYP == 'E')
bostondata$U_HEAT_TYP_P <- (bostondata$U_HEAT_TYP == 'P')
bostondata = within(bostondata, rm(U_HEAT_TYP))

# Clean ZIPCODE
bostondata$ZIPCODE_02116 <- (bostondata$ZIPCODE== '02116')
bostondata$ZIPCODE_02135 <- (bostondata$ZIPCODE== '02135')
bostondata$ZIPCODE_02127 <- (bostondata$ZIPCODE== '02127')
bostondata$ZIPCODE_02118 <- (bostondata$ZIPCODE== '02118')
bostondata$ZIPCODE_02130 <- (bostondata$ZIPCODE== '02130')
bostondata$ZIPCODE_02129 <- (bostondata$ZIPCODE== '02129')
bostondata$ZIPCODE_02115 <- (bostondata$ZIPCODE== '02115')
bostondata$ZIPCODE_02114 <- (bostondata$ZIPCODE== '02114')
bostondata$ZIPCODE_02215 <- (bostondata$ZIPCODE== '02215')
bostondata$ZIPCODE_02134 <- (bostondata$ZIPCODE== '02134')
bostondata$ZIPCODE_02125 <- (bostondata$ZIPCODE== '02125')
bostondata$ZIPCODE_02111 <- (bostondata$ZIPCODE== '02111')
bostondata$ZIPCODE_02131 <- (bostondata$ZIPCODE== '02131')
bostondata$ZIPCODE_02124 <- (bostondata$ZIPCODE== '02124')
bostondata$ZIPCODE_02128 <- (bostondata$ZIPCODE== '02128')
bostondata$ZIPCODE_02109 <- (bostondata$ZIPCODE== '02109')
bostondata$ZIPCODE_02122 <- (bostondata$ZIPCODE== '02122')
bostondata$ZIPCODE_02113 <- (bostondata$ZIPCODE== '02113')
bostondata$ZIPCODE_02132 <- (bostondata$ZIPCODE== '02132')
bostondata$ZIPCODE_02110 <- (bostondata$ZIPCODE== '02110')
bostondata$ZIPCODE_02108 <- (bostondata$ZIPCODE== '02108')
bostondata$ZIPCODE_02119 <- (bostondata$ZIPCODE== '02119')
bostondata$ZIPCODE_02467 <- (bostondata$ZIPCODE== '02467')
bostondata = within(bostondata, rm(ZIPCODE))

# This removes rows with NAs in them
bostondata = na.omit(bostondata)

#head(bostondata)
#names(bostondata)
#sort(table(bostondata$YR_BUILT))

#summary(lm(AV_TOTAL ~  STRUCTURE_CLASS_CLEAN , data=bostondata))

# Rows that break the model :(
# STRUCTURE_CLASS_CLEAN + R_BLDG_STYL_CLEAN + R_ROOF_TYP_CLEAN +R_EXT_FIN_CLEAN +
# R_HEAT_TYP + R_AC +

dataclean1 = newdata <- subset(bostondata, select=c(AV_TOTAL,
                                                    LU_CLEAN,GROSS_AREA,LIVING_AREA,LAND_SF,YR_BUILT,YR_REMOD,
                                                    OWN_OCC_CLEAN, NUM_FLOORS_CLEAN, 
                                                    ST_NUM_15, ST_NUM_1, ST_NUM_10, ST_NUM_9, ST_NUM_2,
                                                    ST_NUM_11, ST_NUM_8, ST_NUM_6, ST_NUM_7, ST_NUM_12, ST_NUM_5, ST_NUM_19,
                                                    ST_NUM_21, ST_NUM_20, ST_NUM_16,
                                                    UNIT_NUM_1, UNIT_NUM_2, UNIT_NUM_3, UNIT_NUM_4, UNIT_NUM_5,
                                                    UNIT_NUM_6, UNIT_NUM_7, UNIT_NUM_8, UNIT_NUM_9,
                                                    ST_NAME_SUF_AV, ST_NAME_SUF_BL, ST_NAME_SUF_CI, ST_NAME_SUF_CT, ST_NAME_SUF_DR,
                                                    ST_NAME_SUF_HW, ST_NAME_SUF_LA, ST_NAME_SUF_PK, ST_NAME_SUF_PL, ST_NAME_SUF_PW,
                                                    ST_NAME_SUF_RD, ST_NAME_SUF_SQ, ST_NAME_SUF_ST, ST_NAME_SUF_TE, ST_NAME_SUF_WY, 
                                                    
                                                    ZIPCODE_02115, ZIPCODE_02114, ZIPCODE_02215, ZIPCODE_02134, ZIPCODE_02125,
                                                    ZIPCODE_02111, ZIPCODE_02131, ZIPCODE_02124, ZIPCODE_02128, ZIPCODE_02109,
                                                    ZIPCODE_02122, ZIPCODE_02113, ZIPCODE_02132, ZIPCODE_02110, ZIPCODE_02108,
                                                    ZIPCODE_02119, ZIPCODE_02467,
                                                    
                                                    ST_NAME_COMMONWEALTH, ST_NAME_BEACON, ST_NAME_WASHINGTON, ST_NAME_TREMONT,
                                                    ST_NAME_DORCHESTER, ST_NAME_MARLBOROUGH, ST_NAME_CENTRE, ST_NAME_PARK,
                                                    ST_NAME_HAWTHORNE, ST_NAME_MASSACHUSETTS, ST_NAME_COLUMBUS, ST_NAME_ADAMS,
                                                    ST_NAME_HYDE_PARK, ST_NAME_BOYLSTON, ST_NAME_COMMERCIAL, ST_NAME_NEWBURY,
                                                    ST_NAME_SOUTH, ST_NAME_HARVARD, ST_NAME_MT_VERNON, ST_NAME_CHESTNUT,
                                                    ST_NAME_RIVER, ST_NAME_E_INDIA, ST_NAME_SARATOGA, ST_NAME_EIGHTH, ST_NAME_WARREN,
                                                    ST_NAME_BLUE_HILL, ST_NAME_HARRISON, ST_NAME_HUNTINGTON, ST_NAME_SHAWMUT, 
                                                    ST_NAME_E_BROADWAY, ST_NAME_WHITTIER, ST_NAME_BENNINGTON,
                                                    U_BDRMS, U_FPLACE, U_HALF_BTH, U_FULL_BTH, U_TOT_RMS, U_CORNER_CLEAN,
                                                    U_ORIENT_CLEAN, U_BASE_FLOOR,
                                                    U_HEAT_TYP_W, U_HEAT_TYP_F, U_HEAT_TYP_E, U_HEAT_TYP_P))


# Iteratively Remove variables with high vif scores
dataclean2 = dataclean1
dataclean2$U_HEAT_TYP_W <- NULL


dataclean2$GROSS_AREA <- NULL


dataclean2$LIVING_AREA <- NULL


dataclean2$ST_NAME_SUF_ST <- NULL


dataclean3 = dataclean2
dataclean3$YR_REMOD2 = dataclean2$YR_REMOD^2
dataclean3$YR_REMOD <- NULL
dataclean3$YR_BUILT3 = dataclean2$YR_BUILT^3
dataclean3$YR_BUILT <- NULL
dataclean3$LAND_SF_LOG = log(dataclean2$LAND_SF)
dataclean3$LAND_SF <- NULL
dataclean3$AV_TOTAL_LOG = log(dataclean2$AV_TOTAL)
dataclean3$AV_TOTAL <- NULL
fit5 = lm(AV_TOTAL_LOG~.,data=dataclean3)
dataclean3[cooks.distance(fit5) > .02,]
#67467 and 133906 have more rooms than U_TOT_RMS, so discard them
influential_points = cooks.distance(fit5) > .04
dataclean3 = dataclean3[!influential_points,]
dataclean4=subset(dataclean3,select=c(AV_TOTAL_LOG, LU_CLEAN,OWN_OCC_CLEAN , NUM_FLOORS_CLEAN , 
                                      ST_NUM_15 , ST_NUM_1 , ST_NUM_10 , ST_NUM_9 , ST_NUM_2 , 
                                      ST_NUM_7 , ST_NUM_12 , ST_NUM_5 , ST_NUM_19 , ST_NUM_21 , 
                                      ST_NUM_20, ST_NUM_16 , UNIT_NUM_1 , UNIT_NUM_2 , UNIT_NUM_3 , 
                                      UNIT_NUM_4 , UNIT_NUM_5 , UNIT_NUM_6 , ST_NAME_SUF_AV , ST_NAME_SUF_BL , 
                                      ST_NAME_SUF_CI , ST_NAME_SUF_CT , ST_NAME_SUF_DR , ST_NAME_SUF_HW , 
                                      ST_NAME_SUF_LA , ST_NAME_SUF_PK , ST_NAME_SUF_PL , ST_NAME_SUF_PW , 
                                      ST_NAME_SUF_RD , ST_NAME_SUF_SQ , ST_NAME_SUF_TE , ST_NAME_SUF_WY , 
                                      ZIPCODE_02115 , ZIPCODE_02114 , ZIPCODE_02215 , ZIPCODE_02134 , 
                                      ZIPCODE_02125 , ZIPCODE_02131 , ZIPCODE_02124 , ZIPCODE_02128 , 
                                      ZIPCODE_02109 , ZIPCODE_02122 , ZIPCODE_02113 , ZIPCODE_02132 , 
                                      ZIPCODE_02110 , ZIPCODE_02108 , ZIPCODE_02119 , ZIPCODE_02467 , 
                                      ST_NAME_COMMONWEALTH , ST_NAME_BEACON , ST_NAME_WASHINGTON , 
                                      ST_NAME_TREMONT , ST_NAME_DORCHESTER , ST_NAME_MARLBOROUGH , 
                                      ST_NAME_CENTRE , ST_NAME_PARK , ST_NAME_HAWTHORNE , ST_NAME_MASSACHUSETTS , 
                                      ST_NAME_COLUMBUS , ST_NAME_ADAMS , ST_NAME_HYDE_PARK , ST_NAME_BOYLSTON , 
                                      ST_NAME_COMMERCIAL , ST_NAME_NEWBURY , ST_NAME_SOUTH , ST_NAME_MT_VERNON , 
                                      ST_NAME_CHESTNUT , ST_NAME_RIVER , ST_NAME_E_INDIA , ST_NAME_EIGHTH , 
                                      ST_NAME_WARREN , ST_NAME_BLUE_HILL , ST_NAME_HARRISON , ST_NAME_HUNTINGTON , 
                                      ST_NAME_SHAWMUT , ST_NAME_E_BROADWAY , ST_NAME_WHITTIER , 
                                      ST_NAME_BENNINGTON , U_BDRMS , U_FPLACE , U_HALF_BTH , U_FULL_BTH , 
                                      U_TOT_RMS , U_CORNER_CLEAN , U_ORIENT_CLEAN , U_BASE_FLOOR , 
                                      U_HEAT_TYP_F , U_HEAT_TYP_E , U_HEAT_TYP_P , YR_REMOD2 , 
                                      YR_BUILT3 , LAND_SF_LOG))
                  
                  
##divide data into train set(70%) and test set(30%)
set.seed(123)
divide = sample(1:2, dim(dataclean4)[1], replace = T, prob = c(0.7, 0.3))
trainset = dataclean4[divide == 1,]
testset = dataclean4[divide == 2,]
dim(trainset)
dim(testset)

# Remove old datasets
rm(bostondata,dataclean1,dataclean2,dataclean3)
rm(newdata,fit5,influential_points)
######################### Gradient Boosted Models ################################

# Now preprocess (standardize and center training data)
# standarizing data is required for neural nets and a can't hurt gbms
# note: only need to standardize quantitative variables
# if there are categorical, create mask to grab only the quantitative
#columns
# Note that we do not standardize the response variable
# preprocessing.vals are the means and stds of the training data

# Turn boolean vectors into numerical vectors
factor.indices = c(1,2,3,89)
for(i in c(4:88,90:96)){
  trainset[,i] = as.numeric(trainset[,i])
}
preprocessing.vals <- preProcess(trainset[,2:96], method = c("center", "scale"))
# standardize the training data
train.predictors.standardized <- predict(preprocessing.vals, trainset[,2:96])
# standardize the test data
test.predictors.standardized <- predict(preprocessing.vals, testset[,2:96])
# it is best to pass the predictor variables as a dataframe, and the response variable as a separate df or vector
tune.grid = expand.grid(n.trees=c(50,100,150,200),interaction.depth=c(1,3,5,7),shrinkage=c(.1),n.minobsinnode=c(10))
model.gbm.cv <- train(train.predictors.standardized, trainset[,1], method='gbm', 
                      trControl=trainControl(method='cv'),tuneGrid=tune.grid)
model.gbm.cv
# fit model on whole training set using best parameters
tune.grid = model.gbm.cv$bestTune
model.gbm.final <- train(train.predictors.standardized, trainset[,1], method='gbm', 
                      trControl=trainControl(method='none'),tuneGrid=tune.grid)
# predict on test set and calculate rmse
preds.gbm = predict(model.gbm.final2,newdata = test.predictors.standardized)
RMSE(exp(preds.gbm),exp(testset[,1]))
#check guessing average
RMSE(rep(mean(exp(testset[,1])),length(testset[,1])),exp(testset[,1]))
