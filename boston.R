bostondata = read.csv("/Users/nick/Desktop/boston/Property_Assessment_2014.csv",header = T)
library(ggplot2)
library(nortest)
library(car)
library(boot)
library(rpart)
library(rpart.plot)
library(e1071)
library(parallel)
library(randomForest)
library(neuralnet)

##############  cleanse data ###############################

## if one variable has as many NA values as more than 50%, then drop it
## drop the irrelevent and meaningless variables such as ID, address,ect..
## AV_TOTAL is the target variable and 7 input variables is left.
## AV_TOTAL>50000000 is regarded as outliers

attach(bostondata)
mydata=data.frame(AV_TOTAL,LU,OWN_OCC,GROSS_AREA,NUM_FLOORS,LAND_SF,YR_BUILT,LIVING_AREA)
detach(bostondata)

fit=lm(AV_TOTAL~GROSS_AREA+NUM_FLOORS+LAND_SF+YR_BUILT+LIVING_AREA,data=mydata)
summary(fit)
vif(fit)
##GROSS_AREA and LIVING_AREA are highly correlated, so drop GROSS_AREA or LIVING_AREA
summary(mydata)


mydata$AV_TOTAL[mydata$AV_TOTAL==0|mydata$AV_TOTAL> 50000000] = NA
mydata$LU = factor(mydata$LU,levels=c("R1","R2","R3","R4","RL","A","RC","CM","CD","CP","CC","AH",
                                      "C","CL","I","E","EA"))
mydata$OWN_OCC = factor(mydata$OWN_OCC, levels = c("Y","N"))
mydata$GROSS_AREA[mydata$GROSS_AREA == 0] = NA
mydata$NUM_FLOORS[mydata$NUM_FLOORS > 100 | mydata$NUM_FLOORS  < 1] = NA
mydata$LAND_SF[mydata$LAND_SF == 0| mydata$LAND_SF > 10000000] = NA
mydata$YR_BUILT[mydata$YR_BUILT < 1000 | mydata$YR_BUILT == 0 | mydata$YR_BUILT>2014] =NA

mydata_clean = na.omit(mydata)
mydata_clean$LU=factor(mydata_clean$LU)

###################### divide data #######################
##divide data into train set(70%) and test set(30%)

set.seed(123)
divide = sample(1:2, dim(mydata_clean)[1], replace = T, prob = c(0.7, 0.3))
trainset = mydata_clean[divide == 1,]
testset = mydata_clean[divide == 2,]

##################### cost function ######################

RMSE = function(a,b){
  sqrt(mean((a-b)^2))
}

##########################################################
####################### modeling #########################
##########################################################

summary(trainset)

## histogram of the target
ggplot(trainset, aes(x=AV_TOTAL,y=..density..)) + geom_histogram(binwidth=10000) + xlim(0,2000000)

####################  linear model  ######################
lm = lm(AV_TOTAL~.,data=trainset)
summary(lm)

## residuals diagnose
ggdatalm = data.frame(residuals=lm$residuals,fitted=lm$fitted.values)
ggplot(ggdatalm,aes(x=fitted,y=residuals))+geom_point(alpha=0.1,size=4)  
# heteroskedasticity

ggplot(ggdatalm,aes(x=fitted,y=residuals))+geom_point(alpha=0.1,size=4)+
  xlim(-1000000,10000000)+ylim(-10000000,10000000)
ad.test(lm$residuals)

leveragePlots(lm)
ncvTest(lm)
vif(lm)

prelm = predict(lm, testset[,2:7])
qplot(prelm,testset[,1])
RMSE(prelm,testset[,1])

################ generalized linear model ################
## Target variable do not has a normal but a fat-tailed distribution,
## Gamma regression is more suitable.

glm = glm(AV_TOTAL~.,data=trainset,family=Gamma(link="log"),maxit = 100)
summary(glm)

## residuals plot
ggdataglm = data.frame(residuals=glm$residuals,fitted=log(lm$fitted.values))
ggplot(ggdataglm,aes(x=fitted,y=residuals))+geom_point(alpha=0.1,size=4)  

## cross validation
cvglm = cv.glm(trainset,glm,cost=RMSE,k=10)
cvglm$delta

preglm = predict(glm,testset[,2:7])
RMSE(preglm,testset[,1])

#######################  decision tree  ###########################
rpart = rpart(AV_TOTAL~.,data=trainset)
rpart.plot(rpart)
printcp(rpart)

prerpart = predict(rpart,testset[,2:7])
RMSE(prerpart,testset[,1])

#######################   SVM   ##################################
svm = svm(AV_TOTAL~.,data=trainset, type="nu-regression")

# tunesvm=tune.svm(AV_TOTAL~.,data=trainset, cost = 2^(0:4))   too slow
# parallel computing

parasvm = function(cost){
  library(e1071)
  svm = svm(AV_TOTAL~.,data=trainset, type="nu-regression", cost=cost)
  presvm = predict(svm,testset[,2:7])
  RMSE(presvm,testset[,1])
}

cl = makeCluster(4)      #set CPU cores cluster
clusterExport(cl,c("trainset","testset","RMSE"))
results = clusterApply(cl,2^(0:3), parasvm)
results

######################  random forest  ###########################
rf = randomForest(AV_TOTAL~.,data=trainset)
prerf = predict(rf,testset[,2:7])
RMSE(prerf,testset[,1])

#####################  neural network  ###########################
n = names(trainset)
f = as.formula(paste("AV_TOTAL ~", paste(n[!n %in% "AV_TOTAL"], collapse = " + ")))

nnet = neuralnet(f,data = trainset, hidden = c(10,5), linear.output = T)
prenn = compute(nnet,testset[,2:7])$net.result
RMS(prenn,testset[,1])


