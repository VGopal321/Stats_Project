install.packages("olsrr", repos = "https://cran.microsoft.com/")

library(tidyverse)
house=read.csv(file.choose(),header=T)

head(house)
dim(house)

#Plotting log-log transform of grlivarea and saleprice
house%>%ggplot(aes(x=log(GrLivArea),y=log(SalePrice)))+
  geom_point()+
  geom_smooth(method='lm')

#Creating feature - interiorSF
house$interiorSF=house$GrLivArea+house$TotalBsmtSF


#Creating feature - YrDiff
house$YrDiff=house$YrSold-house$YearBuilt

#Plotting log-log transform of interiorSF and SalePrice
ggplot(data=house,aes(x=log(interiorSF),y=(log(SalePrice))))+
  geom_point(col=house$MSSubClass)
logb(10,base=10)
#Creating Factors
for(i in 1:dim(house)[2]){
  if (class(house[,i])!='integer'){
    house[,i]=as.factor(house[,i])
  }
}

#Additional Factors
house$MSSubClass=as.factor(house$MSSubClass)
house$OverallQual=as.factor(house$OverallQual)
house$OverallCond=as.factor(house$OverallCond)
house$GarageCars=as.factor(house$GarageCars)

#Making the model
housefit=lm(log(SalePrice)~log(interiorSF)+YrDiff+Neighborhood+OverallCond+OverallQual+MSZoning,data=house)
summary(housefit)

#Cross Validation
seednum=sample(1:100000,1)
seednum
set.seed(seednum)
set.seed(35)
trainInd=sample(1:dim(house)[1],round(.8*dim(house)[1]))
house80=house[trainInd,]
house20=house[-trainInd,]
h80fit=lm(log(SalePrice)~log(interiorSF)+YrDiff+Neighborhood+OverallCond+OverallQual+MSZoning,data=house80)
h20pred=predict(h80fit,newdata=house20)

#Finding R2 of fit
h20resid=h20pred-log(house20$SalePrice)
length(h20resid)
h20SSR=sum(h20resid^2)
h20SSR
meanresid20=log(house20$SalePrice)-mean(log(house20$SalePrice))
meanresid20SSR=sum(meanresid20^2)
meanresid20SSR
R2=(meanresid20SSR-h20SSR)/meanresid20SSR
n=1460*.2
p=dim(summary(h80fit)$coefficients)[1]-1
R2
AdjR2=1-(1-R2)*(n-1)/(n-p-1)
AdjR2

#Loading Test Set
htest=read.csv(file.choose(),header=T)
htest$SalePrice=NA
htest$interiorSF=htest$GrLivArea+htest$TotalBsmtSF
htest$YrDiff=htest$YrSold-htest$YearBuilt
#Creating Factors
for(i in 1:dim(htest)[2]){
  if (class(htest[,i])!='integer'){
    htest[,i]=as.factor(htest[,i])
  }
}

#Additional Factors
htest$MSSubClass=as.factor(htest$MSSubClass)
htest$OverallQual=as.factor(htest$OverallQual)
htest$OverallCond=as.factor(htest$OverallCond)
htest$GarageCars=as.factor(htest$GarageCars)
head(htest$MSSubClass)

#Assessing test set
housepred=predict(housefit,newdata = htest)
htest$SalePrice=exp(housepred)

submission=htest%>%select(Id,SalePrice)
write.csv(submission,'submission.csv')
