install.packages("nlme")
####Packages ####
library(GGally)
library(caret)
library(lmtest)
library(car)
library(MASS)
library(fit.models)
library(nlme)
library(fBasics)

#####Data Importation #####
data1=read.csv(file.choose(),header=T)
attach(data1)
summary(data1[,4:6])
summary(data1[,6:9])
summary(data1[,3])

####Descriptive statistics#####
results1=basicStats( data1[,c(4,5,6,7,8,9)] )[c("Minimum", "Maximum","Mean","Skewness","Kurtosis"),]
results2 =data.frame(results1)
results2

####Transforming Data ####
y<-log(data.matrix(data1[,3]))
data_sample<-log(data.frame(data.matrix(data1[,4:9])))
head(round(data_sample,3))
head(round(y,3))
cor_matrix=cor(data.matrix(data_sample))
rid_col=findCorrelation(cor_matrix,cutoff=0.6,exact=FALSE)
rid_col
colnames(data_sample)[rid_col]

####Dropping the Correlated Columns ####
data_sample$B1=NULL
data_sample$B=NULL
head(round(data_sample,3))

#### Train and Test set ####
set.seed(2016)
N=nrow(data_sample)
train=sample(1:N,550,FALSE)
y_train=y[train]
y_test=y[-train]
data_train=data_sample[train,]
data_test=data_sample[-train,]
nrow(data_train)
nrow(data_test)

#### Train Models using Training Set #####
fit=lm(y_train~.,data=data_train)
fit
round(fit$coefficients,3)

#### The coefficient of T1 is negative raises suspicion####
#### Fit a simple regression between L and T1 to confirm our suspicion ####
fitT1=lm(y_train~T1,data=data_train)
fitT1

#### Although there are alternative solutions in literature we simply drop T1 ####
fit=lm(y_train~L1+B2+T,data=data_train)
fit
round(fit$coefficient,3)

#### Viewing Confidence Intervals ####
CI=round(confint(fit,level=0.95),3)
CI

#### Evaluate Model performance #####
summary(fit)

##### Check for Non-linear patterns in the residuals ######
plot(fit,which=1)

#### Check the distribution of the residuals ####
windows(width=20,height=10)
par(mfrow=c(2,1))
plot(density(fit$residuals),main="Residuals",xlab="Value")
plot(fit,which=2)

#### Check for normality ####
shapiro.test(fit$residuals)

#### Check equal variance ####
windows(width=20,height=10)
plot(fit,which=3)

####Breuch Pagan test ####
bptest(fit)

#### check for Residuals for Autocorrelation ####
windows(width=20,height=10)
acf(fit$residuals)
dwtest(fit)

#### Assessing train set performance ####
round(cor(fit$fitted,y_train),4)

#### Assessing Test Set performance ####
pred=predict(fit,data_test)
round(head(pred,3),3)
r2=round(cor(y_test,pred)^2,3)
r2
round(sqrt(r2),4)
