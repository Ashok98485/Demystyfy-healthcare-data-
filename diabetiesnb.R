Lab 1( Bank Loan Prediction Model)
----------------------------------------------
  setwd("C:/Users/Dr.Sobhan/Desktop/Dec2016/Data sets/5-Logistic Regression/")                                               
loan = read.csv("bank.csv")
m1=glm(loan$repaid~loan$age+loan$salary,family=binomial)
summary(m1)



Lab 2(Polynomial Modeling :  Heart Stroke Prediction)
--------------------------------------------------------
  health= read.csv("health.csv")
attach(health)
plot(weight,BP,col=Suffered.Heart.storke+1)

BPS = BP*BP
weights = weight*weight
BPweight = BP*weight

m1=glm(Suffered.Heart.storke~BP+weight+weights+BPS+BPweight,family=binomial)
summary(m1)

min(weight)
max(weight)

me = dim(10)
for ( i in  1: 10){
  t = (weight> (i-1)*10) & (weight <= i*10)
  me[i] = mean(Suffered.Heart.storke[t])
}
me

me = log(me/(1-me))
plot(me,type="l")
points(me)

=======================================Lab 3=====================================
  
  Example 1: ICU Data 

SIZE:  200 observations, 21 variables

SOURCE: Hosmer, D.W., Lemeshow, S. and Sturdivant, R.X. (2013) 

LIST OF VARIABLES:
  
  Name	                                  	            Codes/Values	                           Abbreviations
-----------------------------------------------------------------------------------------------------------------------------------------
  Identification Code	                                    ID Number	                                         ID

Vital Status	                                            0 = Lived, 1=Died	                                 STA

Age		                                            Years		                                 AGE

Gender		                                            0 = Male,1=Female	                                 GENDER

Race	                                                    1 = White,2=Black,3=Other                            RACE

Service at ICU Admission                                    0 = Medical,1=Surgical	                         SER

Cancer Part of PresentProblem                               0 = No,1=Yes		                         CAN

History of Chronic Renal Failure                            0 = No,  1=Yes                                       CRN

Infection Probable at ICU Admission	                    0 = No,1=Yes            		                 INF

CPR Prior to ICU Admission	                            0 = No,1=Yes	        	                 CPR	

Systolic Blood Pressure at ICU Admission	            mm Hg		                                 SYS

Heart Rate at ICU Admission	                            Beats/min	                                         HRA	

Previous Admission to an ICU within 6 Months                0 = No,1=Yes		                         PRE	

Type of Admission		                            0 = Elective 1=Emerggency	                         TYP


Long Bone, Multiple, Neck,	                            0 = No,1=Yes		                         FRA
Single Area, or Hip Fracture	

PO2 from Initial Blood Gases	                            0 = > 60, 1<60	                                 PO2


PH from Initial Blood Gases	                           0 => 7.25, 1<7.25          	                         PH


PCO2 from initial Blood Gases		                   0  < 45, 1>=45	                                 PCO


Bicarbonate from Initial Blood Gases	                   0 = >18, 1=<18	                                 BIC


Creatinine from Initial Blood Gases	                   0 = < 2.0, 1>2.0	                                  CRE


Level of Consciousness at ICU Admission                    0 = No Coma or Stupor,1=Deep stupor,2=Coma	          LOC



icu.dat = read.table("ICU.txt",header=T)
summary(icu.dat)

model = glm(sta~age+sex+ser+hra+pre+typ+can+sys, family=binomial,data=icu.dat)
summary(model)

model = glm(sta~age+pre+typ+can, family=binomial,data=icu.dat)
summary(model)

model = glm(sta~age+typ+can, family=binomial,data=icu.dat)
summary(model)

#Deviance test
#lower.tail = FALSE gives probability of greater or equal
pchisq(null.deviance - residual deviance,  df.null - df.residual,  lower.tail = FALSE)

set.seed(1)
n=nrow(icu.dat)
n
n1=floor(n*(0.7))
n1
n2=n-n1
n2

train=sample(1:n,n1)
model = glm(sta~age+typ+can, family=binomial,data=icu.dat[train,])
summary(model)
pchisq(null.deviance - residual deviance,  df.null - df.residual,  lower.tail = FALSE)

ptrain <- predict(model,newdata=icu.dat[train,],type="response")
gg1=floor(ptrain+0.5)
ttt=table(icu.dat$sta[train],gg1)
error=(ttt[1,2]+ttt[2,1])/n1
error

ptest <- predict(model,newdata=icu.dat[-train,],type="response")
gg1=floor(ptest+0.5)
ttt=table(icu.dat$sta[-train],gg1)
error=(ttt[1,2]+ttt[2,1])/n2
error



=====================================Lab 4=================================================
  
  
  tp = vector("numeric",7)
fp = vector("numeric",7)
j=1

for ( i in c(0.3,0.4,0.5,0.6,0.7,0.8,0.9)){
  gg1 = floor(ptest+i)
  ttt=table(icu.dat[-train,"sta"],gg1)
  tp[j] = ttt[2,2]/(ttt[2,1]+ttt[2,2])
  fp[j] = ttt[1,2]/(ttt[1,1]+ttt[1,2])
  j= j+1
}

plot(tp~fp, type="l")


=====================Lab 5(Lasso Logistic regression)===========================
  
  library(glmnet)

Xdel = as.matrix(icu.dat[,c("age","sex","ser","hra","pre","typ","can","sys")])
ydel = icu.dat[,"sta"]

cv.glmmod<-cv.glmnet(x=Xdel,y=ydel,alpha=1, type.measure = "deviance")
plot(cv.glmmod)
f<-cv.glmmod$lambda.min
f

glmmod<-glmnet(x=Xdel,y=ydel, alpha=1, family='binomial')
coef(glmmod,s=f)

predict(glmmod, newx = Xdel, type = "response", s = c(f))


===================Lab 6=======================================================
  
  Several choices are available to estimate multinomial logistic regression models
in R. For example, one can use the command mlogit in the package mlogit, the
command vglm in the package VGAM, or the mnlm function in the package textir.

data = read.csv("brand.csv")
head(data)
data$BRAND = factor(data$BRAND)
data$BRAND
library(nnet)

train = sample(1:nrow(data),600)
test <- multinom(BRAND ~ FEMALE + AGE, data = data[train,])
summary(test)
pp <- fitted(test)
predict(test, newdata = data[-train,], "probs")


z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
