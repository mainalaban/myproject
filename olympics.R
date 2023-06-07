#question one: olympic data
library(readr)
olympics <- read_csv("C:/Users/Laban/Downloads/olympics.csv")
head(olympics)
model1.0<- lm(olympics$`2011 GDP` ~ olympics$`Gold medals`)
summary(model1.0)
model1.1<- lm(olympics$`Gold medals` ~ olympics$`2011 GDP`)
summary(model1.1)
olympics_new<- olympics[,3:9]
olympics_new
cor(olympics_new)
model1.2 <- lm(olympics$`2011 GDP` ~ olympics$`2010 population` + olympics$`Female count`+ olympics$`Male count` + olympics$`Gold medals` + olympics$`Silver medals` + olympics$`Bronze medals`)
summary(model1.2)
model1.3<- lm(olympics_new$`Gold medals`~ olympics_new$`2011 GDP`+ olympics_new$`2010 population` + olympics_new$`Female count`+ olympics_new$`Male count`)
summary(model1.3)
model1.4<- lm(olympics_new$`Silver medals` ~ olympics_new$`2011 GDP`+ olympics_new$`2010 population` + olympics_new$`Female count`+ olympics_new$`Male count`)
summary(model1.4)

#question two:viewing head of the housing data 
library(readr)
housing <- read_csv("C:/Users/Laban/Downloads/housing.csv")
head(housing)
#linear model for the data 
model1<- lm(MEDV ~ CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS +RAD + TAX + PTRATIO + LSTAT, data=housing)
model1
summary(model1)

#creating scatter plot matrix 
pairs(~ MEDV +CRIM + ZN + INDUS + CHAS, data=housing)
pairs(~MEDV + NOX + RM + AGE + DIS, data= housing)
pairs(~MEDV +RAD + TAX + PTRATIO + LSTAT, data=housing)
cor(housing)
#reducing number of variables 
model2<- lm(MEDV~ CRIM + ZN + CHAS+ NOX + RM + DIS +RAD + TAX + PTRATIO + LSTAT, data= housing)
summary(model2)
#transforming the variables 
house.new<- log(housing)
View(house.new)
model3<- lm(MEDV ~ CRIM + ZN + CHAS+ NOX + RM + DIS +RAD + TAX + PTRATIO + LSTAT, data= housing)
summary(model3)

#stepwise procedure: starting with forward
library(olsrr)
ols_step_forward_p (model1, penter= 0.05)

ols_step_backward_p(model1, penter=0.05)

#perfoming the regsubsets
library(leaps)
library(ISLR)
regfit.full<- regsubsets(MEDV~., housing, nvmax = 12 )
reg.summmary<- summary(regfit.full)
names(reg.summmary)
plot(reg.summmary$rsq, xlab="no of variables", ylab = "Rsquared", type = "l")
plot(reg.summmary$rss, xlab = "no of variables", ylab = "RSS", type = "l")
#adjusted R-squared 
which.max(reg.summmary$adjr2)
which.min(reg.summmary$cp)

#regressing 8 variables 
model4<- lm(MEDV~ CRIM + ZN + CHAS+ NOX + RM + DIS +RAD + TAX + PTRATIO, data= housing)
summary(model4)

# question 3
# (a) v.w  (dot product)
library(pracma)
v<-matrix(c(-1,1,3),nrow = 3)
v
w<-matrix(c(2,-1,1),nrow = 3)
w
dot(v,w)
#(b) -3*w
w<-matrix(c(2,-1,1),nrow = 3)
w
-3*w
# (c) m*v
M<-matrix(c(20,5,0,5,25,-10,0,10,5),nrow = 3,byrow = T)
M
V<-matrix(c(-1,1,3),nrow = 3)
V
MV<-M%*%V
MV
#(d) M+N
M<-matrix(c(20,5,0,5,25,-10,0,10,5),nrow = 3,byrow = T)
M
N<-matrix(c(-20,0,10,5,10,15,5,20,-5),nrow = 3,byrow = T)
N
M+N
# (e)
M-N
#(f) Z Transpose
Z<-matrix(c(1,1,1,1,4,3,2,-5),nrow = 4,byrow = F)
Z
t(Z)
#(g) t(Z)*Z
t(Z)%*%Z



