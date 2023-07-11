#import and attach the data
library(readxl)
Project_data <- read_excel("D:/MUN/STAT 6519/term project/Project_data.xlsx")
attach(Project_data)

#data type identification
str(Project_data)

#summary statistics of data for data cleaning purpose
summary(Project_data)

#conversion to indicator variable
Project_data$Air_conditioning<-as.factor(Project_data$Air_conditioning)
Project_data$Pool<-as.factor(Project_data$Pool)
Project_data$Quality<-as.factor(Project_data$Quality)
Project_data$Style<-as.factor(Project_data$Style)
Project_data$Adj_to_highway<-as.factor(Project_data$Adj_to_highway)

#initial model development
modelv1<-lm(Project_data$Price~Project_data$Area+
              Project_data$`#bedroom`+
              Project_data$`#bathroom`+
              Project_data$Air_conditioning+
              Project_data$Garage_capacity+
              Project_data$Pool+
              Project_data$Quality+
              Project_data$Style+
              Project_data$Lot_area+
              Project_data$Adj_to_highway+
              Project_data$AGE)

#check multicollinearity
library(car)
vif(modelv1)

#ANOVA 
anova(modelv1)

#revised model
modelv2<-lm(Project_data$Price~Project_data$Area+
              Project_data$`#bedroom`+
              Project_data$`#bathroom`+
              Project_data$Air_conditioning+
              Project_data$Garage_capacity+
              Project_data$Quality+
              Project_data$Style+
              Project_data$Lot_area+
              Project_data$AGE)

#ANOVA and summary
anova(modelv2)
summary(modelv2)

#model adequacy check
prd_modelv2<-modelv2$fitted.values
resid_modelv2<-rstudent(modelv2)
library(car)
qqPlot(resid_modelv2,xlab = 'Norm Quantiles',
       ylab = 'Externally Studentized Residual',
       grid = FALSE)

plot(prd_modelv2,resid_modelv2,xlab = 'Predicted Values',
     ylab = 'Externally Studentized Residual')

#transformation
ystar<-log(Project_data$Price)
modelv3<-lm(ystar~Project_data$Area+
              Project_data$`#bedroom`+
              Project_data$`#bathroom`+
              Project_data$Air_conditioning+
              Project_data$Garage_capacity+
              Project_data$Quality+
              Project_data$Style+
              Project_data$Lot_area+
              Project_data$AGE)
anova(modelv3)
summary(modelv3)

#after tranformation again check model adequacy
prd_modelv3<-modelv3$fitted.values
resid_modelv3<-rstudent(modelv3)
library(car)
qqPlot(resid_modelv3,xlab = 'Norm Quantiles',
       ylab = 'Externally Studentized Residual',
       grid = FALSE)

plot(prd_modelv3,resid_modelv3,xlab = 'Predicted Values',
     ylab = 'Externally Studentized Residual')

#removing insignificant variable and check again
modelv4<-lm(ystar~Project_data$Area+
              Project_data$`#bathroom`+
              Project_data$Air_conditioning+
              Project_data$Garage_capacity+
              Project_data$Quality+
              Project_data$Style+
              Project_data$Lot_area+
              Project_data$AGE)
anova(modelv4)
summary(modelv4)

prd_modelv4<-modelv4$fitted.values
resid_modelv4<-rstudent(modelv4)
library(car)
qqPlot(resid_modelv4,xlab = 'Norm Quantiles',
       ylab = 'Externally Studentized Residual',
       grid = FALSE)

plot(prd_modelv4,resid_modelv4,xlab = 'Predicted Values',
     ylab = 'Externally Studentized Residual')

summary(influence.measures(modelv4))

#comparison between model 3 and model 4
anova(modelv3,modelv4)

#findout influential point 
covratio_general<-covratio(modelv3)
covratio_offlimit<-covratio_general>1.06 | covratio_general<0.94 
covratio_offlimit

#removeal influential point
newproject_data<-Project_data[!covratio_offlimit,]
plot(Project_data$ID,covratio_general)+
  abline(h=1.06,col='red')+
  abline(h=.94,col='red')

#final model after removal influential point
newproject_data<-newproject_data[-c(39,44,265),]
ystar_2<-log(newproject_data$Price)
