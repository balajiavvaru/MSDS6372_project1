library(VIM)
library(mice)
library(corrgram)
library(car)
library(tidyverse)

## Read CSV file
lifeexpdata = read.csv("C:/Users/BelajiAvvaru/Desktop/Docs/SMU/MSDS 6372/Project 1/Life Expectancy Data.csv", header=T, sep = ',')
summary(lifeexpdata)
str(lifeexpdata)

# Verify any missing values in any variable
lifeexpdata[!complete.cases(lifeexpdata), ]

#Display missing-data patterns
md.pattern(lifeexpdata, plot=TRUE, rotate.names = TRUE)

#Display missing-data in a bar-plot
mice_plot <- aggr(lifeexpdata, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(lifeexpdata), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#Data frame with all continuous variables 
lifeexpdata1 = lifeexpdata %>% select(c(Life.expectancy, Adult.Mortality, Alcohol, Hepatitis.B, BMI, Polio, 
                                        Total.expenditure, Diphtheria, GDP,Population, Income.composition.of.resources, 
                                        thinness..1.19.years , thinness.5.9.years, Schooling, HIV.AIDS, under.five.deaths,
                                        Measles, percentage.expenditure, infant.deaths)) 

#correlation plot for all continuous variables
corrgram(lifeexpdata1, order=TRUE,
         upper.panel=panel.cor, lower.panel=panel.pie, main="Life expectancy correlogram")

#Impute missing data with "CART"(Classification and Regression Trees) method
imputed_Data1 <- mice(lifeexpdata1, m=5, maxit =5, method = 'cart', seed = 500)

#Data frame after imputation
completeData <- mice::complete(imputed_Data1,)
str(completeData)

#Display missing-data in a bar-plot after imputation
mice_plot <- aggr(completeData, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(lifeexpdata), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Continuous Variables
#Life.expectancy, 
#Adult.Mortality, 
#Alcohol, 
#Hepatitis.B, 
#BMI, 
#Polio, 
#Total.expenditure, 
#Diphtheria, 
#GDP,
#Population, 
#Income.composition.of.resources, 
#thinness..1.19.years , 
#thinness.5.9.years, 
#Schooling
#HIV.AIDS
#under.five.deaths
#Measles
#percentage.expenditure 
#infant.deaths


#create a regression model
model1<-lm(Life.expectancy~Adult.Mortality+Alcohol+Hepatitis.B+BMI+Polio+Total.expenditure+Diphtheria+GDP+Population
           +Income.composition.of.resources+thinness..1.19.years+thinness.5.9.years+Schooling+HIV.AIDS
           +under.five.deaths+Measles+percentage.expenditure+infant.deaths, data=completeData)

summary(model1)

#variables Hepatitis.B, Population, thinness..1.19.years, thinness.5.9.years, Measles and percentage.expenditure  are not statistically significant
#use stepwise selection to identify statistically significant variables

model2<-step(model1, data=completeData, direction = "both", test="F")
summary(model2)

#Verify multicollinearity
vif(model2)

## under.five.deaths and infant.deaths variables are highly collinear with VIF of 170.406 and 169.211. Remove these two variables from the model
model3<-lm(Life.expectancy~Adult.Mortality+Alcohol+Hepatitis.B+BMI+Polio+Total.expenditure+Diphtheria+GDP+Population
           +Income.composition.of.resources+thinness..1.19.years+thinness.5.9.years+Schooling+HIV.AIDS
           +Measles+percentage.expenditure, data=completeData)

summary(model3)

#variables Hepatitis.B, Population, thinness..1.19.years, thinness.5.9.years, Measles  are not statistically significant
#use stepwise selection to identify statistically significant variables

model4<-step(model3, data=completeData, direction = "both", test="F")
summary(model4)

#Verify multicollinearity
vif(model4)

#Alcohol and Measles are not significant

## under.five.deaths and infant.deaths variables are highly collinear with VIF of 170.406 and 169.211. Remove these two variables from the model
model5<-lm(Life.expectancy~Adult.Mortality+Hepatitis.B+BMI+Polio+Total.expenditure+Diphtheria+GDP+Population
           +Income.composition.of.resources+thinness..1.19.years+thinness.5.9.years+Schooling+HIV.AIDS
           +percentage.expenditure, data=completeData)

summary(model5)

#variables Hepatitis.B, Population, thinness..1.19.years, thinness.5.9.years, Measles  are not statistically significant
#use stepwise selection to identify statistically significant variables

model6<-step(model5, data=completeData, direction = "both", test="F")
summary(model6)

#Verify multicollinearity
vif(model6)
# Model4 has all singificant variables without any collinearity
# Plot residual diagrams for Model4

plot(model6)


