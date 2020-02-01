library(VIM)
library(mice)
library(corrgram)
library(car)
library(tidyverse)

## Read CSV file
lifedata = read.csv("C:/Users/BelajiAvvaru/Desktop/Docs/SMU/MSDS 6372/Project 1/Life Expectancy Data.csv", header=T, sep = ',')
summary(lifedata)
str(lifedata)
lifeexpdata = subset(lifedata,Year==2015)
lifedata
# Verify any missing values in any variable
lifedata[!complete.cases(lifedata), ]

#Display missing-data patterns
md.pattern(lifedata, plot=TRUE, rotate.names = TRUE)

#Display missing-data in a bar-plot
mice_plot <- aggr(lifedata, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(lifedata), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#Data frame with all continuous variables 
lifedata1 = lifedata %>% select(c(Life.expectancy, Adult.Mortality, Alcohol, Hepatitis.B, BMI, Polio, 
                                        Total.expenditure, Diphtheria, GDP,Population, Income.composition.of.resources, 
                                        thinness..1.19.years , thinness.5.9.years, Schooling, HIV.AIDS, under.five.deaths,
                                        Measles, percentage.expenditure, infant.deaths)) 

#correlation plot for all continuous variables
corrgram(lifedata1, order=TRUE,
         upper.panel=panel.cor, lower.panel=panel.pie, main="Life expectancy correlogram")

#Impute missing data with "CART"(Classification and Regression Trees) method
imputed_Data2 <- mice(lifedata1, m=5, maxit =5, method = 'cart', seed = 500)

#Data frame after imputation
completeData1 <- mice::complete(imputed_Data2,)
str(completeData1)

#Display missing-data in a bar-plot after imputation
mice_plot <- aggr(completeData1, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(completeData1), cex.axis=.7,
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
life.model1<-lm(Life.expectancy~Adult.Mortality+Alcohol+Hepatitis.B+BMI+Polio+Total.expenditure+Diphtheria+GDP+Population
           +Income.composition.of.resources+thinness..1.19.years+thinness.5.9.years+Schooling+HIV.AIDS
           +under.five.deaths+Measles+percentage.expenditure+infant.deaths, data=completeData1)

summary(life.model1)

#variables Hepatitis.B, Population, thinness..1.19.years, thinness.5.9.years, Measles and percentage.expenditure  are not statistically significant
#use stepwise selection to identify statistically significant variables

life.model2<-step(life.model1, data=completeData1, direction = "both", test="F")
summary(model2)

#Verify multicollinearity
vif(life.model2)

## under.five.deaths and infant.deaths variables are highly collinear with VIF of 170.406 and 169.211. Remove these two variables from the model
life.model3<-lm(Life.expectancy~Adult.Mortality+Alcohol+Hepatitis.B+BMI+Polio+Total.expenditure+Diphtheria+GDP+Population
           +Income.composition.of.resources+thinness..1.19.years+thinness.5.9.years+Schooling+HIV.AIDS
           +Measles+percentage.expenditure, data=completeData1)

summary(life.model3)

#variables Hepatitis.B, Population, thinness..1.19.years, thinness.5.9.years, Measles  are not statistically significant
#use stepwise selection to identify statistically significant variables

life.model4<-step(life.model3, data=completeData1, direction = "both", test="F")
summary(life.model4)

#Verify multicollinearity
vif(life.model4)

#Alcohol and Measles are not significant

## under.five.deaths and infant.deaths variables are highly collinear with VIF of 170.406 and 169.211. Remove these two variables from the model
life.model5<-lm(Life.expectancy~Adult.Mortality+Hepatitis.B+BMI+Polio+Total.expenditure+Diphtheria+GDP+Population
           +Income.composition.of.resources+thinness..1.19.years+thinness.5.9.years+Schooling+HIV.AIDS
           +percentage.expenditure, data=completeData1)

summary(life.model5)

#variables Hepatitis.B, Population, thinness..1.19.years, thinness.5.9.years, Measles  are not statistically significant
#use stepwise selection to identify statistically significant variables

life.model6<-step(life.model5, data=completeData1, direction = "both", test="F")
summary(life.model6)

#Verify multicollinearity
vif(life.model6)
# Model4 has all singificant variables without any collinearity
# Plot residual diagrams for Model4

plot(life.model6)


