#3.14

library(GLMsData)
data ("manuka")
head(manuka)

#1

install.packages("dplyr")
library(dplyr)
over_2 <- subset(manuka, Duration > 2)
reduced_size <- over_2$Size0 - over_2$Size2
red_size_percentage <- ((over_2$Size0 - over_2$Size2) / over_2$Size0) * 100

over_2
red_size_percentage
plot(red_size_percentage ~ pH0, data = over_2, xlab ="Initial pH",
         ylab = "Percentage Reduction in Wound Size over 2 weeks", xlim=c(7.2,8.2), 
     ylim = c(-100,100))

#2

model_over <- lm(red_size_percentage ~ pH0, data = over_2)
abline(model_over, col="blue")

#3

summary(model_over)

#4

resd.raw=resid(model_over)
resd.std=rstandard(model_over)
c( Raw=var(resd.raw), Standardized=var(resd.std) )

scatter.smooth( rstandard(model_over ) ~ over_2$pH0, col="red",
                las=1, ylab="Standardized residuals", xlab="Initial pH",
                xlim = c(7,9),ylim = c(-4,4))


scatter.smooth( rstandard( model_over ) ~ fitted( model_over ), col="green",
                las=1, ylab="Standardized residuals", xlab="Fitted values",
                xlim = c(160,260), ylim = c(-4,4))

qqnorm( rstandard( model_over ), las=1, pch=19,
xlab = "lm(red_size_percentage ~ pH0,data = over_2)", 
ylab = "Standardied residuals", xlim=c(-2,2),ylim=c(-4,4))

qqline( rstandard( model_over ) )
shapiro.test(resid(model_over))

model_over=influence.measures(model_over)
model_over

# 5

delete <- c(14, 15, 18)
over_3 <- over_2[-delete, ]
over_3

reduced_size1 <- over_3$Size0 - over_3$Size2
red_size_percentage1 <- ((over_3$Size0 - over_3$Size2) / over_3$Size0) * 100


model_over1 <- lm(red_size_percentage1 ~ pH0, data = over_3)
summary(model_over1)

#6
plot(red_size_percentage1 ~ pH0, data = over_3, xlab ="Initial pH",
     ylab = "Percentage Reduction in Wound Size over 2 weeks", xlim=c(7.2,8.2), 
     ylim = c(-100,100))
abline(model_over1, col='red')


#3.18

library(GLMsData)
data("humanfat")
head(humanfat)

#1

female_data <- subset(humanfat, Gender == "F")
head(female_data)
male_data <- subset(humanfat, Gender == "M")
head(male_data)

library(ggplot2)
plot(female_data$BMI, female_data$Percent.Fat, main = "Scatter Plot: Percent.Fat vs BMI", 
     xlab = "Female.BMI", ylab = "Percent.Fat")

plot(male_data$BMI, male_data$Percent.Fat, main = "Scatter Plot: Percent.Fat vs BMI", 
     xlab = "Male.BMI", ylab = "Percent.Fat")

plot(female_data$Age, female_data$Percent.Fat, main = "Scatter Plot: Percent.Fat vs Age", 
     xlab = "Female.Age", ylab = "Percent.Fat")

plot(male_data$Age, male_data$Percent.Fat, main = "Scatter Plot: Percent.Fat vs Age", 
     xlab = "Male.Age", ylab = "Percent.Fat")

#2

lm.model <- lm(Percent.Fat ~ Age*Gender, data=humanfat)
summary(lm.model)


#6
anova(lm.model)

#8

plot(male_data$Age, male_data$Percent.Fat, main = "Scatter Plot: Percent.Fat vs Age", 
     xlab = "Male.Age", ylab = "Percent.Fat")
lm.model1 <- lm(Percent.Fat ~ Age, data= male_data)
abline (lm.model1)

plot(male_data$BMI, male_data$Percent.Fat, main = "Scatter Plot: Percent.Fat vs BMI", 
     xlab = "Male.BMI", ylab = "Percent.Fat")
lm.model2 <- lm(Percent.Fat ~ BMI, data = male_data)
abline(lm.model2)

plot(female_data$BMI, female_data$Percent.Fat, main = "Scatter Plot: Percent.Fat vs BMI", 
     xlab = "Female.BMI", ylab = "Percent.Fat")
lm.model3 <- lm(Percent.Fat ~ Age, data= female_data)
abline (lm.model3)

plot(female_data$Age, female_data$Percent.Fat, main = "Scatter Plot: Percent.Fat vs Age", 
     xlab = "female.Age", ylab = "Percent.Fat")
lm.model4 <- lm(Percent.Fat ~ Age, data = female_data)
abline(lm.model4)

#9

prediction <- predict(lm.model1, interval = "confidence", level = 0.90)
male_data <- cbind(male_data, prediction)
male_data

fit_Model <- data.frame(Age=seq(min(male_data$Age), max(male_data$Age, lenght.out = 100)))
pred_values <- predict(lm.model1, fit_Model, interval = "confidence", level = 0.90)
pred_values <- predict(lm.model1, fit_Model, interval = "confidence", level = 0.90)
fit_values <- data.frame(Age=fit_Model$Age, fitted.values=pred_values[, "fit"], LowerCI = pred_values[, "lwr"], UpperCI = pred_values[, "upr"])
plot(fit_values$Age, fit_values$fitted.values, type = "l",col = "red", lwd = 2, 
     xlab = "Male.Age", ylab = "Fitted values", 
     main= " Fitted Values Male with 90% Confidence Interval")




prediction1 <- predict(lm.model4, interval = "confidence", level = 0.90)
female_data <- cbind(female_data, prediction1)
female_data

fit_Model <- data.frame(Age=seq(min(female_data$Age), max(female_data$Age, lenght.out = 100)))
pred_values <- predict(lm.model4, fit_Model, interval = "confidence", level = 0.90)
pred_values <- predict(lm.model4, fit_Model, interval = "confidence", level = 0.90)
fit_values <- data.frame(Age=fit_Model$Age, fitted.values=pred_values[, "fit"], LowerCI = pred_values[, "lwr"], UpperCI = pred_values[, "upr"])
plot(fit_values$Age, fit_values$fitted.values, type = "l",col = "red", lwd = 2, 
     xlab = "Female.Age", ylab = "Fitted values", xlim = c(20,100), ylim = c(20,70),
     main= " Fitted Values Female with 90% Confidence Interval")

#10

?humanfat

# 11

female_data1 <- female_data[-1,]
lm.data <- lm(Percent.Fat ~ Age + BMI, data= female_data1)
summary(lm.data)

#12

cook = cooks.distance(lm.data)
cook

lev = hatvalues(lm.data)
lev

resd.stud = rstudent (lm.data)
resd.stud

resd.stan = rstandard(lm.data)
resd.stan

inf.mea = influence.measures(lm.data)
inf.mea

boxplot(female_data1)

