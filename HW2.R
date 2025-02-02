#2.13

print(anova_table)
head(anova_table)
anova_table <- data.frame(
  Source_of_Variation = c("Cue", "Sex", "Age", "Residual"),
  df = c(3, 1, 3, 60),
  ss = c(117793, 2659, 22850, 177639)
)

print(anova_table)
View(anova_table)


# 2.16

library(GLMsData)
data("Crawl")
head(crawl)
View(crawl)

#1

plot(Age ~ Temp, data=crawl, cex=0.05*SampleSize, pch=19, xlab = "Monthly Average Temp(°F)", ylab = "Mean Age Of crawling Started(weeks)")

#3

weighted_model <- lm(Age ~ Temp, data = crawl, weights = SampleSize)
summary(weighted_model)

#4
round(coef( summary( weighted_model ) ), 5)
 
# 5
confint(weighted_model, level=0.90)
  
# 6
unwei_model <- lm(Age~Temp, data = crawl)
plot(Age ~ Temp, data=crawl, cex=0.05*SampleSize, pch=19, xlab = "Monthly Average Temp(°F)", ylab = "Mean Age Of crawling Started(weeks)")
abline(weighted_model, col="Blue")
abline(unwei_model, col="Red")
  
# 7
prediction <- predict(weighted_model, interval = "confidence", level = 0.95)
crawl <- cbind(crawl, prediction)
crawl

fit_Model <- data.frame(Temp=seq(min(crawl$Temp), max(crawl$Temp, lenght.out = 100)))
pred_values <- predict(Weighted_Model, fit_Model, interval = "confidence", level = 0.95)
pred_values <- predict(weighted_model, fit_Model, interval = "confidence", level = 0.95)
fit_values <- data.frame(Temp=fit_Model$Temp, fitted.values=pred_values[, "fit"], LowerCI = pred_values[, "lwr"], UpperCI = pred_values[, "upr"])
plot(fit_values$Temp, fit_values$fitted.values, type = "l",col = "red", lwd = 2, xlab = "Average Monthly Temperature", ylab = "Fitted values", main= "Weighted Regression Fitted Values with 95% Confidence Interval")
       

#2.19

library(GLMsData)
data("sharpener")

initial_model <- lm(log(Y)~1, data = sharpener)
full_model <- lm(log(Y)~X1+X2+X3+X4+X5+X6+X7+X8+X9+x10, data = sharpener)

#1
reg_forw1 <- step(initial_model, direction = "forward",
                         scope = list(lower=initial_model, upper = full_model))
#2
reg_back1 <- step(full_model, direction = "backward",
                         scope = list(lower=initial_model, upper = full_model))
       
#3
reg_both1 <- step(initial_model, direction = "both",
                         scope = list(lower=initial_model, upper = full_model))

#4
summary(reg_forw1)
summary(reg_back1)
summary(reg_both1)

#5

library(MASS)
reg_for_bic <- step(reg_forw1, direction = "forward", k=log(nrow(sharpener)))
reg_bac_bic <- step(reg_back1, direction = "backward", k=log(nrow(sharpener)))
reg_both_bic <- step(reg_both1, direction = "both", k=log(nrow(sharpener)))

#6
?sharpener
confint(reg_forw1)
       