library(GLMsData)
library(statmod)
data(blocks)
head(blocks)

lm1 <- glm(Number~Age, data=blocks, family=poisson)
summary(lm1)

Pearson <- residuals(lm1, type = "pearson")
Pearson

deviance_resid <- residuals(lm1, type = "deviance")
deviance_resid

par(mfrow=c(2, 2))
qqnorm(qresid(lm1), ylim = c(-1.5, 1.5), 
       xlim = c(-2,2))
qqline(qresid(lm1))

fitted_values <- lm1$fitted.values
quantile_resid <- qresid(lm1)
quantile_resid

# Residuals vs Fitted Values
plot(fitted(lm1), deviance_resid, main = "Deviance Residuals vs Fitted")
abline(h = 0, col = "red")

plot(fitted(lm1), Pearson, main = "Pearson Residuals vs Fitted")
abline(h = 0, col = "green")


plot(fitted_values, quantile_resid,
     xlab = "Fitted Values",
     ylab = "Quantile Residuals",
     main = "Quantile Residuals vs Fitted Values")
abline(h = 0, col = "blue")


