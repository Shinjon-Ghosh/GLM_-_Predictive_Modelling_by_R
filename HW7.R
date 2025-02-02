## 7.5

library(GLMsData)
library(statmod)
data(nambeware)
head(nambeware)

lm2 = glm(Price ~ Diam, data = nambeware, family=Gamma(link="log"))
summary(lm2)

##1
printCoefmat(coef(summary(lm2)))
phi.meandev <- deviance(lm2) / df.residual(lm2)
phi.pearson <- summary(lm2)$dispersion
c(Mean.deviance=phi.meandev, Pearson=phi.pearson)
printCoefmat(coef(summary(lm2, dispersion=phi.meandev)))


##7
confint(lm2,level= 0.95)


##2
t.diam <- glm.scoretest( lm2, log(nambeware$Diam) )
p.diam <- 2 * pt( -abs(t.diam), df=df.residual(lm2) ) 
tab <- data.frame(Score.stat = t.diam, P.Value=p.diam )
print(tab, digits=3)

##3
lm0 <- glm( Price ~ 1, data=nambeware,family=Gamma(link="log"))
summary(lm0)

deviance(lm2)
deviance(lm0)
phi.meandev <- deviance(lm2) / df.residual(lm2)
phi.Pearson <- summary(lm2)$dispersion
print(phi.pearson)
phi = 0.1567253
L <- (deviance(lm0) - deviance(lm2))/phi
pchisq(L, 1, lower.tail=FALSE )

anova(lm0,lm2,test="F")
print(L)

##8

newA <- seq( min(nambeware$Diam), max(nambeware$Diam), length=100)
newB <- predict( lm2, newdata=data.frame(Diam=newA), type="response",
                   se.fit=TRUE)
plot(Price~Diam, data=nambeware)
lines(newB$fit ~ newA, lwd=2)
t.star <- qt(p=0.975, df=df.residual(lm2))
ci.lo <- (newB$fit - t.star * newB$se.fit)
ci.hi <- (newB$fit + t.star * newB$se.fit)
lines(ci.lo~newA, lty=2, col="green")
lines(ci.hi~newA, lty=2, col = "red")



## 8.10

library(GLMsData)
library(statmod)
seabirds <- data.frame(
  Species = c(rep("Murre", 10), rep("Crested auklet", 10), 
              rep("Least auklet", 10), rep("Puffin", 10)),
  Quadrat = rep(1:10, 4),
  Count = c(0, 0, 0, 1, 1, 0, 0, 1, 1, 3,
            0, 0, 0, 2, 3, 1, 5, 0, 1, 5,
            1, 2, 0, 0, 0, 0, 1, 3, 2, 3,
            1, 0, 1, 1, 0, 0, 3, 1, 1, 0)
)

##1
pl1 <- glm(Count ~ Species + factor(Quadrat), data = seabirds,
                 family = poisson(link = "log"))

## 2
min(seabirds$Count)

##3
# Calculate constant-information scale
constant_info_scale <- function(mu) {
  sqrt(mu * pl1$ family$variance(mu))
}

# Compute deviance residuals
dev_resid <- residuals(pl1, type = "deviance")

# Compute fitted values
fitted_values <- pl1$fitted.values

# Transform fitted values to constant-information scale
transformed_fitted_values <- constant_info_scale(fitted_values)

# Plot deviance residuals against fitted values
plot(fitted_values, dev_resid,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Fitted Values")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red")

# Plot deviance residuals against transformed fitted values
plot(transformed_fitted_values, dev_resid,
     xlab = "Transformed Fitted Values",
     ylab = "Deviance Residuals",
     main = "Deviance Residuals vs Transformed Fitted Values")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red")

##4
# Compute quantile residuals
quantile_resid <- qresid(pl1)

# Plot quantile residuals against fitted values
plot(fitted_values, quantile_resid,
     xlab = "Fitted Values",
     ylab = "Quantile Residuals",
     main = "Quantile Residuals vs Fitted Values")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red")

# Plot quantile residuals against transformed fitted values
plot(transformed_fitted_values, quantile_resid,
     xlab = "Transformed Fitted Values",
     ylab = "Quantile Residuals",
     main = "Quantile Residuals vs Transformed Fitted Values")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red")






