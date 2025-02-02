library(GLMsData)
data(nambeware)
head(nambeware)
?nambeware


##1
lm2 = glm(Price ~ Diam, data = nambeware, family=Gamma(link="log"))

## 2
summary(lm2)

## 3
deviance(lm2)

## 4
deviance(lm2)/df.residual(lm2)

## 5
summary(lm2)$dispersion

w <- weights(lm2, type="working")
e <- residuals(lm2, type="working")
sum( w * e^2 ) / df.residual(lm2)
