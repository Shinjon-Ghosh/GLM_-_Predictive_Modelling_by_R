library(GLMsData)
#1
#data frame
data("humanfat")
humanfat
variable_names=names(humanfat)
print(variable_names)

#2
str(humanfat)

#qualitative variables
ql_var = sapply(humanfat, is.factor)

#quantitative variables
qn_var = sapply(humanfat, is.numeric)

print(names(humanfat)[ql_var])
print(names(humanfat)[qn_var])

#3
#create dummy variables using treatment coding
dummy_var = model.matrix(~Gender - 1, data = humanfat)
head(humanfat)
humanfat = cbind(humanfat, dummy_var)
head(humanfat)

#4
# summary
summary(humanfat)

#5
# scatter plot of quantitative variables
plot(humanfat$BMI, humanfat$Percent.Fat, main = "Scatter Plot: Percent.Fat vs BMI", xlab = "BMI", ylab = "Percent.Fat")

plot(humanfat$Age, humanfat$Percent.Fat, main = "Scatter Plot: Percent.Fat vs Age", xlab = "Age", ylab = "Percent.Fat")

#box plot for categorical variables
boxplot(Percent.Fat ~ Gender, data = humanfat, main = "Boxplot: Percent.Fat by Gender", xlab = "Gender", ylab = "Percent.Fat")

#6 regression line
model = lm(Percent.Fat~Age+BMI+Gender, data=humanfat)
par(mfrow= c(2, 2))
plot(model)

#7.
?humanfat

#9 
model1= lm(Percent.Fat~BMI, data = humanfat)
summary(model1)


# 10 glm model
glm_model <- glm(log(Percent.Fat) ~ BMI + Gender, data = humanfat)
summary(glm_model)




