library(ggplot2) library(reshape2) library(corrplot) library(dplyr) library(car) library(MASS)
mydata <- read.csv("/Users/lenhi/Downloads/Admission_Predict.csv") mydata
#drop column of number
df = subset(mydata, select = -c( Serial.No.))
#transfer GPA to 4.0 scale
df$CGPA = as.numeric(df$CGPA)
df$CGPA = round((df$CGPA/10)*4, digits = 2) df2 <- melt(df,id.vars= 'Chance.of.Admit')
#Scatter plots chance of admit against 6 variables ggplot(df2) +
geom_jitter(aes(value,Chance.of.Admit, colour=variable),) +
 geom_point(aes(value,Chance.of.Admit, colour=variable)) + facet_wrap(~variable, scales="free_x")
#Chance Of Admit distribution
ggplot(df, mapping = aes(x = Chance.of.Admit)) + geom_histogram(breaks = seq(0, 1, 0.03),
color = "blue", fill = 'light blue') + labs(title = "CHANCE OF ADMIT", x = "Chance",
y= "Count")
#Correlation Plot
corrplot(cor, method="color", type = "upper")
#VARIABLE SELECTION, Full model and reduced model
fit <- lm(data = df, Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating + SOP +
LOR + CGPA + Research) summary(fit)
reduced <- lm(data = df, Chance.of.Admit ~ GRE.Score + TOEFL.Score + LOR + CGPA + Research)
summary(reduced) vif(fit)
##ANOVA TABLE anova(reduced, fit) #p-value = 0.481 > 0.05
#RESIDUAL ANALYSIS PRE TRANSFORMATION
#Standardized Residuals

 stdres(reduced) #Studentized Residuals studres(reduced) #R-Student Residuals rstudent(reduced)
range(stdres(reduced))
barplot(height = stdres(reduced), names.arg = 1:400, main = "Standardized Residuals",
xlab = "Index", ylab = "Standardized Resid", ylim=c(-6,6), cex.names = 0.8) 20
abline(h=4, col = "Red", lwd=2)
abline(h=-4, col = "Red", lwd=2)
range(studres(reduced))
barplot(height = studres(reduced), names.arg = 1:400, main = "Studentized Residuals",
xlab = "Index", ylab = "Studentized Resid", ylim=c(-6,6), cex.names = 0.8) abline(h=4, col = "Red", lwd=2)
abline(h=-4, col = "Red", lwd=2)
range(rstudent(reduced))
barplot(height = rstudent(reduced), names.arg = 1:400, main = "R Student Residuals",
xlab = "Index", ylab = "R Student Resid", ylim=c(-6,6), cex.names = 0.8) abline(h=4, col = "Red", lwd=2)
abline(h=-4, col = "Red", lwd=2)
#Influential Point Analysis
dfbetasPlots(reduced, intercept = TRUE) influenceIndexPlot(reduced)
#Histogram and QQ Plot

 par(mfrow=c(1,2))
hist(studres(reduced), breaks=10, freq=F, col="pink",
cex.axis=1.5, cex.lab=1.5, cex.main=2) qqPlot(reduced)
# Fitted against residuals pre
par(mfrow=c(1,1))
residualPlot(reduced, type = "rstudent", quadratic = FALSE, col = 'pink',
pch = 16, cex = 0.75, cex.axis = 1.5, cex.lab = 1.5, ylab = "R-Student Residuals")
# Residuals against regressors pre
residualPlots(reduced, type = "rstudent", fitted = FALSE, quadratic = FALSE, col = "#ff5a60", pch = 16, cex = 0.75, cex.axis = 1,
cex.lab = 1.3, ylab = "R-Student Residuals")
#TRANSFORMATION
bc <- boxCox(df$Chance.of.Admit ~ df$GRE.Score + df$TOEFL.Score + df$LOR + df$CGPA + df$Research,
lambda = seq(-3,3,1/10)) bc$x[which.max(bc$y)]
bc.power <- bc$x[which.max(bc$y)]
# Our plot of the residuals against the fitted values showed non constant error # variance, so we will transform the response, price, using the square
# method.
df_test <- df
df_test$Chance.of.Admit <- (df$Chance.of.Admit)^2
transformed <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score +
LOR + CGPA + Research, data = df_test ) summary(transformed)
# RESIDUAL ANALYSIS POST TRANSFORMATION

range(stdres(transformed))
barplot(height = stdres(transformed), names.arg = 1:400,
main = "Standardized Residuals - Post Transformation", xlab = "Index", ylab = "Standardized Residual", ylim = c(-4, 4))
abline(h = 3, col = "#ff5a60", lwd = 2) abline(h = -3, col = "#ff5a60", lwd = 2)
par(mfrow=c(1,2))
hist(studres(transformed), breaks=10, freq=F, col="pink",
cex.axis=1.5, cex.lab=1.5, cex.main=2) qqPlot(transformed)
par(mfrow=c(1,1))
residualPlot(transformed, type="rstudent", quadratic=F, col = 'pink', pch=20, cex=1.5, cex.axis=1.5,
cex.lab=1.5)
