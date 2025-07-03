library(tseries)
library(readxl)
library(ggplot2)
library(writexl)

df <- read_excel("Residuals & Fits.xlsx")
res <- df$Residuals[-1]
sqres <- df$`Squared Residuals`[-1]
fit <- df$Fits[-1]

##### GARCH Model Selection #####
-.5*1256*(1+log(2*pi*mean(res^2))) # log Likelihood ARCH(0)

for (i in 1:10){
  l <- logLik(garch(res, order = c(0,i), trace = F))
  print(l)
}

model <- garch(res, order = c(1,1), trace = F)
logLik(model)
summary(model)
-2*3240.729+2*(2+1)*(1256/(1256-2-2)) #AICc GARCH(1,1)

model$coef[1]/(1-model$coef[2]-model$coef[3]) # unconditional variance

##### GARCH Model Fit #####
arimamean <- 5.2471051799170780
h_t <- model$fitted.values[,1]^2
h_tplus1 <- coef(model)[1]+coef(model)[2]*res[1256]^2+coef(model)[3]*h_t[1256]
lower <- arimamean-qnorm(p = 1-0.05/2)*sqrt(h_tplus1)
upper <- arimamean+qnorm(p = 1-0.05/2)*sqrt(h_tplus1)
lower
upper
qnorm(p = 0.05,mean = arimamean,sd = sqrt(h_tplus1))

##### Conditional Variance #####
ConditionalVariance <- data.frame(h_t)
write_xlsx(ConditionalVariance, path = "C:\\Users\\Fu\\Desktop\\My PJ2\\Graph & Dataset\\Conditional Variance.xlsx")






