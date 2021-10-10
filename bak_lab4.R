dane <- read.csv("./data/lab4-dane.csv", sep=";", header=TRUE, nrow=1768, dec = ".", na.strings = " ")

dane <- dane[complete.cases(dane),]

summary(dane)
library(dplyr)
hist(dane$Wiek.M, breaks=10, ylab="Ilosc", xlab="Wiek")
dane$MasaUr <- as.numeric(dane$MasaUr)
hist(dane$MasaUr, breaks=10, ylab="Ilosc", xlab="MasaUr")
##note: w pliku z zadaniem nale¿alo dodatkowo zmienic przecinki na kropki, inaczej rstudio nie rozpoznawalo danych i traktowalo je jako character zamiast numeric
mean_Wiek.m <- mean(dane$Wiek.M)
dane$Wiek.M[dane$Wiek.M<= 0] <- mean_Wiek.m 

library(dummies)
df <- dummy.data.frame(dane, names=c("ï.¿Grupa"))
df <- df[,-2]

glm.fit = glm(ï.¿Grupabadana~., data = df, family = binomial)
summary(glm.fit)

model_koncowy <- glm(formula=ï.¿Grupabadana~Plec+MasaUr+KolCiazy+InfOddech+Palenie+WyksztM, data=df, family=binomial)
summary(model_koncowy)
step(model_koncowy, direction = "backward")

library(car)
vif(model_koncowy)
exp(coef(model_koncowy))