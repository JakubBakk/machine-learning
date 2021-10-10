dane <- read.csv("./data/House-Price.csv", sep=",", header=TRUE, nrow= 8524, dec = ".", na.strings = " ")
df <- data.frame(dane)
df <- df[-c(3,424),]
df <- df[-c(214),]

n_hos_beds <- mean(df$n_hos_beds, na.rm = TRUE)
df$n_hos_beds[is.na(df$n_hos_beds)] <- n_hos_beds
df <- df[,-17]

df$avg_distance <- rowMeans(df[,6:9])

df <- df[,-(6:9)]

library("dummies")

df <- dummy.data.frame(df, names = c("airport", "waterbody"))
df <- df[,-8]
df <- df[,-13]

glm.fit = glm(Sold~price, data = df, family = binomial)

summary(glm.fit)

glm.fit = glm(Sold~., data = df, family = binomial)

summary(glm.fit)

model_koncowy <- glm(formula = Sold~price+air_qual+room_num+teachers+poor_prop+n_hos_beds+avg_distance, data = df, family = binomial)
summary(model_koncowy)
step(model_koncowy, direction = "backward")

library(car)
vif(model_koncowy)
exp(coef(model_koncowy))
