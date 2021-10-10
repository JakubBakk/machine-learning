dane <- dget("./data/dane_pomiarowe2019_al_krasinskiego")

data_1 <- dane[, c("NO2", "PM10", "PM25")]

x1 <-data_1$NO2
y1 <-data_1$PM10
z1 <-data_1$PM25

good_1 <-complete.cases(x1,y1,z1)

x1 <- x1[good_1]
y1 <- y1[good_1]
z1 <- z1[good_1]

cor(x1,y1)
cor(x1,z1)
cor(y1,z1)

library(ggplot2)

df <- data.frame(y1,z1)
ggplot(data=df) + geom_point(aes(y1,z1))

model <- lm(y1~z1)
model$coefficients["z1"]

p1 <- ggplot(data=df) + geom_point(aes(y1,z1))
p1 + geom_smooth(aes(y1,z1), method = "lm", se = TRUE)

a <- 1.36977
b <- 9.65667

z1mean <- mean(z1)
y1mean <- mean(y1)
z1 <- z1 - z1mean
y1 <- y1 - y1mean

sum_of_the_squared <- function(z1,y1,a,b){
  sum <- 0
  for(i in seq_along(z1)){
    sum <- sum + (z1[i] - (a*y1[i]+b))^2
  }
  sum
}
