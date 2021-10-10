load("./data/nasiona.RData")

##do usuniecia: id = 164,165,184 (dane wykraczaja poza 3*sd w zwiezlosci, wiec uznaje je za outliery);id = 214,219 ( >>3*sd w wspolczynnik asymetrii)
df <- nasiona
df <- df[-c(154,155,169,197,201),]
df <- scale(df)
summary(df)

res.dist <- dist(df, method="euclidean")
as.matrix(res.dist)[1:7, 1:7]
df_cluster <- hclust(res.dist, method="ward.D2")
plot(df_cluster)
rect.hclust(df_cluster, k=5, border="blue")
groups <- cutree(df_cluster, k=5)
df <- cbind(df, groups)
df <- as.data.frame(df)

library(dplyr)

##podsumowanie
summary(subset(df, groups == 1))[4,1:7]
summary(subset(df, groups == 2))[4,1:7]
summary(subset(df, groups == 3))[4,1:7]
summary(subset(df, groups == 4))[4,1:7]
summary(subset(df, groups == 5))[4,1:7]

##liczebnosci grup
nrow(filter(df, groups == 1))
nrow(filter(df, groups == 2))
nrow(filter(df, groups == 3))
nrow(filter(df, groups == 4))
nrow(filter(df, groups == 5))

plot(df$powierzchnia~df$obwod, df, col=groups)

