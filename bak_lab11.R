library("PogromcyDanych")
library("archivist")
auta <- auta2012
auta$nazwa <- rownames(auta)
auta$Cena_norm <- scale(sqrt(auta$Cena))
mean_km <- mean(auta$KM, na.rm=TRUE)
auta$KM[is.na(auta$KM)] <- mean_km
auta$KM_norm <- scale(sqrt(auta$KM))
head(auta)

library(dplyr)
set.seed(3243)
sample_auta <- sample_n(auta,57)

library(ggrepel)
ggplot(sample_auta, aes(Cena_norm, KM_norm, label=nazwa)) +
  geom_point(size=3, color="red") +
  geom_text_repel(color="darkgrey") + theme_bw()

grupy <- kmeans(sample_auta[,c("Cena_norm", "KM_norm")], 
                centers = 4, nstart = 10)

head(grupy$cluster)
grupy$centers

sample_auta$grupa <- factor(grupy$cluster)
centra <- data.frame(grupy$centers)
centra$nazwa <- centra$grupy <- factor(1:nrow(centra))

ggplot(sample_auta, aes(Cena_norm, KM_norm, color=grupa, label=nazwa)) +
  geom_text_repel(color="darkgrey") + 
  geom_point(size=3) + 
  geom_text(data=centra, size=8, color="black") +  theme_bw()

###########################################################
grupy_2 <- kmeans(sample_auta[,c("Cena_norm", "KM_norm")], 
                centers = 3, nstart = 10)

head(grupy_2$cluster)
grupy_2$centers

sample_auta$grupa_2 <- factor(grupy_2$cluster)
centra_2 <- data.frame(grupy_2$centers)
centra_2$nazwa <- centra_2$grupy_2 <- factor(1:nrow(centra_2))

ggplot(sample_auta, aes(Cena_norm, KM_norm, color=grupa_2, label=nazwa)) +
  geom_text_repel(color="darkgrey") + 
  geom_point(size=3) + 
  geom_text(data=centra_2, size=8, color="black") +  theme_bw()
############################################################
grupy_3 <- kmeans(sample_auta[,c("Cena_norm", "KM_norm")], 
                centers = 5, nstart = 10)

head(grupy_3$cluster)
grupy_3$centers

sample_auta$grupa_3 <- factor(grupy_3$cluster)
centra_3 <- data.frame(grupy_3$centers)
centra_3$nazwa <- centra_3$grupy_3 <- factor(1:nrow(centra_3))

ggplot(sample_auta, aes(Cena_norm, KM_norm, color=grupa_3, label=nazwa)) +
  geom_text_repel(color="darkgrey") + 
  geom_point(size=3) + 
  geom_text(data=centra_3, size=8, color="black") +  theme_bw()
############################################################
grupy$withinss
grupy$betweenss
grupy$totss

grupy_2$withinss
grupy_2$betweenss
grupy_2$totss

grupy_3$withinss
grupy_3$betweenss
grupy_3$totss

Kmax <- 10
WC <- sapply(2:Kmax, function(k) {
  grupy <- kmeans(sample_auta[,c("Cena_norm", "KM_norm")], 
                  centers = k, nstart = 10)
  sum(grupy$withinss)
})
WC
ggplot(data.frame(K=factor(2:Kmax), WC), aes(K, WC)) +
  geom_bar(stat="identity")
