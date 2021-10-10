drzewo <- read.csv("./data/Movie_classification.csv", sep=",", header=TRUE, nrow=510, dec = ".", na.strings = " ")
drzewo$X3D_available <- as.factor(drzewo$X3D_available)
drzewo$Genre <- as.factor(drzewo$Genre)
library(dplyr)

drzewo %>%
  filter(!complete.cases(drzewo))

mean_Time_taken <- mean(drzewo$Time_taken, na.rm=TRUE)
drzewo$Time_taken[is.na(drzewo$Time_taken)] <- mean_Time_taken

library(dummies)
new_drzewo <- dummy.data.frame(drzewo, names=c("X3D_available","Genre"))

library(caTools)
drzewo_split <- sample.split(new_drzewo, SplitRatio = 2/3)
training_data <- subset(new_drzewo, drzewo_split == TRUE)
testing_data <- subset(new_drzewo, drzewo_split == FALSE)

library(rpart)
model <- rpart(training_data$Start_Tech_Oscar~., data=training_data, subset = c(sample(1:330, 330)))
library(maptree)
draw.tree(model, cex=0.7, nodeinfo=TRUE)

pred <- predict(model, testing_data)
testing_data$pred <- pred

for (i in 1:nrow(testing_data)){
  if (testing_data$pred[i] > 0.5) {
    testing_data$will_win[i] <- 1
  }
  else{
    testing_data$will_win[i] <- 0
  }
} 

confusion_matrix <- table(testing_data$Start_Tech_Oscar,testing_data$will_win)
confusion_matrix
barplot(confusion_matrix)
model_accuracy <- sum(confusion_matrix[1,1],confusion_matrix[2,2])/sum(confusion_matrix[1,1],confusion_matrix[2,1],confusion_matrix[1,2],confusion_matrix[2,2])
model_accuracy

