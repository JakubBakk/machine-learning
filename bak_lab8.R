drzewo <- read.csv("./data/Movie_classification.csv", sep=",", header=TRUE, nrow=510, dec = ".", na.strings = " ")
drzewo$X3D_available <- as.factor(drzewo$X3D_available)
drzewo$Genre <- as.factor(drzewo$Genre)
drzewo$Start_Tech_Oscar <- as.factor(drzewo$Start_Tech_Oscar)
library(dplyr)

drzewo %>%
  filter(!complete.cases(drzewo))

mean_Time_taken <- mean(drzewo$Time_taken, na.rm=TRUE)
drzewo$Time_taken[is.na(drzewo$Time_taken)] <- mean_Time_taken

library(dummies)
new_drzewo <- dummy.data.frame(drzewo, names=c("X3D_available","Genre"))

library(caTools)
set.seed(120)
drzewo_split <- sample.split(new_drzewo$Start_Tech_Oscar, SplitRatio = 0.7)
training_data <- subset(new_drzewo, drzewo_split == TRUE)
testing_data <- subset(new_drzewo, drzewo_split == FALSE)

library(e1071)

model_linear <- svm(training_data$Start_Tech_Oscar~.,data=training_data, kernel="linear", cost=1, cross=4)
model_linear        ##Support vectors: 261


pred_linear1 <- predict(model_linear, testing_data)
testing_data$pred_linear1 <- pred_linear1

confusion_matrix_linear1 <- table(testing_data$Start_Tech_Oscar,testing_data$pred_linear1)
confusion_matrix_linear1
barplot(confusion_matrix_linear1)
model_accuracy_linear1 <- sum(confusion_matrix_linear1[1,1],confusion_matrix_linear1[2,2])/sum(confusion_matrix_linear1[1,1],confusion_matrix_linear1[2,1],confusion_matrix_linear1[1,2],confusion_matrix_linear1[2,2])
model_accuracy_linear1*100    

tune.out_linear <- tune(svm, Start_Tech_Oscar~., data = training_data, kernel="linear", ranges =list(cost=c(0.001 , 0.01, 0.1, 1,10,100)), cross=4)
summary(tune.out_linear$best.model)

pred_linear2 <- predict(tune.out_linear$best.model, testing_data)
testing_data$pred_linear2 <- pred_linear2


confusion_matrix_linear2 <- table(testing_data$Start_Tech_Oscar,testing_data$pred_linear2)
confusion_matrix_linear2
barplot(confusion_matrix_linear2)
model_accuracy_linear2 <- sum(confusion_matrix_linear2[1,1],confusion_matrix_linear2[2,2])/sum(confusion_matrix_linear2[1,1],confusion_matrix_linear2[2,1],confusion_matrix_linear2[1,2],confusion_matrix_linear2[2,2])
model_accuracy_linear2*100    
=========================================================

model_polynomial <- svm(Start_Tech_Oscar~.,data=training_data, kernel="polynomial", cost=1, degree=1, cross=4)
model_polynomial    ##Support vectors: 308

pred_polynomial1 <- predict(model_polynomial, testing_data)
testing_data$pred_polynomial1 <- pred_polynomial1

confusion_matrix_polynomial1 <- table(testing_data$Start_Tech_Oscar,testing_data$pred_polynomial1)
confusion_matrix_polynomial1
barplot(confusion_matrix_polynomial1)
model_accuracy_polynomial1 <- sum(confusion_matrix_polynomial1[1,1],confusion_matrix_polynomial1[2,2])/sum(confusion_matrix_polynomial1[1,1],confusion_matrix_polynomial1[2,1],confusion_matrix_polynomial1[1,2],confusion_matrix_polynomial1[2,2])
model_accuracy_polynomial1*100    

tune.out_polynomial <- tune(svm, Start_Tech_Oscar~., data = training_data, kernel="polynomial", ranges =list(cost=c(0.001 , 0.01, 0.1, 1,10,100),degree=c(0.5,1,2,3,5)),cross=4)
summary(tune.out_polynomial$best.model)

pred_polynomial2 <- predict(tune.out_polynomial$best.model, testing_data)
testing_data$pred_polynomial2 <- pred_polynomial2


confusion_matrix_polynomial2 <- table(testing_data$Start_Tech_Oscar,testing_data$pred_polynomial2)
confusion_matrix_polynomial2
barplot(confusion_matrix_polynomial2)
model_accuracy_polynomial2 <- sum(confusion_matrix_polynomial2[1,1],confusion_matrix_polynomial2[2,2])/sum(confusion_matrix_polynomial2[1,1],confusion_matrix_polynomial2[2,1],confusion_matrix_polynomial2[1,2],confusion_matrix_polynomial2[2,2])
model_accuracy_polynomial2*100  
==========================================================
  
model_radial <- svm(Start_Tech_Oscar~.,data=training_data, kernel="radial", cost=1, gamma=1)
model_radial    ##Support vectors: 354

pred_radial1 <- predict(model_radial, testing_data)
testing_data$pred_radial1 <- pred_radial1

confusion_matrix_radial1 <- table(testing_data$Start_Tech_Oscar,testing_data$pred_radial1)
confusion_matrix_radial1
barplot(confusion_matrix_radial1)
model_accuracy_radial1 <- sum(confusion_matrix_radial1[1,1],confusion_matrix_radial1[2,2])/sum(confusion_matrix_radial1[1,1],confusion_matrix_radial1[2,1],confusion_matrix_radial1[1,2],confusion_matrix_radial1[2,2])
model_accuracy_radial1*100    

tune.out_radial <- tune(svm, Start_Tech_Oscar~., data = training_data, kernel="radial", ranges =list(cost=c(0.001 , 0.01, 0.1, 1,10,100),gamma=c(0.01, 0.1,0.5,1,2,3,4,10, 50)),cross=4)
summary(tune.out_radial$best.model)

pred_radial2 <- predict(tune.out_radial$best.model, testing_data)
testing_data$pred_radial2 <- pred_radial2


confusion_matrix_radial2 <- table(testing_data$Start_Tech_Oscar,testing_data$pred_radial2)
confusion_matrix_radial2
barplot(confusion_matrix_radial2)
model_accuracy_radial2 <- sum(confusion_matrix_radial2[1,1],confusion_matrix_radial2[2,2])/sum(confusion_matrix_radial2[1,1],confusion_matrix_radial2[2,1],confusion_matrix_radial2[1,2],confusion_matrix_radial2[2,2])
model_accuracy_radial2*100  