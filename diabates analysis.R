library(mclust)
diabetes_data <- diabetes
#Cel badania: Celem moich badañ jest stworzenie modelu rozponajacego stan danej osoby(zdrowa, ktory rodzaj cukrzycy) na podstawie 3 wskaznikow: 
#poziom glukozy, poziom insuliny oraz sspg. W tym celu najpierw wstepnie przetworze i uporzadkuje dane, a nastepnie skorzystam z paru modeli: najpierw
#Maszyny wektorów nosnych z roznymi wspolczynnikami, aby zobaczyc ktory model bedzie dzialal najlepiej

#Uporzadkowanie danych
is.na.data.frame(diabetes_data)
plot(diabetes_data$class) #sprawdzenie czy nie ma jakis loose kategorii
summary(diabetes_data) #sprawdzenie czy nie ma jakis loose kategorii + czy wszystkie dane sa prawidlowe
hist(diabetes_data$insulin) #jedna dana wyglada na nieprawidlowa, poniewaz ta osoba zostala zaklasyfikowana jako "chemical" podejrzewam, ze
#ktos napisal niewlasciwa(za niska) wartosc, dlatego zastapie ja srednia wartosci insulin
mean_chemical <- mean(diabetes_data$insulin)
diabetes_data$insulin[diabetes_data$insulin < 100] <- mean_chemical
#Reszta danych wyglada prawidlowo i nie ma potrzeby dalszego przetwarzania danych, przechodze do podzialu zbioru danych

library(caTools)
library(dplyr)
#Ustawiam seeda, aby moc pozniej odtwarzac te same dane dla roznych metod by moc je potem porownac
set.seed(52)
#Dziele dane na zestaw treningowy oraz testowy, potem dodatkowo skorzystam z k-folda z powodu malej ilosci danych, aby dostac jeszcze lepsze
#wyniki
diabetes_split <- sample.split(diabetes_data$class, SplitRatio = 0.7)
training_data <- subset(diabetes_data, diabetes_split == TRUE)
testing_data <- subset(diabetes_data, diabetes_split == FALSE)


#Jako moj model korzystam z SVM, ktory bardzo dobrze dziala na tego typu danych.
# zbior jest maly dlatego nie powinno to dlugo zajac. Na poczatku bedzie to model lineary z k-foldem rownym 4
library(e1071)
#najpierw linear
model_svm_1 <- svm(class~., data=training_data, kernel="linear", cost=1, cross=4)
model_svm_1
pred_svm_1 <- predict(model_svm_1, testing_data)
testing_data$pred_svm_1 <- pred_svm_1
library(caret) #Do tworzenia confusion matrix automatycznie
confusionMatrix(testing_data$class, testing_data$pred_svm)
#Sprawdzam najlepszy model dla linear
tune.out_linear <- tune(svm, class~., data=training_data, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 10, 100)), cross=4)
summary(tune.out_linear$best.model) #najlepszy model to ten z cost=10
pred_svm_linear <- predict(tune.out_linear$best.model, testing_data)
testing_data$pred_svm_linear <- pred_svm_linear
confusionMatrix(testing_data$class, testing_data$pred_svm_linear) #accuracy = 0.88

#polynomial
tune.out_polynomial <- tune(svm, class~., data=training_data, kernel="polynomial", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 10, 100), degree=c(0.5,1,2,3,4,5)), cross=4)
summary(tune.out_polynomial$best.model)
pred_svm_polynomial <- predict(tune.out_polynomial$best.model, testing_data)
testing_data$pred_svm_polynomial <- pred_svm_polynomial
confusionMatrix(testing_data$class, testing_data$pred_svm_polynomial) #accuracy = 0.863

#radial
tune.out_radial <- tune(svm, class~., data=training_data, kernel="radial", ranges=list(cost=c(0.001,0.01,0.1,1,10,100),gamma=c(0.01,0.1,0.5,1,2,3,4,5,10,50)),cross=4)
summary(tune.out_radial$best.model)
pred_svm_radial <- predict(tune.out_radial$best.model, testing_data)
testing_data$pred_svm_radial <- pred_svm_radial
confusionMatrix(testing_data$class, testing_data$pred_svm_radial) #accuracy = 0.8409

#wniosek: Najlepszy model w tym wypadku to svm linearny z kosztem = 10, a jego dokladnosc wynosi = 0.88