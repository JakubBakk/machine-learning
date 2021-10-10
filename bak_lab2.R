Train_BMS <- read.csv("./data/Train_BMS.csv", sep=",", header=TRUE, nrow= 8524, dec = ".", na.strings = " ")
library(dplyr)

hist(Train_BMS$Item_Weight, breaks=10, ylab="Ilosc", xlab="Waga")
hist(Train_BMS$Item_Visibility, breaks=10, ylab="Ilosc", xlab="Widzialnosc")
hist(Train_BMS$Item_MRP, breaks=10, ylab="Ilosc", xlab="MRP")
hist(Train_BMS$Item_Outlet_Sales, breaks=10, ylab=" Ilosc w przedziale", xlab="Iloscs sprzedanych produktow")
hist(Train_BMS$Outlet_Establishment_Year, breaks=10, ylab="Ilosc", xlab="Rok")
hist(Train_BMS$Item_Weight, breaks=10, ylab="Ilosc", xlab="Waga")

barplot(table(Train_BMS$Item_Fat_Content))
barplot(table(Train_BMS$Item_Type))
barplot(table(Train_BMS$Outlet_Size))
barplot(table(Train_BMS$Outlet_Location_Type))
barplot(table(Train_BMS$Outlet_Type))

Item_Weight_Median <- median(Train_BMS$Item_Weight, na.rm = TRUE)
Train_BMS$Item_Weight[is.na(Train_BMS$Item_Weight)] <- Item_Weight_Median

Item_Visibility_Median <- median(Train_BMS$Item_Visibility, na.rm = TRUE)
Train_BMS$Item_Visibility[Train_BMS$Item_Visibility == 0] <- Item_Visibility_Median

table(Train_BMS$Outlet_Size, Train_BMS$Outlet_Type)
levels(Train_BMS$Outlet_Size)[1] <- "Other"

my_data <- Train_BMS[-c(1,12)]
names(my_data)

my_data$Item_Fat_Content <- gsub("low fat", "Low Fat", my_data$Item_Fat_Content)
my_data$Item_Fat_Content <- gsub("LF", "Low Fat", my_data$Item_Fat_Content)
my_data$Item_Fat_Content <- gsub("reg", "Regular", my_data$Item_Fat_Content)


install.packages("dummies")

library("dummies")
new_my_data <- dummy.data.frame(my_data, names = c("Item_Fat_Content","Item_Type", "Outlet_Establishment_Year","Outlet_Size", "Outlet_Location_Type", "Outlet_Type"))
new_my_data <- new_my_data[-c(22)]

PCA_train <- prcomp(new_my_data, scale. = T)
summary(PCA_train)

PCA_train_sdev <- summary(PCA_train)$importance[1,]
PCA_train_sdev[1:3]
PCA_train_var <- PCA_train_sdev^2
PCA_train_var[1:4]
PCA_train_PoV <- PCA_train_var/sum(PCA_train_var) 
plot(PCA_train_PoV, xlab = "Index", ylab = "Percentage of Variance")
plot(cumsum(PCA_train_PoV), ylab = "Skumulowany procent wariancji")
summary(PCA_train)$importance[3,][27]
