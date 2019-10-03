#  Load Packages
library(tidyverse)
library(e1071)
# Load Dataset and Checking the structure of the data from data.frame

str(iris)

# scatter color plot 

qplot(iris$Petal.Length,iris$Petal.Width, data = iris,color = iris$Species)

# train Radial SVN-Cernel

mymodel <- svm(Species~., data = iris)
summary(mymodel)

#SVM classification Plot using Radial Kernel 
plot(mymodel, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3,Sepal.Length = 4))

#train Linear SVN-Cernel 
mymodel1 <- svm(Species~., data = iris, kernel = "linear")
summary(mymodel1)

#SVM classification Plot using Linear kernel 
plot(mymodel1, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3,Sepal.Length = 4))

#train Polynomial SVN-Cernel 
mymodel2 <- svm(Species~., data = iris, kernel = "polynomial")
summary(mymodel2)

#SVM classification Plot using Polynomial Kernel 
plot(mymodel2, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3,Sepal.Length = 4))

#train Sigmoid SVN-Cernel 
mymodel3 <- svm(Species~., data = iris, kernel = "sigmoid")
summary(mymodel3)

#SVM classification Plot using Signoid Kernel 
plot(mymodel3, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3,Sepal.Length = 4))

#confusing Matrix 

mypred <- predict(mymodel3, iris)
Con_Mat <- table(predict = mypred, actual = iris$Species)
Con_Mat

# isclassification rate
1 - sum(diag(Con_Mat))/sum(Con_Mat)


# Tuning our model to find the best classification 

set.seed(123)
Tuning_model <- tune(svm, Species~., data = iris, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:7)))
# Performance of SVM
plot(Tuning_model)     
summary(Tuning_model)

#Best Model

BestModel <- Tuning_model$best.model
summary(BestModel)

# PLot of the best Model
plot(BestModel, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3,Sepal.Length = 4))

#Misclassification rate
mybestpred <- predict(BestModel, iris)
Con_Mat <- table(predict = mybestpred, actual = iris$Species)
Con_Mat
