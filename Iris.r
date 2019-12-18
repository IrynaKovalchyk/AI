library(nnet);
library(c50);


input <- read.csv(file = "Iris.csv", header = TRUE, sep = ";")

input.setosa <- data.frame();
input.versicolor <- data.frame();
input.virginica <- data.frame();


for (i in 1:nrow(input))
{
  if (input$Class[i] == "Iris-setosa")
  {
    input.setosa <- rbind(input.setosa, input[i,]); 
  }  
  if (input$Class[i] == "Iris-versicolor")
  {
    input.versicolor <- rbind(input.versicolor, input[i,]); 
  }  
  if (input$Class[i] == "Iris-virginica")
  {
    input.virginica <- rbind(input.virginica, input[i,]); 
  }  
}

indexs <- 1:30;
trainingIris.data <- rbind(input.setosa[indexs,], input.versicolor[indexs,], input.virginica[indexs,]);
testingIris.data <- rbind(input.setosa[-indexs,], input.versicolor[-indexs,], input.virginica[-indexs,]);

neural.network1 <- nnet(Class~., data = trainingIris.data, size = 50);
coef(neural.network1);

confusionIris.matrix <- table(predicted = predict(neural.network1,  newdata = testingIris.data, type = "class"),
                          actual = testingIris.data$Class);
confusionIris.matrix;

error1 <- (confusionIris.matrix[1,1] + confusionIris.matrix[2,2] +confusionIris.matrix[3,3])/ nrow(testingIris.data)*100;
error1

cat('Procent of error: ',error1);

dtree <- C50::C5.0(x = testingIris.data[,-5], y = testingIris.data[,5], z = testingIris.data[,-5]);
summary(dtree);
plot(dtree);

confusionMatrixIris <- table(predicted = predict(dtree, newdata = testingIris.data[,-5]),
                         actual = testingIris.data$Class);
confusionMatrixIris

accuracy1 <- (confusionMatrixIris[1,1] + confusionMatrixIris[2,2] + confusionMatrixIris[3,3]) / nrow(testingIris.data);
accuracy1;

IrisSetosa <- (confusionMatrixIris[1,1] + confusionMatrixIris[2,1] + confusionMatrixIris[3,1])/ nrow(testingIris.data);
IrisVersicolor <- (confusionMatrixIris[2,1] + confusionMatrixIris[2,2] + confusionMatrixIris [2,3]) / nrow(testingIris.data);
IrisVirginica <- (confusionMatrixIris[3,1] + confusionMatrixIris[3,2] + confusionMatrixIris [3,3]) / nrow(testingIris.data);
pie(c(IrisSetosa, IrisVersicolor, IrisVirginica), c("Iris-setosa","Iris-versicolor","Iris-virginica"));



lable1 <- paste("Iris-setosa", round(100*(confusionMatrixIris[1,1] + confusionMatrixIris[2,1] + confusionMatrixIris[3,1])/nrow(testingIris.data), digits = 2),"%");
lable2 <- paste("Iris-versicolor", round(100*(confusionMatrixIris[2,1] + confusionMatrixIris[2,2] + confusionMatrixIris [2,3])/nrow(testingIris.data), digits = 2),"%");
lable3 <- paste("Iris-virginica", round(100*(confusionMatrixIris[3,1] + confusionMatrixIris[3,2] + confusionMatrixIris [3,3])/nrow(testingIris.data), digits = 2),"%");
labelsT <- c(lable1, lable2, lable3);
pie(c(IrisSetosa, IrisVersicolor,IrisVirginica), labels = labelsT);



