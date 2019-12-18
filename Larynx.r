library(nnet);
library(c50);

input <- read.csv(file = "Larynx.csv", header = TRUE, sep = ";")

input.norm <- data.frame();
input.path <- data.frame();

for (i in 1:nrow(input))
{
  if (input$Class[i] == " norm")
  {
    input.norm <- rbind(input.norm, input[i,]); 
  }  
  if (input$Class[i] == " path")
  {
    input.path <- rbind(input.path, input[i,]); 
  }  
}

indexs <- 1:30;
training.data <- rbind(input.norm[indexs,], input.path[indexs,]);
testing.data <- rbind(input.norm[-indexs,], input.path[-indexs,]);

neural.network <- nnet(Class~., data = training.data, size = 50);
coef(neural.network);

confusion.matrix <- table(predicted = predict(neural.network,  newdata = testing.data, type = "class"),
                          actual = testing.data$Class);
confusion.matrix;

error <- (confusion.matrix[2,1] + confusion.matrix[1,2])/ nrow(testing.data)*100; # [2,1] == 2 row 1 colum
cat('Procent of error: ',error); # it also can be just name of value, ex.: error

dt <- C50::C5.0(x = training.data[,-10], y = training.data[,10]);
summary(dt);
plot(dt);

confusionMatrix <- table(predicted = predict(dt, newdata = testing.data[,-10]),
                         actual = testing.data$Class);
confusionMatrix

accuracy <- (confusionMatrix[1,1] + confusionMatrix[2,2]) / nrow(testing.data);
accuracy;

correct <- (confusionMatrix[1,1] + confusionMatrix[2,2]) / nrow(testing.data);
incorrect <- (confusionMatrix[1,2] + confusionMatrix[2,1]) / nrow(testing.data);
  
pie(c(correct, incorrect), c("correct","incorrect")); # c - creating vector 

lable1 <- paste("correct", round(100*(confusionMatrix[1,1] + confusionMatrix[2,2])/nrow(testing.data), digits = 2),"%");
lable2 <- paste("incorrect", round(100*(confusionMatrix[1,2] + confusionMatrix[2,1])/nrow(testing.data), digits = 2),"%");
labelsT <- c(lable1, lable2);
pie(c(correct, incorrect), labels = labelsT);

