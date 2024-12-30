
##
## 7. knn clustering 
##

library(tidyverse)
library(ggplot2)

# correlation of energy v loudness
cor(ss_ArtSongAF$energy,ss_ArtSongAF$loudness)

# simple regression line for best fit - results all very similar
lm(energy~loudness,data=ss_ArtSongAF)
lm(energy~loudness,data=test)
lm(energy~loudness,data=train)

# look at knn: energy v loudness clustered by is_pop
# use 80% of data for training
samprate <- 0.8
nrow(sp_ArtSongAF)
numpoints <- nrow(sp_ArtSongAF)

# number of points in test set
numTestLabels <- round(numpoints * (1- samprate))

# rows for training set
training <- sample(1:numpoints, round(samprate * numpoints), replace=FALSE)
# make training set
train <- subset(sp_ArtSongAF[training,],select=c(energy,loudness))
length(training)
nrow(train)

# rows for test set
testing <- setdiff(1:numpoints,training)
# make test set
test <- subset(sp_ArtSongAF[testing,],select=c(energy,loudness))

# check test and train sets are correct lengths
length(testing)
nrow(test)
nrow(sp_ArtSongAF)==length(training)+length(testing)
nrow(sp_ArtSongAF)==nrow(train)+nrow(test)

# label the data sets
train_lab <- sp_ArtSongAF$is_pop[training]
true_lab <- sp_ArtSongAF$is_pop[testing]

install.packages("e1071") 
install.packages("caTools") 
install.packages("class")
library(e1071) 
library(caTools) 
library(class) 

# run the knn model, k=3
knn(train,test,train_lab,k=3)


# loop through for different values of k
for (k in 1:20){
  print(k)
  predlab <- knn(train,test,train_lab,k)
  
  num_incorrectlabels <- sum(predlab != true_lab)
  misclass_rate <- num_incorrectlabels/numTestLabels
  print(misclass_rate)
}


# best: k=18, mr=0.46
knn_model <- knn(train,test,train_lab,k=4)
length(knn_model)
nrow(test)
test$model <- knn_model

# label test results
# true-true and false-false = success, else fail
test$success <- (test$model == TRUE & test$is_pop == TRUE) | (test$model == FALSE & test$is_pop == FALSE)
View(test)
sum(test$success==TRUE)

length(true_lab)
length(predlab)

test1 <- c(0.31,0.71)
knn(train,test1,train_lab,k=5)
misclass_rate <- num_incorrectlabels/numTestLabels
misclass_rate
correct <- 1-misclass_rate
correct * 100

####################################################
# data plots
####################################################
model_output <- read.csv("model_output.csv")
names(model_output) <- c("k","data")
ggplot(model_output)+geom_point(aes(x=k,y=data),shape=4,size=5, colour="blue")+ylim(0.4,0.55)+
  labs(title="Figure 9. Misclassification rate for different k-values in knn model",x="k-value",y="misclassification rate")+
  theme_light()

# plot test data - TRUE means a successful prediction
ggplot()+
  geom_point(data=test_plot,shape=4,aes(x=energy,y=loudness,colour=success))+
  labs(title="Figure 9. Classification success of knn model of energy v loudness classified by popularity", caption="MusicoSet data",
       x="Energy",y="Loudness",)
  
# test set is 19% of data
nrow(test)/(nrow(train)+nrow(test))

# re-make training set with labels
train <- subset(sp_ArtSongAF[training,],select=c(energy,loudness,is_pop))
# re-make test set with labels
test <- subset(sp_ArtSongAF[testing,],select=c(energy,loudness,is_pop))

test_plot <- test
train_plot <- train

# plot all data
ggplot()+
  geom_point(data=train_plot,shape=2,aes(x=energy,y=loudness),colour="purple")+
  geom_point(data=test_plot,shape=4,aes(x=energy,y=loudness),colour="black")+
  labs(title="Figure 8. Test and training data used for knn model",caption="MusicOSet data",
       x="Energy",y="Loudness")

ggplot() +
  geom_point(data = train_plot, shape=2, aes(x = energy, y = loudness), color = "purple", alpha = 0.5) +
  geom_smooth(data = train_plot, aes(x = energy, y = loudness), method = "lm", color = "blue", se = FALSE) +
  geom_point(data = test_plot, shape=4, aes(x = energy, y = loudness), color = "black", alpha = 0.5) +
  geom_smooth(data = test_plot, aes(x = energy, y = loudness), method = "lm", color = "red", se = FALSE) +
  labs(title="Figure 10. Test and training data used for knn model with regression best fit lines",caption="MusicOSet data",
       x="Energy",y="Loudness")
