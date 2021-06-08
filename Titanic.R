library(ranger)
library(caret)
library(data.table)
data <- read.csv("C:/Users/hp/Desktop/titanic.csv")
#0=No, 1=Yes
# Data Exploration
dim(data)
head(data,6)
tail(data,6)
table(data$Survived) #tells count
names(data) #column names
summary(data$Age)

library(RColorBrewer) #color Palette
barplot(prop.table(table(data$Survived)), col  = brewer.pal(3,"Set1"))  #prop.table returns num/total
barplot(prop.table(table(data$Survived, data$Sex)),col  = brewer.pal(3,"Set1"))
qplot(factor(Pclass), data=data, geom="bar", fill=factor(Survived)) #qplot plots different graphs together
#factor stores values as a vector

#Gender Visulization
a=table(data$Sex)
barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))
pct=round(a/sum(a)*100) #to calculate percentage
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")

library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")

#Visulization of Age
hist(data$Age,
     col="blue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)
boxplot(customer_data$Age,
        main="Boxplot for Descriptive Analysis of Age")

ggplot(data[1:891,], aes(x = Parents.Children.Aboard, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size')

# relationship between different categorical variables and survival
with(data, table(Pclass, Survived)) # Pclass=1: more likely to survive
with(data, table(Sex, Survived))
with(data, table(Age, Survived)) 

#Look for missing values
library(Amelia)
missmap(data, main="Missing Vs Observed")
#Encoding of Sex
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}
data$Sex=encode_ordinal(data[["Sex"]], order = c("male","female"))
table(data$Sex)
#Remove names
df <- data[ -c(1,3) ]
t<- data[-c(3)]
head(t)
head(df)

#correlation
library(GGally)
ggcorr(t,
       nbreaks = 6,
       label = TRUE,
       label_size = 3)
#Survived count
library(ggplot2)
ggplot(t, aes(x = Survived)) +
  geom_bar(width=0.5, fill = "coral") +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  theme_classic()

#Survived count by Sex
ggplot(t, aes(x = Survived, fill=factor(Sex))) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()

#k
library(NbClust)
library(factoextra)
library(cluster) 
fviz_nbclust(df, kmeans, method = "silhouette")

set.seed(125)
stat_gap <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)   #estimate the number of data clusters
fviz_gap_stat(stat_gap) #visulaize gap statistic



library(gridExtra)
library(grid)



k2<-kmeans(df,2,iter.max=100,nstart=50,algorithm="Lloyd")
sil <- silhouette(k2$cluster, dist(df,"euclidean"))
fviz_silhouette(sil)
k3<-kmeans(df,3,iter.max=100,nstart=50,algorithm="Lloyd")
sil <- silhouette(k3$cluster, dist(df,"euclidean"))
fviz_silhouette(sil)
k4<-kmeans(df,4,iter.max=100,nstart=50,algorithm="Lloyd")
sil <- silhouette(k4$cluster, dist(df,"euclidean"))
fviz_silhouette(sil)
k5<-kmeans(df,5,iter.max=100,nstart=50,algorithm="Lloyd")
sil <- silhouette(k5$cluster, dist(df,"euclidean"))
fviz_silhouette(sil)
k6<-kmeans(df,6,iter.max=100,nstart=50,algorithm="Lloyd")
sil <- silhouette(k6$cluster, dist(df,"euclidean"))
fviz_silhouette(sil)
k7<-kmeans(df,7,iter.max=100,nstart=50,algorithm="Lloyd")
sil <- silhouette(k7$cluster, dist(df,"euclidean"))
fviz_silhouette(sil)
k8<-kmeans(df,8,iter.max=100,nstart=50,algorithm="Lloyd")
sil <- silhouette(k8$cluster, dist(df,"euclidean"))
fviz_silhouette(sil)
k9<-kmeans(df,9,iter.max=100,nstart=50,algorithm="Lloyd")
sil <- silhouette(k9$cluster, dist(df,"euclidean"))
fviz_silhouette(sil)
k10<-kmeans(df,10,iter.max=100,nstart=50,algorithm="Lloyd")
sil <- silhouette(k10$cluster, dist(df,"euclidean"))
fviz_silhouette(sil)


#Kmeans
cluster.params <- 2:10
set.seed(893247)
clusters.sum.squares <- rep(0.0, 9)
for (i in cluster.params) {
  kmeans.temp <- kmeans(df, centers = i)
  clusters.sum.squares[i - 1] <- sum(kmeans.temp$withinss)
}   
clusters.sum.squares
ggplot(NULL, aes(x = cluster.params, y = clusters.sum.squares)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  labs(x = "Number of Clusters",
       y = "Cluster Sum of Squared Distances",
       title = "Titanic Training Data Scree Plot")
k2<-kmeans(df,2,iter.max=100,nstart=50,algorithm="Lloyd")
k2

t$Cluster <- as.factor(k2$cluster)

ggplot(t, aes(x = Cluster, fill = factor(Survived))) +
  theme_bw() +
  geom_bar() +
  labs(x = "Cluster Assignment",
       y = "Passenger Count",
       title = "Titanic Training Survivability by Cluster")

pcclust=prcomp(df,scale=FALSE) #principal component analysis
summary(pcclust)
head(pcclust)
kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}
digCluster<-k2$cluster; dignm<-as.character(digCluster); # K-means clusters
plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="survived")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))

#Original Dataset
plot(pcclust$x[,1:2], col =t$Survived,pch =19,xlab ="K-means",ylab="survived")
legend("bottomleft",unique(dignm),fill=t$Survived)

t$Survived<-t$Survived+1
head(t)
conf <- table(Actualvalue=t$Survived, PredictedValue=t$Cluster)
conf
AV <- conf[1,1]
AVT <- conf[2,2]
PV <- conf[1,2]
PVF <- conf[2,1]
acc = (AV+AVT)/(AV+AVT+PV+PVF)
print(acc)

#DBSCAN
library(fpc)
set.seed(220)
Dbscan_cl <- dbscan(df, eps = 0.45, MinPts = 5)
Dbscan_cl
Dbscan_cl$cluster
conf<-table(Dbscan_cl$cluster, t$Survived)
conf
AV <- conf[1,1]
AVT <- conf[2,2]
PV <- conf[1,2]
PVF <- conf[2,1]
acc = (AV+AVT)/(AV+AVT+PV+PVF)
print(acc)


#Data Modelling
library(caTools)
set.seed(123)
head(data)
titanic=data[-c(3)]
head(titanic)
data_sample = sample.split(titanic$Survived,SplitRatio=0.80)
train_data = subset(titanic,data_sample==TRUE)
test_data = subset(titanic,data_sample==FALSE)
dim(train_data)
dim(test_data)

#Fitting Logistic Regression Model
Logistic_Model=glm(Survived~.,train_data,family=binomial())
summary(Logistic_Model)
confint(Logistic_Model)


#Model Performance Evaluation
predict <- predict(Logistic_Model, newdata= subset(test_data,type="response"))
head(predict)
conf <- table(Actualvalue=test_data$Survived, PredictedValue=predict>0.5)
conf

AV <- conf[1,1]
AVT <- conf[2,2]
PV <- conf[1,2]
PVF <- conf[2,1]
acc = (AV+AVT)/(AV+AVT+PV+PVF)
print(acc)

#ROC curve
library(pROC)
lr.predict <- predict(Logistic_Model,test_data, probability = TRUE)
auc.gbm = roc(test_data$Survived, lr.predict, plot = TRUE, col = "blue")

#Decision Tree
library(rpart)
library(rpart.plot)
decisionTree_model <- rpart(Survived ~ . , train_data, method = 'class')
predicted_val <- predict(decisionTree_model, test_data, type = 'class')
probability <- predict(decisionTree_model, test_data, type = 'prob')
rpart.plot(decisionTree_model)

table = table(test_data$Survived, predicted_val)
dt_accuracy = sum(diag(table)) / sum(table)
paste("The accuracy is : ", dt_accuracy)

#Split
data <- read.csv("C:/Users/hp/Desktop/titanic.csv")
data_sample = sample.split(titanic$Survived,SplitRatio=0.80)
train_data = subset(titanic,data_sample==TRUE)
test_data = subset(titanic,data_sample==FALSE)


#Encoding
data <- read.csv("C:/Users/hp/Desktop/titanic.csv")
data_sample = sample.split(titanic$Survived,SplitRatio=0.80)
train_data = subset(titanic,data_sample==TRUE)
test_data = subset(titanic,data_sample==FALSE)


#Artificial neural network
library(neuralnet)
ANN_model =neuralnet (Survived~.,train_data,linear.output=FALSE)
plot(ANN_model)
predANN=compute(ANN_model,test_data)
resultANN=predANN$net.result
resultANN=ifelse(resultANN>0.5,1,0)
conf <- table(Actualvalue=test_data$Survived, PredictedValue=resultANN)
conf
AV <- conf[1,1]
AVT <- conf[2,2]
PV <- conf[1,2]
PVF <- conf[2,1]
acc = (AV+AVT)/(AV+AVT+PV+PVF)
print(acc)

#Gradient Boosting
library(gbm)
# Get the time to train the GBM model
system.time(
  model_gbm <- gbm(Survived ~ .  #formula used
                   , distribution = "bernoulli" #for 0-1 outcomes
                   , data = rbind(train_data, test_data)
                   , n.trees = 500
                   , interaction.depth = 3
                   , n.minobsinnode = 100  #minimum num of observation in terminal node
                   , shrinkage = 0.01  #learning rate
                   , bag.fraction = 0.5 #randomness
                   , train.fraction = nrow(train_data) / (nrow(train_data) + nrow(test_data))
  )
)
# Determine best iteration based on test data
gbm.iter = gbm.perf(model_gbm, method = "test")

model.influence = relative.influence(model_gbm, n.trees = gbm.iter, sort. = TRUE) #importance if each var in training model
#Plot the gbm model
plot(model_gbm)

# Plot and calculate AUC on test data
gbm_test = predict(model_gbm, newdata = test_data, n.trees = gbm.iter)
gbm_auc = roc(test_data$Survived, gbm_test, plot = TRUE, col = "red")

print(gbm_auc)

#Naive Bayes
library(e1071)
nb_model = naiveBayes(Survived ~., data=train_data)
nb_predict = predict(nb_model,test_data)
table_mat = table(nb_predict, test_data$Survived)
nb_accuracy = sum(diag(table_mat)) / sum(table_mat)
nb_accuracy

#Knn
library(class)
library(dummies)
train_test_split = function(data, fraction = 0.8, train = TRUE) {
  total_rows = nrow(data)
  train_rows = fraction * total_rows
  sample = 1:train_rows
  if (train == TRUE) {
    return (data[sample, ])
  } else {
    return (data[-sample, ])
  }
}
library(dplyr)
# one hot encoding using dummy
ohdata = cbind(titanic, dummy(titanic$Pclass))
ohdata = cbind(ohdata, dummy(ohdata$Sex))
# drop original factor variables
ohdata$Pclass = NULL
ohdata$Sex = NULL
ohtrain = train_test_split(ohdata, 0.8, train = TRUE)
ohtest = train_test_split(ohdata, 0.8, train = FALSE)
train_labels = select(ohtrain, Survived)[,1]
test_labels = select(ohtest, Survived)[,1]
# drop labels for prediction
ohtrain$Survived=NULL
ohtest$Survived=NULL
knn_predict = knn(train = ohtrain,
                  test = ohtest,
                  cl = train_labels,
                  k=10)
table_mat = table(knn_predict, test_labels)
accuracy_knn = sum(diag(table_mat)) / sum(table_mat)
accuracy_knn
