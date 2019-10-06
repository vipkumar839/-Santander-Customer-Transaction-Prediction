getwd()
rm(list=ls())
train_data=read.csv("train.csv",header = TRUE, sep = ",")
test_data=read.csv("test.csv",header = TRUE, sep = ",")
head(train_data)
#Target variable is not depedent on ID_code attribute
train_data$ID_code<-NULL
train_data$target<-as.factor(train_data$target)

#MISSING VALUE ANALYSIS

missinig_value= data.frame(apply(train_data,2,function(x){sum(is.na(x))}))
 #No missing value found.

#OUTLIER ANALYSIS
library("ggplot2")
library("scales")
cnames=colnames(train_data)
cnames=cnames[-1] # Removing the target variable from list

 for (i in 2:length(cnames))
{
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "target"), data = train_data)+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="Target")+
            ggtitle(paste("Box plot for",cnames[i])))
 }

gridExtra::grid.arrange(gn6,gn5,gn2,ncol = 3)



 for(i in cnames){
  
   val = train_data[,i][train_data[,i] %in% boxplot.stats(train_data[,i])$out]
   train_data = train_data[which(!train_data[,i] %in% val),]
 }


#COORELATION ANALYSIS
library(usdm)
VIfDf=data.frame(vif(train_data[,-1]))
vifcor(train_data[,-1], th = 0.7) #no variable have collinearity problem


#FEATURE SCALING
#Normalising Variables
for(i in cnames)
{
  train_data[,i] = (train_data[,i]- min(train_data[,i]))/(max(train_data[,i])-min(train_data[,i]))
}


#Dividing Data
library(caret)
trainIndex = createDataPartition(train_data$target, p = .70, list = FALSE)
train = train_data[ trainIndex,]
test  = train_data[-trainIndex,]

#Balancing Data
library(sampling)

table(train$target)
stratas = strata(train, c("target"), size = c(30000, 11973), method = "srswor")
Bal_data = getdata(train, stratas)
Bal_data<-Bal_data[,-c(202,203,204)]
write.csv(Bal_data,"BalancedData.csv")

#MODELLING

#Logistic Regression
logit_model = glm(target ~ ., data = Bal_data, family = "binomial")
summary(logit_model)
LogR_Predictions = predict(logit_model, newdata = test[,-1], type = "response")

LogR_Predictions = ifelse(LogR_Predictions > 0.5, 1, 0)

ConfMatrix_RF = table(test$target, LogR_Predictions)
ConfMatrix_RF
#Accuracy = TP+TN/(no. of rows)  88.37%
#Recall =  TP/(TP+FN)            55.38%
#Precision = TP/(TP+FP)          42.66
#FNR = FN/(FN+TP)                44.61%

#Decision Tree  
library(party)
tree <- ctree(target~.,data = Bal_data,controls = ctree_control(mincriterion = 0.90,minsplit = 200))
treePredcition=predict(tree,newdata=test[,-1],type="response")
ConfMatrix_DT= table(test$target, treePredcition)
ConfMatrix_DT

#Accuracy = TP+TN/(no. of rows)  84.12%
#Recall =  TP/(TP+FN)            21.36%
#Precision = TP/(TP+FP)          20.30%
#FNR = FN/(FN+TP)                78.63%

#Random Forest
library(randomForest)
library(e1071)
RF_model = randomForest(target ~ ., Bal_data, importance = TRUE, ntree = 50)
ForestPrediction=predict(RF_model, newdata=test[,-1],type="response")
ConfMatrix_RF= table(test$target, ForestPrediction)
confusionMatrix(ConfMatrix_RF)

#Accuracy = TP+TN/(no. of rows)  90.59%
#Recall =  TP/(TP+FN)            15.61%
#Precision = TP/(TP+FP)          56.72%
#FNR = FN/(FN+TP)                43.27%

##KNN Implementation
library(class)
KNN_Predictions = knn(Bal_data[,1:200], test[, 1:200], Bal_data$target, k = 7)
Conf_matrix = table(KNN_Predictions, test$target)
Conf_matrix


#Accuracy = TP+TN/(no. of rows)  90.11
#Recall =  TP/(TP+FN)            19.19%
#Precision = TP/(TP+FP)          0.37%
#FNR = FN/(FN+TP)                99.62%




