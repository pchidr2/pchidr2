#Packages

library("rpart")
library("ggplot2")
library("tidyverse")
library("psych")
library("corrplot")
library("RColorBrewer")
library(dplyr)
library(caTools)
library(caret)
library("randomForest")
install.packages("randomForest")


#Load Dataset
data_set<-read.csv("C:/Users/Prajwal/Downloads/Training Dataset.arff",header=FALSE, comment.char="@")

#Renaming the columns
names(data_set)<-c('having_IP_Address', 'URL_Length', 'Shortening_Service', 'having_At_Symbol', 'double_slash_redirecting', 'Prefix_Suffix', 'having_Sub_Domain', 'SSLfinal_State', 'Domain_registeration_length', 'Favicon', 'port', 'HTTPS_token', 'Request_URL', 'URL_of_Anchor', 'Links_in_tags', 'SFH', 'Submitting_to_email', 'Abnormal_URL', 'Redirect', 'on_mouseover', 'RightClick', 'popUpWidnow', 'Iframe', 'age_of_domain', 'DNSRecord', 'web_traffic', 'Page_Rank', 'Google_Index', 'Links_pointing_to_page', 'Statistical_report', 'Result')
#summary of the dataset
summary(data_set)

#Starting Elements of the dataset
head(data_set)
#If we have only -1 and 1 in a column then -1 = phishing and 1 = legitimate #If we have -1,0,1 values in a column then -1=legitimate, 0=suspicious, 1= phishing

#Exploratory Data Analysis
str(data_set)
#checking for missing values
sapply(data_set,function(x) sum(is.na(x)))

#Detecting outliers using boxplot
boxplot(data_set,main="Detection of Outliers using Boxplot")

#Calculate Correlation Matrix
correlationMatrix<-cor(data_set)

#summarize Correlation Matrix
print(correlationMatrix)


#Plot correlation Matrix
corrplot(correlationMatrix,type = "upper",order="hclust",col = brewer.pal(n=8,name="RdYlBu"),tl.cex =0.5)

#Finding attributes that are highly correlated
highlyCorrelated<- findCorrelation(correlationMatrix, cutoff=0.8,verbose=TRUE)
print(highlyCorrelated)

# From the output we can see that columns SSLfinal_State, URL_of_Anchor, web_traffic, having_Sub_Domain, Domain_registeration_length,Request_URL are highly correlated with Result variable

# Plotting the relationships between important features and target variable


# Plot Result vs SSLfinal_State
ggplot(data_set,aes(x=SSLfinal_State,fill=Result)) + geom_bar(position = "dodge")

#Plot Result vs URL_of_Anchor
ggplot(data_set,aes(x=URL_of_Anchor,fill=Result)) + geom_bar(position = "stack")

#Plot Result vs web_traffic
ggplot(data_set,aes(x= web_traffic,fill=Result)) + geom_bar(position = "stack")

#Plot Result vs  having_Sub_Domain
ggplot(data_set,aes(x=having_Sub_Domain ,fill=Result)) + geom_bar(position = "stack")

#Plot Result vs Domain_registeration_length
ggplot(data_set,aes(x=Domain_registeration_length,fill=Result)) + geom_bar(position = "stack")

#Plot Result vs Request_UR
ggplot(data_set,aes(x=Request_URL,fill=Result)) + geom_bar(position = "dodge")

#Converting all fields to factor variables
data_set<-mutate_if(data_set,is.numeric,as.factor)

#split data into training and test data
set.seed(1234)
sample<-sample(c(TRUE,FALSE), nrow(data_set), replace = TRUE, prob =c(0.8,0.2))
train<-data_set[sample,]
test<-data_set[!sample,]
head(train)
head(test)

#Constructing a Decision Tree
data_set_DT<-rpart(Result~.,data=train,parms = list(split="information"),method = "class")
summary(data_set_DT)

#Plotting Decision Tree using rpart.plot()
rpart.plot::rpart.plot(data_set_DT,main="Decision Tree for Website Phishing")

#Feature evaluation of Decision Tree
dt_feature<-data.frame(imp=data_set_DT$variable.importance)
dt_feature1<-dt_feature %>%
  tibble::rownames_to_column() %>% 
  dplyr::rename("variable" = rowname) %>% 
  dplyr::arrange(imp) %>%
  dplyr::mutate(variable = forcats::fct_inorder(variable))

ggplot2::ggplot(dt_feature1) +
  geom_col(aes(x=variable,y=imp),
           col="black",show.legend=F) +
  coord_flip() +
  scale_fill_grey() +
  theme_bw()


#Predicting the model using train data
dt_predict_train<-predict(data_set_DT,train,type="class")
dt_traintable<-table(train$Result,dt_predict_train)
dt_traintable

#Predicting the model using test data
dt_predict_test<-predict(data_set_DT,test,type="class")
dt_test_table<-table(test$Result,dt_predict_test)
dt_test_table

#Confusion Matrix to calculate the performance of the Decision Tree
#Confusion Matrix for train data
confusionMatrix(dt_traintable,reference=train$Result)

#Confusion Matrix for the Test Data
confusionMatrix(dt_test_table,reference=test$Result)

#Cross Validation of Decision Tree on Test Data
Validation_DT<-data.frame(R2=R2(as.numeric(dt_predict_test),as.numeric(test$Result)),
                          RMSE=RMSE(as.numeric(dt_predict_test),as.numeric(test$Result)),
                          MAE=MAE(as.numeric(dt_predict_test),as.numeric(test$Result)))
Validation_DT


#Random Forest Model and Evaluation
#Set a Random Seed
set.seed(54)

#Training the model using the Random Forest
rf_model<- randomForest(formula = Result~.,data=train,ntree=1000,nodesize=10)
rf_model

#Predicting the Test set Results
rf_pred_test<-predict(rf_model, newdata = test)
rf_pred_test
rf_table<-table(test$Result,rf_pred_test)


#Confusion Matrix
confusionMatrix(rf_table,reference=test$Result)

plot(rf_model)

#Importance plot
importance(rf_model)

#Variable Importance Plot
varImpPlot(rf_model)

#Cross Validation
Validation_rf<-data.frame(R2=R2(as.numeric(rf_pred_test),as.numeric(test$Result)),
                          RMSE=RMSE(as.numeric(rf_pred_test),as.numeric(test$Result)),
                          MAE=MAE(as.numeric(rf_pred_test),as.numeric(test$Result)))
Validation_rf
