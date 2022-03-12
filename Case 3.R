
#Part 1
covid<-read.csv("Case4.csv",stringsAsFactors = TRUE)
str(covid)
class(covid$intubated)
shortlogmode<-glm(intubated~age+Gender,family=binomial,data=covid)
summary(shortlogmode)
exp(shortlogmode$coefficients)

#The coefficients for this logistic regression signify changes for the variables age and gender. The coefficient for age is 0.0143261,
#meaning that with every 1 unit increase in age (i.e. 1 year older), the log odds of being intubated increases by 0.0143261. Gender is
#specified to be applicable to women, meaning that if a person is female, their log odds of being intubated changes by -0.3001395. From these
#coefficients, we can see that younger people clearly are less likely to be intubated, which is consistent with general knowledge and findings
#of covid. In terms of gender, women are less likely to be intubated than men. It can be concluded from these coefficients that younger women
#are the least likely to be intubated.


#Part 2
library(MASS)
library(dbplyr)
library(caret)
attach(covid)
set.seed(100)
dividedata<-createDataPartition(intubated,p=.1,list=FALSE)
train<-covid[dividedata,]
test<-covid[-dividedata,]

preprocessing<-train %>% preProcess(method=c("center","scale"))
traintransformed<-preprocessing %>% predict(train)
testtransformed<-preprocessing %>% predict(test)


##Logistic Regression
logisticmodel<-glm(intubated~.,family=binomial,data=train)
probs<-predict(logisticmodel,test,type="response")
pred<-ifelse(probs>.5,"Yes","No")
table(pred,test$intubated)
mean(pred!=test$intubated) ## error rate is 0.1820128
mean(pred==test$intubated)##accuracy rate is 0.8176371
#True Positives=46374, True Negatives=22, False Positives=10301, False Negatives=47. 
(sensitivity=46374/(46374 +47))#0.9989875
(specificity=22/(10301+22))#0.002131163

#In this logistic regression model using all predictor variables, the accuracy rate is 0.8176371, or about 81.76%. The true positives are equal to
#46374, meaning that the model predicted that 46374 people are intubated that actually are. The true negatives are equal to 22, meaning that the model
#suggested that 22 people are not intubated that actually are not. The false positives are quite high at 10301, meaning that the model suggested
#this number of people are intubated when they actually do not. The false negatives are equal to 47, which means that the model suggested that 47 people are
#are not intubated when they actually are. The sensitivity is high at 0.9989875, meaning that the model did a good job at predicting cases, but the specificity
#is simultaneously very low, which is because this model was not very accurate at predicting negatives, which can be seen through the very high
#number of false positives that should ideally be true negatives.


##LDA
ldamodel<-lda(intubated~.,data=traintransformed)
predictions<-ldamodel %>% predict(testtransformed)
names(predictions)
mean(predictions$class==testtransformed$intubated) #accuracy rate is 0.816615 
table(predictions$class,testtransformed$intubated)
#True Positives=46243, True Negatives=95, False Positives=10228, False Negatives=178 
(sensitivity=46243/(46243+178))#0.9961655
(specificity=95/(10228+95))#0.009202751

#In this LDA model, the accuracy rate is 0.816615. In terms of the confusion matrix, the number of true positives is quite high at 46243, meaning that
#the model predicted that 46243 people are intubated that actually are. The true negatives are equal to 95, meaning that the model predicted that
#95 people are not intubated that are not. The number of false positives is extremely high, which could be a contributing factor to the
#comparatively low number of true negatives. The false positives are equal to 10228, meaning that the model predicted there was this number of people who are intubated
#that actually are. The sensitivity, which is the ability of the model to predict the number of positives out of the actual number of positives, is
#relatively high at 0.9961655. Likewise, the specificity is low at 0.009202751, meaning that the model is much less able to predict negative results
#that are actually negative. Again, this is clear in the very high number of false positives.


##QDA
qdamodel<-qda(intubated~.,data=traintransformed) 
predictions<-qdamodel %>% predict(testtransformed)
names(predictions)
mean(predictions$class==testtransformed$intubated) 
table(predictions$class,testtransformed$intubated)

#The QDA model does not run. This indicates that there is a rank deficiency, meaning that some variables are collinear and at least one covariance
#matrix cannot be inverted. Given that the covariance matrices of the QDA model are what mainly separate it from the LDA model, this is an important
#discrepancy in being unable to run the mode. In terms of analysis, this means that we are unable to analyze QDA as a part of our final analysis.
#QDA models provide a level of flexibility that is in between LDA and KNN with more flexibility in terms of variables, so being unable to incorporate
#this model leaves us with a linear model and a non-parametric model that have restrictions that may not perform as well on such a large data set.


##KNN
knnmodel<-train(intubated~.,data=train,method="knn",preProcess=c("center","scale"))
plot(knnmodel)
knnmodel$bestTune
knnclass<-predict(knnmodel,newdata=test)
head(knnclass)
table(knnclass,test$intubated)
mean(knnclass==test$intubated)##accuracy rate is 0.802358
confusionMatrix(knnclass,test$intubated)
#summary
#True Positives=44912, True Negatives=617, False Positives=9706, False Negatives=1509    
(sensitivity=44912/(44912 +1509 ))#0.9674932
(specificity=617/(9706+617))#0.05976945

#The KNN model is the most flexible model, since they are simple and lack many rigid parametric assumptions. However, the accuracy rate in this case
#is the lowest out of the models listed at 0.802358. The number of true positives is 44912, meaning that the model predicted that 44912 people
#are intubated who actually are. The true negatives are the highest out of the models listed here at 617, meaning that the model predicted that 617
#people are not intubated who are not. The false positives are equal to 9706, meaning that the model suggested that 9706 people
#are intubated but they actually are not. The false negatives is also the highest of the above models at 1509, meaning that the model suggested that
#1509 people are not intubated when they actually are. The sensitivity is relatively low at 0.9674932, which suggests that out of 100 actually positive cases
#the model predicted 96.75% of them correctly. Similarly, the specificity is slightly higher than other models at 0.05976945. This is because
#the true negatives are higher and the false positives lower, so the KNN model was slightly better at predicted people who are not intubated that
#actually are not.



#Overall, we believe the the logistic regression model is the best because it has the highest accuracy rate at 0.8176371. Though it had a very
#high number of false positives, it also had the highest number of true positives. When discussing a potentially deadly illness, it is arguably better
#to more accurately predict the number of people that actually have the illness and help people that may not need it as opposed to being inaccurate
#and not assisting people that really need it. The shape of the data in this case will be linear, meaning there is a linear relationship between
#people having covid and whether or not they are intubated, among other factors. 

#It is also worth noting that the KNN model was able to best predict true negatives and had significantly fewer false positives. However, as
#previously stated, in this situation it is likely better to correctly predict actual cases rather than negative cases. Also, while the LDA model
#is extremely close to the logistic model, there is a collinear variable warning when it is run, meaning we are unable to use it as part of our
#final determination.


