

case<-read.csv("Case2.csv") #change as needed

#AverageSalary and FTRententionRate relationship

relationship_1<-lm(case$FTRetentionRate~case$AverageSalary,data=case)
plot(case$FTRetentionRate~case$AverageSalary)
abline(relationship_1,col="Blue")
summary(relationship_1)
mean(relationship_1$residuals)
hist(relationship_1$residuals)
par(mfrow=c(2,2))
plot(relationship_1)
lmtest::bptest(relationship_1)
#It is a positive relationship because beta coefficient is 2.774e-04 which is a positive number.
#With Multiple R-squared=0.2602 and Adjusted R-squared=0.2597, the relationship between AverageSalary and FTRetentionRate is weak.
#1)Linearity: In AverageSalary and FTRetentionRate plot, it is not perfect linear because some points are faraway.In the QQ plot,the residuals deviate from the diagonal line in both the upper and lower tail.
#2)Normality of errors: from the hist plot and mean value(1.492752e-15),it is left skew but not serious departure
#3)Independence: of errors:In the residuals versus fitted plot, the points concentrate as fitted values increase, so it appears that there is a relationship.
#4)Homoskedasicity--From residuals and fitted plot,the distribution of the residuals is quite well concentrated around 0 for small fitted values, but the data is funnel shape.
#also, p-value is very close to 0,so violates assumption of homoskedasicity. We found heteroskedasicity
#5)This relationship suggests that better paid teachers increases the full time retention rate of students. However, as a weak correlation,
#the teacher's salary is only a small part of what drives the retention rate. Without further research, we can propose that better paid
#teachers are better or happier, which increases the quality of the student's experience, leading them to stay.

relationship_2<-lm(case$FTRetentionRate~case$S2FRatio,data=case)
plot(case$FTRetentionRate~case$S2FRatio)
abline(relationship_2,col="Blue")     
summary(relationship_2)
mean(relationship_2$residuals)
hist(relationship_2$residuals)
par(mfrow=c(2,2))
plot(relationship_2)
lmtest::bptest(relationship_2)

#There was a negative relationship between S2FRatio and FtRetentionRate, shown by beta coefficient -0.30678. 
#The relationship between S2FRatio and FtRetentionRate was also shown to be statistically significant with Multiple R-squared = 0.0156 and Adjusted R-squared = 0.01492
#1)Linearity: In the S2FRetention and FtRentionRate plot, linearity is not perfect because points are faraway north and south of the upper portion of the abline
#2)Normality of errors: from observing the hist plot and calculating the a mean value of -3.712153e-15 it can be seen that the residuals are skewed left.
#3)Independence of errors: In the residuals vs. fitted plot, as the fitted values increase the points do not concentrate indicating a weak relationship. 
#4)Homoskedasicity: From the residuals and fitted plot we can see that the distribution of the residuals is not concentrated around 0 for small fitted values and the p-value is not close to 0, this led us to the conclusion of homoskedasicity
#5)Statisical insignificance suggests that there is no meaningful relationship between Student to Faculty ratio and full time student retention rates.

summary(case)
relationship_3<-lm(case$FTRetentionRate~case$TuitionAndFees,data=case)
plot(case$FTRetentionRate~case$TuitionAndFees)
abline(relationship_3,col="Blue")
summary(relationship_3)
par(mfrow=c(2,2))
plot(relationship_3)
mean(relationship_3$residuals)
hist(relationship_3$residuals)
lmtest::bptest(relationship_3)


#beta coefficient is 2.329e-04, meaning the relationship between retention rate and tution and fees is positive
#The relationship between TuitionAndFees and FtRetentionRate was also shown to be very weak with an R-Squared of 0.096 and Adjusted R-squared of 0.9546
#There is an extremely slight left skew but otherwise evenly distrubuted
#The histogram shows a normal bell curve, so there is normality of errors
#Based on the bptest, we found heteroskedasticity and violated the assumptions of homoskedasticity
#Tution can be a factor behind dropping out of college, so studying the relationship between retention rates and tuition is important.
#Of course, there are other factors to be explored, such as financial aid, but this comparision is the most basic behind financial commitments
#The weak positive correlation suggests that higher tuition can lead to higher retention rates, perhaps because people who are attending more
#expensive schools are more motivated to stay in school.
#This data column is also missing about a quarter of the total readings, which is capable of throwing off the data and results