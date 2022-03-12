
#Read in data, perform summary functions
Sales<-read.csv("Case3.csv", stringsAsFactors = TRUE)
Sales1<-Sales[,-c(2,5:8,28:29,31:65)] #Includes all continuous variables to choose most significant ones
lmSales1<-lm(SalePrice~.,data=Sales1)
str(Sales1)
summary(lmSales1) #Checks significance
cor(Sales1) #Checks significance and eliminates multicolinearity

#Create linear model
Sales2<-Sales[,c(1,4,5,12,13)] #Includes significant continuous variables
lmSales2<-lm(SalePrice~.,data=Sales2)
str(Sales2)
summary(lmSales2) #Shows heteroskedasticity
cor(Sales2)
plot(lmSales2)
mean(lmSales2$residuals)
hist(lmSales2$residuals) #Left skew
car::vif(lmSales2)
lmtest::bptest(lmSales2)
#1)Linearity: In 1st plot, it is not perfectly linear because some points are clear outliers. In the QQ plot,the residuals deviate from the diagonal line in both the upper and lower tail.
#2)Normality of errors: from the hist plot and mean value, the data is left skewed
#3)No multicollinearity: no value above 5, so the data already does not have any highly correlated independent variables.
#4)Heteroskedasticity: The initial linear model is heteroskedastic because the p value is below 0.05

#Delete strong outliers---explain QQ plot and cook's distance in the last plot
cooksD <- cooks.distance(lmSales2) #Use cook's distance to filter out values greater than 3x the mean
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential

Sales3<-Sales2[-c(258,360,430,568,635,663,740,956,967,1062,582,562,439),]
lmSales3<-lm(SalePrice~.,data=Sales3)
str(Sales3)
summary(lmSales3)
hist(lmSales3$residuals) #Right skew
#These outliers were chosen because they clearly stand out when the data set was previously plotted. They specifically caused the
#QQ plot to bend upwards away from the line. By eliminating them, we are able to focus the data analysis on data that does not alter the plots.
#Removing outliers also increases R squared, which shows the variance because they can skew R squared to fit the given data, even if that makes it inaccurate to the general pattern found.


#Transform x and y
lmSalesloglog<-lm(log(SalePrice)~log(LotArea)+log(OverallQual)+TotalBsmtSF+X1stFlrSF,data=Sales3)
hist(lmSalesloglog$residuals) 
#We chose to use the loglog transformation to lessen heteroskedasticity because there were strong outliers and the P value was still notably heteroskedastic.
#This resulted in a much higher R squared value and homoskedasticity, in addition to a histogram that has normal distribution.

###final regression equation
#predicted log(SalesPrice) is (-1.232e+05)+(2.549e+00)*log(LotArea)+(3.503e+04)*log(OverallQual)+(2.377e+01)*TotalBsmtSF+(3.342e+01)*X1stFlrSF         

#Cor since log(y)model used
b0<-lmSalesloglog$coefficients[1]
b1<-lmSalesloglog$coefficients[2]
b2<-lmSalesloglog$coefficients[3]
b3<-lmSalesloglog$coefficients[4]
b4<-lmSalesloglog$coefficients[5]
sig<-sigma(lmSalesloglog)
yhat<-exp(b0+b1*log(Sales3$LotArea)+b2*log(Sales3$OverallQual)+b3*Sales3$TotalBsmtSF+b4*Sales3$X1stFlrSF+sig^2/2)
cor(yhat,log(Sales3$SalePrice))^2
#We are unable to use adjusted R squared due to the double log transformation. The correlation is 0.7679188 as seen from the cor function above.


#Summary
summary(lmSalesloglog)
lmtest::bptest(lmSalesloglog) #Homoskedastic with p value = 0.07207 > 0.05
plot(lmSalesloglog) #Follows linearity
car::vif(lmSalesloglog) #No multilinearity

#The correlation explains 76.79% of the variance here, since its value is 0.7679188. It is also very clear in the QQ plot that the points mainly follow the diagonal line.
#Overall, the model did relatively well in explaining various assumptions. It explains almost 77% of variance in the model,
#shows homoskedasticity, and no multilinearity, meaning all the variables are individually significant and not highly related to each other.
#The standard error on the final summary is also very low for each value, with the R squared and adjusted R squared values being extremely close as well.
#Therefore, our assumptions were not violated using this final loglog adjusted model.


#The information provided in the final regression equation is useful in the general housing market population, which most people participate
#in. The final few variables we used clearly correlate to the price of a house just by using knowledge of how houses sell.
#This sort of equation might be useful to a realtor or somebody looking to sell their house but are unsure where to start. These factors
#could change over time as what people consider important in houses changes.
