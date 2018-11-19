rm(list=ls(all=T))
#change working directory
setwd("C:/Users/parul/Desktop/Data Science/PROJECT/project2")
library(readxl)
#load dataset
dataset=readxl::read_excel("Absenteeism_at_work_Project.xls",col_names = TRUE, na=c(""," "))
dataset=as.data.frame(dataset)

#**********************************FEATURE ENGINEERING**************************************
#create new column year
dataset$year=NA
dataset$year[1:113]=2007
dataset$year[114:358]=2008
dataset$year[359:570]=2009
dataset$year[571:740]=2010

#reorder variables
dataset=dataset[,c(1:20,22,21)]
#remove last 3 observations of dataset as they dont signify anything
dataset=dataset[1:737,]

#Convert to factor data type
#categorical variables
cat_var=c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week','Seasons','Disciplinary.failure','Education','Social.drinker','Social.smoker','Son','Pet')

for(i in cat_var)
{
  dataset[,i]=as.factor(dataset[,i])
}


#***********************************MISSING VALUE ANALYSIS******************************************


#create dataframe with missing percentage
missing_val = data.frame(apply(dataset,2,function(x){sum(is.na(x))}))
#convert row names into columns
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL
#Rename the variable conating missing values
names(missing_val)[1] =  "Missing_percentage"
#calculate missing percentage
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(dataset)) * 100
#arrange in descending order
missing_val = missing_val[order(-missing_val$Missing_percentage),]
#rearranging the columns
#exchanging positions of column 2 and column 1
missing_val = missing_val[,c(2,1)]

#We see that the missing data percentage is very low, so we impute the missing values

dataset1=subset(dataset,year==2007)
dataset2=subset(dataset,year==2008)
dataset3=subset(dataset,year==2009)
dataset4=subset(dataset,year==2010)

library(grid)
library(lattice)
library(DMwR)
library(bnstruct)

#correcting a missing value
dataset[67,3]=10

#impute missing values using mean for numeric data and mode for categorical data
#however apply these functions after grouping the whole data using years. 
#This will help to generate more authentic results.
num_var=c('Body.mass.index','Absenteeism.time.in.hours','Height','Work.load.Average.per.day','Transportation.expense','Hit.target','Distance.from.Residence.to.Work','Service.time','Age','Weight')
for(i in num_var)
{
  dataset1[,i][is.na(dataset1[,i])]=median(dataset1[,i],na.rm = T)
  dataset2[,i][is.na(dataset2[,i])]=median(dataset2[,i],na.rm = T)
  dataset3[,i][is.na(dataset3[,i])]=median(dataset3[,i],na.rm = T)
  dataset4[,i][is.na(dataset4[,i])]=median(dataset4[,i],na.rm = T)
}

Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}


for(i in cat_var)
{
  dataset1[[i]][is.na(dataset1[[i]])]=Mode(dataset1[[i]])
  dataset2[[i]][is.na(dataset2[[i]])]=Mode(dataset2[[i]])
  dataset3[[i]][is.na(dataset3[[i]])]=Mode(dataset3[[i]])
  dataset4[[i]][is.na(dataset4[[i]])]=Mode(dataset4[[i]])
}


dataset=rbind(dataset1,dataset2,dataset3,dataset4)





##Data Manupulation; convert string categories into factor numeric
for(i in 1:ncol(dataset)){
  
  if(class(dataset[,i]) == 'factor'){
    
    dataset[,i] = factor(dataset[,i], labels=(1:length(levels(factor(dataset[,i])))))
    
  }
}

#******************************OUTLIER ANALYSIS*******************************************
#**********univariate
numeric_index = sapply(dataset,is.numeric) #selecting only numeric
numeric_data = dataset[,numeric_index]
cnames = colnames(numeric_data)
#cnames=cnames[1:11]

library(ggplot2)
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(dataset))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Absenteeism time in hours")+
           ggtitle(paste("Box plot for",cnames[i])))
}

#visualize outliers
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,gn7,gn8,gn9,gn10,gn11,ncol=4)


# #imputing outliers by using flooring and capping method
#  for(i in cnames)
#  {
#    x <- dataset[,i]
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
#   caps <- quantile(x, probs=c(.05, .95), na.rm = T)
#   H <- 1.5 * IQR(x, na.rm = T)
#   x[x < (qnt[1] - H)] <- caps[1]
#   x[x > (qnt[2] + H)] <- caps[2]
#  }


#Treat outliers as missing values and then impute them using KNN
for(i in cnames){
  val = dataset[,i][dataset[,i] %in% boxplot.stats(dataset[,i])$out]
  #print(length(val))
  dataset[,i][dataset[,i] %in% val] = NA
}

#check the number of outliers in each variable
for(i in cnames){
print(sum(is.na(dataset[[i]])))
}

#impute outliers using KNN
dataset_temp=dataset
dataset = knnImputation(dataset, k = 3)

write.csv(dataset, 'cleandata.csv', row.names = TRUE)




#**********************************************FEATURE SELECTION**********************************
library(car)
library(carData)
library(stats)
library(pedometrics)
library(caret)
library(corrgram)
library(GGally)

#corrgram(dataset, order = F,upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#visual plot of correlation matrix
ggcorr(dataset[cnames],label=TRUE,label_alpha = TRUE)

#here we see that there are no highly correlated numeric independent variables

#Now we use ANOVA to check dependency between target variable and independent categorical variables
ccnames=cnames[-11]
summary(aov(formula = Absenteeism.time.in.hours~ID,data = dataset))
summary(aov(formula = Absenteeism.time.in.hours~Reason.for.absence,data = dataset))
summary(aov(formula = Absenteeism.time.in.hours~Month.of.absence,data = dataset))
summary(aov(formula = Absenteeism.time.in.hours~Day.of.the.week,data = dataset))
summary(aov(formula = Absenteeism.time.in.hours~Seasons,data = dataset))
summary(aov(formula = Absenteeism.time.in.hours~Disciplinary.failure,data = dataset))
summary(aov(formula = Absenteeism.time.in.hours~Education,data = dataset))
summary(aov(formula = Absenteeism.time.in.hours~Social.drinker,data = dataset))
summary(aov(formula = Absenteeism.time.in.hours~Social.smoker,data = dataset))
summary(aov(formula = Absenteeism.time.in.hours~Son,data = dataset))
summary(aov(formula = Absenteeism.time.in.hours~Pet,data = dataset))

#the results of anova show that target variable depends on these independent categorical variables

## Dimension Reduction
dataset= subset(dataset, select = -c(Body.mass.index))

#******************DATA VISUALIZATION*********************
#*********************Univariate Data analysis******************************

library(purrr)
library(tidyr)

#Density plots of numeric variables
dataset %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_density()


ggplot(dataset,aes(factor(Reason.for.absence),fill=factor(Reason.for.absence))) +geom_bar()+labs(y="count",x="Reason for absence") + theme_classic()+ggtitle("Count of Reason for absence")
ggplot(dataset,aes(factor(ID),fill=factor(ID))) +geom_bar()+labs(y="count",x="ID") + theme_classic()+ggtitle("Count for ID")
ggplot(dataset,aes(factor(Month.of.absence),fill=factor(Month.of.absence))) +geom_bar()+labs(y="count",x="Month of absence") + theme_classic()+ggtitle("Count for Month of absence")

ggplot(dataset,aes(factor(Day.of.the.week),fill=factor(Day.of.the.week))) +geom_bar()+labs(y="count",x="Day of the week") + theme_classic()+ggtitle("Count for Day of the week")
ggplot(dataset,aes(factor(Seasons))) +geom_bar()+labs(y="count",x="Seasons") + theme_classic()+ggtitle("Count for Seasons")
ggplot(dataset,aes(factor(Disciplinary.failure),fill=factor(Disciplinary.failure))) +geom_bar()+labs(y="count",x="Disciplinary.failure") + theme_classic()+ggtitle("Count for Disciplinary failure")
ggplot(dataset,aes(factor(Education),fill=factor(Education))) +geom_bar()+labs(y="count",x="Education") + theme_classic()+ggtitle("Count for Education")
#here, class 1=no , class2 =yes
ggplot(dataset,aes(factor(Social.drinker),fill=factor(Social.drinker))) +geom_bar()+labs(y="count",x="Social.drinker") + theme_classic()+ggtitle("Count for Social.drinker")

ggplot(dataset,aes(factor(Social.smoker),fill=factor(Social.smoker))) +geom_bar()+labs(y="count",x="Social.smoker") + theme_classic()+ggtitle("Count for Social.smoker")
ggplot(dataset,aes(factor(Son),fill=factor(Son))) +geom_bar()+labs(y="count",x="Son") + theme_classic()+ggtitle("Count for Son")
ggplot(dataset,aes(factor(Pet),fill=factor(Pet))) +geom_bar()+labs(y="count",x="Pet") + theme_classic()+ggtitle("Count for Pet")


#*********************Bivariate Data analysis*******************************
boxplot(Absenteeism.time.in.hours~Reason.for.absence, data=dataset, main="Absenteeism in hours vs Reason.for.absence")
boxplot(Absenteeism.time.in.hours~Month.of.absence, data=dataset, main="Absenteeism in hours vs month")
boxplot(Absenteeism.time.in.hours~Disciplinary.failure, data=dataset, main="Absenteeism in hours vs Disciplinary.failure")
boxplot(Absenteeism.time.in.hours~Day.of.the.week, data=dataset, main="Absenteeism in hours vs Day.of.the.week")
boxplot(Absenteeism.time.in.hours~Seasons, data=dataset, main="Absenteeism in hours vs Seasons")
boxplot(Absenteeism.time.in.hours~Education, data=dataset, main="Absenteeism in hours vs Education")
boxplot(Absenteeism.time.in.hours~Social.drinker, data=dataset, main="Absenteeism in hours vs Social.drinker")
boxplot(Absenteeism.time.in.hours~Social.smoker, data=dataset, main="Absenteeism in hours vs Social.smoker")
library(plyr)
library(dplyr)

#summarize/aggregate absenteeism hours w.r.t categorical variables for proper analysis
hours_vs_reason=dataset%>%
  group_by(Reason.for.absence)%>%
  summarise(sum=sum(Absenteeism.time.in.hours))
hours_vs_reason=as.data.frame(hours_vs_reason)
plot(hours_vs_reason)

hours_vs_year=dataset%>%
  group_by(year)%>%
  summarise(sum=sum(Absenteeism.time.in.hours))
hours_vs_year=as.data.frame(hours_vs_year)
plot(hours_vs_year)

hours_vs_month=dataset%>%
  group_by(Month.of.absence)%>%
  summarise(sum=sum(Absenteeism.time.in.hours))
hours_vs_month=as.data.frame(hours_vs_month)
plot(hours_vs_month)

hours_vs_drinker=dataset%>%
  group_by(Social.drinker)%>%
  summarise(sum=sum(Absenteeism.time.in.hours))
hours_vs_drinker=as.data.frame(hours_vs_drinker)
plot(hours_vs_drinker)

hours_vs_smoker=dataset%>%
  group_by(Social.smoker)%>%
  summarise(sum=sum(Absenteeism.time.in.hours))
hours_vs_smoker=as.data.frame(hours_vs_smoker)
plot(hours_vs_smoker)

hours_vs_education=dataset%>%
  group_by(Education)%>%
  summarise(sum=sum(Absenteeism.time.in.hours))
hours_vs_education=as.data.frame(hours_vs_education)
plot(hours_vs_education)
#class 1= no smoke class2=smoke

hours_vs_season=dataset%>%
  group_by(Seasons)%>%
  summarise(sum=sum(Absenteeism.time.in.hours))
hours_vs_season=as.data.frame(hours_vs_season)
plot(hours_vs_season)


hours_vs_ID=dataset%>%
  group_by(ID)%>%
  summarise(sum=sum(Absenteeism.time.in.hours))
hours_vs_ID=as.data.frame(hours_vs_ID)
plot(hours_vs_ID)

hours_vs_Son=dataset%>%
  group_by(Son)%>%
  summarise(sum=sum(Absenteeism.time.in.hours))
hours_vs_Son=as.data.frame(hours_vs_Son)
plot(hours_vs_Son)


hours_vs_Pet=dataset%>%
  group_by(Pet)%>%
  summarise(sum=sum(Absenteeism.time.in.hours))
hours_vs_Pet=as.data.frame(hours_vs_Pet)
plot(hours_vs_Pet)

#***********************Finding Important features using Random Forest algorithm
#based on mean drop in impurity
library(randomForest)
fit_rf=randomForest(Absenteeism.time.in.hours~.,data=dataset)
importance(fit_rf,type=2)
varImpPlot(fit_rf)


#********************************TIME SERIES MODELING AND FORECASTING****************************************
#aggreagte absenteeism hours w.r.t year and months
hours_vs_year_month=dataset%>%
  group_by(year,Month.of.absence)%>%
  summarise(sum=sum(Absenteeism.time.in.hours))
hours_vs_year_month=as.data.frame(hours_vs_year_month)
#rename last variable indicating sum of absenteeism hours
names(hours_vs_year_month)[names(hours_vs_year_month)=="sum"]="absent_hours"
#create a time series by assigning the time to the absent_hours variable
#each data point is mapped to a month of the year in the series from july 2007 to july 2010
dataset_ts=ts(hours_vs_year_month$absent_hours,frequency = 12,start=c(2007,7),end=c(2010,7))
plot.ts(dataset_ts)
#check different components from the plot like trend,seasonal,cyclic, irregularities
#we see an irregular spike in 2008 and seasonal components
#transformation in case of seasonal and random fluctuations
#test=log(dataset_ts)
#plot(test)
library(TTR)
#smmothing time series to check the trend component
#a) simple moving average over different periods to find the hidden trend ignoring the seasonality
dataset_ts_sma3=SMA(dataset_ts,3)
plot(dataset_ts_sma3)
dataset_ts_sma5=SMA(dataset_ts,5)
plot(dataset_ts_sma5)
dataset_ts_sma7=SMA(dataset_ts,7)
plot(dataset_ts_sma7)
dataset_ts_sma8=SMA(dataset_ts,8)
plot(dataset_ts_sma8)
dataset_ts_sma9=SMA(dataset_ts,9)
plot(dataset_ts_sma9)
dataset_ts_sma10=SMA(dataset_ts,10)
plot(dataset_ts_sma10)
#b) exponential moving average (weighted moving average) to find the missing trend ignoring the seasonality
dataset_ts_ema3_25=EMA(dataset_ts,3,ratio = .25)
plot(dataset_ts_ema3_25)
dataset_ts_ema3_5=EMA(dataset_ts,3,ratio = .5)
plot(dataset_ts_ema3_5)
dataset_ts_ema3_75=EMA(dataset_ts,3,ratio = .75)
plot(dataset_ts_ema3_75)
#c) stl method to decompose the seasonal data into trend + seasonality + random fluctuations
#plot(decompose(dataset_ts))
library(forecast)
library(SWMPr)
library(wql)
library(tseries) #for stationarity
library(lmtest)
#model is additive as the seasonality peaks remain constant over time
dataset_ts_stl=stl(dataset_ts,s.window="periodic")
dataset_ts_seasonality=(dataset_ts_stl$time.series[,1])
dataset_ts_trend=(dataset_ts_stl$time.series[,2])
dataset_ts_remainder=(dataset_ts_stl$time.series[,3])
plot(dataset_ts_stl)
#remove random fluctuations from time series
dataset_ts_wo_random=(dataset_ts)-(dataset_ts_remainder)
plot.ts(dataset_ts_wo_random)


#Diving into train and test; 80:20 split
train = window(dataset_ts, start = c(2007,7), end = c(2009,12))
test = window(dataset_ts, start = c(2010))
#************************FORECASTING*********************************
#a) Using Holtwinter exponential smoothning (as time series has both trend and seasonality)
dataset_ts_hw<- HoltWinters(train)
summary(dataset_ts_hw)
#Smoothing parameters:
#alpha: 0.9218336
#beta : 1
#gamma: 1
#alpha,beta,gamma values show that all the weight is assigned to the most recent value to calculate seasonality and trend for future year
plot(dataset_ts_hw)
#dataset_ts_hw shows the in-sample forecast where black line is the original time series
#and red line shows forecasted values
#now we perfom the out of sample forecast for the year 2011
dataset_ts_hw_2011 <- forecast:::forecast.HoltWinters(dataset_ts_hw, h=30)
forecast:::plot.forecast(dataset_ts_hw_2011)
#MODEL VALIDATION


#forecast error=observed-forecasted value for data points covered by original time series
#in-sample forecast errors are stored in the named element "residuals" of the list variable returned by forecast.HoltWinters()
#For a good model, there should be no correlations between forecast errors for successive predictions.

acf(na.exclude(dataset_ts_hw_2011$residuals),lag.max=20)
#no prominent corelation between forecast error
#there should be constant variance of forecast errors and mean should be near 0
#forecast errors should be normally distributed with mean 0 and constant variance
plot.ts(dataset_ts_hw_2011$residuals)
hist(dataset_ts_hw_2011$residuals)
#OR plotting histogram with overlaid normal curve
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(na.exclude(dataset_ts_hw_2011$residuals))
accuracy(dataset_ts_hw_2011,test)
checkresiduals(dataset_ts_hw_2011)

#b) using ARIMA model
#check data stationarity using Augmented Dickey Fuller Test
tseries::adf.test(dataset_ts, alternative="stationary", k=0)
#H0:ts not stationary
#p-value=0.01<alpha value, reject H0.
#Hence our time series is STATIONARY
#acf-auto correlation function
#linear correlation between lagged values
#pacf
#correlation when the linearity is removed. It considers the lagged difference and the value of interval between them

acf(dataset_ts,lag.max=20)
pacf(dataset_ts,lag.max=20)
#dataset_ts_arima=auto.arima(dataset_ts)
dataset_ts_arima=auto.arima(train)
#forecast absenteeism for 2011
dataset_ts_arima_2011=forecast(dataset_ts_arima,h=20)
forecast:::plot.forecast(dataset_ts_arima_2011)
plotForecastErrors(na.exclude(dataset_ts_arima_2011$residuals))
accuracy(dataset_ts_arima_2011,test)
checkresiduals(dataset_ts_arima_2011)
