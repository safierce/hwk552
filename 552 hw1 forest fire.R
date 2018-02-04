forest <- read.csv("/Users/sashawang/Downloads/forestfires.csv", stringsAsFactors = FALSE)
forest2= as.data.frame(forest)
forest2
summary(forest)
dim(forest)
head(forest)
colnames(forest)
# (b) Exploring the data:
# i. How many rows are in this data set? How many columns? What do the rows and columns represent?
#  thre are 517 rows and 13 columns
# each row is an information of fires with burned area.
# columns represents information about the fire. for example, X and Y
#are axis spatial corrdinate within the park map, month and date of the fire,
#FFMC, DMC, DC idnexs, temperature, relative humidity, wind speed, rain,
#and the burned area of the forrest.

#ii. Explain why the transformation area1 = ln(1 + Y ), where Y is the response/dependent variable is useful for this dataset. 
#In the following, use area1 as the new response variable.
# Y has many zero in the records. It has large standard deviation and is higly skewed, 
# It is hard to interpret any information from the distribution of Y
# thre fore wee need to be transform Y in a format that 
#so needs to be transformed to log. and avoid the situation of log(0), we change to log(Y+1)
sd(forest$area)
median(forest$area)
subset(forest, area==0)
quantile(forest$area, c(0.1, 0.25, 0.5, 0.75, 0.9))
plot(density(forest$area))
forest$area1=log((1+forest$area))

colnames(forest)
plot(density(forest$area1))

forestfires=forest [, c(-3,-4,-13)] # delete original area
colnames(forestfires) 

#iii. Make pairwise scatterplots of the predictors (columns) in this data set with the dependent variable. Describe your findings.
forest_num= forestfires[,-c(3,4)] # exclude categorical variables

# change month to numeric
#match(forestfires,month.abb)

colnames(forest_num)
pairs(forest_num)

#iv. Make at least 16 pairwise scatterplots of predictors of your choice and describe your findings. You are welcome to make all possible scatter plots.
forest_num2=forest_num[,c(6,7,8,9)]
pairs(forest_num2)
#v. What are the mean, the median, range, first and third quartiles, and in- terquartile ranges of each of the variables in the dataset? Summarize them in a table.
summary(forestfires)


summary(lm(area1 ~ X, data = forestfires))
# if p < 0,05, has 1 star, significant
# R-squred: 0.3% variance in response can be interpreted by the model
summary(lm(area1 ~ Y, data = forestfires))
summary(lm(area1 ~ FFMC, data = forestfires))
summary(lm(area1 ~ DMC, data = forestfires))
summary(lm(area1 ~ DC, data = forestfires))
summary(lm(area1 ~ ISI, data = forestfires))
summary(lm(area1 ~ temp, data = forestfires))
summary(lm(area1 ~ RH, data = forestfires))
summary(lm(area1 ~ wind, data = forestfires))
summary(lm(area1 ~ rain, data = forestfires))

plot(area1 ~ . -area1, data = forest_num)


'The Month variable is the only statically signficant result between the predictor and the response 
. The rest of variables have no statistically significant result with response
When looking at the response
variables and crime in simple scatter plots, one can see how a general linear 
regression with these variables would allow for a better prediction of crime than 
simply using the mean of crime. That is, the data seems to have some slight shape 
sloping up or down, and is not a random cloud of data. That being said, while almost
every variable is statistically significant, R-squared is very low, and so these 
predictors only describe a small amount of the variation in the response'

#(d)
mlfire=lm(area1 ~ . - area1, data = forestfires)
summary(mlfire)


 
'wind is found to be statistically signficant with the response.For other variables, we 
fail to reject the null hypothesis. R-squared is also the highest compared to single 
linear regression of each variable. more variance in predictor can be explained using
this model.'
'(e) How do your results from 2c compare to your results from 2d? Create a plot displaying 
the univariate regression coefficients from 2c on the x-axis, and the multiple regression
coefficients from 2d on the y-axis. That is, each predictor is displayed as a single point
in the plot. Its coefficient in a simple linear regression model is shown on the x-axis,
and its coefficient estimate in the multiple linear regression model is shown on the y-axis.'

#coefficient in a simple linear regression:
unico <- lm(area1 ~ X, data = forestfires)$coefficients[2]
unico <- append(unico, lm(area1 ~ Y, data = forestfires)$coefficients[2])
unico <- append(unico, lm(area1 ~ FFMC, data = forestfires)$coefficients[2])
unico <- append(unico, lm(area1 ~ DMC, data = forestfires)$coefficients[2])
unico <- append(unico, lm(area1 ~ DC, data = forestfires)$coefficients[2])
unico <- append(unico, lm(area1 ~ ISI, data = forestfires)$coefficients[2])
unico <- append(unico, lm(area1 ~ temp, data = forestfires)$coefficients[2])
unico <- append(unico, lm(area1 ~ RH, data = forestfires)$coefficients[2])
unico <- append(unico, lm(area1 ~ wind, data = forestfires)$coefficients[2])
unico <- append(unico, lm(area1 ~ rain, data = forestfires)$coefficients[2])
unico
# coefficient in the multiple linear regression:
mlco <- mlfire$coefficients[-1]

#plot
plot(unico, mlco, xlab = "single", ylab = "multiple")

'(f) Is there evidence of nonlinear association between any of the predictors and the response? 
To answer this question, for each predictor X, fit a model of the form'
summary(lm(area1 ~ X + I(X^2) + I(X^3),  data = forestfires))
summary(lm(area1 ~ Y + I(Y^2) + I(Y^3), data = forestfires))
#summary(lm(area1 ~ month + I(month^2) + I(month^3), data = forestfires))
#summary(lm(area1 ~ day + I(day^2) + I(day^3), data = forestfires))
summary(lm(area1 ~ FFMC + I(FFMC^2) + I(FFMC^3), data = forestfires))
summary(lm(area1 ~ DMC + I(DMC^2) + I(DMC^3), data = forestfires))
summary(lm(area1 ~ DC + I(DC^2) + I(DC^3), data = forestfires))
summary(lm(area1 ~ ISI + I(ISI^2) + I(ISI^3), data = forestfires))
summary(lm(area1 ~ temp + I(temp^2) + I(temp^3), data = forestfires)) # I(temp^2) sig
summary(lm(area1 ~ RH + I(RH^2) + I(RH^3), data = forestfires))
summary(lm(area1 ~ wind + I(wind^2) + I(wind^3), data = forestfires)) #I(wind^3) sig
summary(lm(area1 ~ rain + I(rain^2) + I(rain^3), data = forestfires)) 
summary(lm(area1 ~ indus + I(indus^2) + I(indus^3), data = forestfires))




'according to previous pairwise scatterplots, we can observe that there are interaction between DMC and DC
DC and num_month, temp and RH'
colnames(forestfires)
vars=colnames(forestfires[, c(-13,-3,-4)])
vars
for (i in vars)  {
  for (j in vars) {
    if (i != j) {
      factor= paste(i,j,sep='*')}
lm.fit <- lm(area1 ~paste(i,j,sep='*'), data=forestfires)
    print(summary(lm.fit))
  }}

i=1
j=2
paste(i,j,sep = '*')

summary(lm(area1 ~ DMC + DC + DMC:DC , data = forestfires))
summary(lm(area1 ~ DC + num_month + DC:num_month, data = forestfires)) #R**2=0.19
summary(lm(area1 ~ temp + RH + temp:RH, data = forestfires))
summary(lm(area1 ~ Y + DC + DC:Y, data = forestfires))
summary(lm(area1 ~ temp + DMC + DMC:temp, data = forestfires)) # sig
summary(lm(area1 ~ temp + wind + wind:temp, data = forestfires)) #sig


#STATISTICALLY SIGNIFICANT: summary(lm(area1 ~ temp + DMC + DMC:temp, data = forestfires))
summary(lm(area1 ~ temp + DMC + DMC:temp, data = forestfires))
summary(lm(area1 ~ temp + wind + wind:temp, data = forestfires))
colnames(forestfires)
'However, the statistics do not show any significance of the interaction'

'h) Can you improve your model using possible interaction terms or nonlinear associ- ations and between
the predictors and response? Train the model on a randomly selected 70% subset of the data and test it
on the remaining points and report your train and test results.'
# split data into training and testing
set.seed(1)
train.ind <- sample(1:dim(forestfires)[1], 0.7 * dim(forestfires)[1])
train <- forestfires[train.ind, ]
test <- forestfires[-train.ind, ]
train.def <- forestfires$area1[train.ind]
test.def <- forestfires$area1[-train.ind]
'select features'
mod1= lm(area1 ~  I(wind^3)+ DMC +wind +temp , data=forestfires)
summary(mod1)  #0.0499
# residual:
# coefficient: a random variable
# residual stad error
#R ^2 225% variance can be interprested by the model
colnames(forestfires)
plot(mod1$fit, mod1$res, xlab = 'Fitted', ylab = 'residual')
summary(train$area1)
train.sub<- train[, c('num_month' + 'temp' + 'RH')]
# scale to get same magnitute of coefficient

install.packages("ISLR")
library(ISLR)
install.packages("caret")
library(caret)


#first 4 predictors
colnames(forestfires)
train4=train[, c(1,2,3,4,11)]
train4
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(area1 ~., data = train4, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

knnFit
plot(knnFit)

#last 4 predictors
trainlast4=train[, c(7,8,9,10,11)]
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit2 <- train(area1 ~., data = trainlast4, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

knnFit2
plot(knnFit2)

#1,2,9,10,11
trainr=train[, c(1,2,9,10,11)]

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit3 <- train(area1 ~., data = trainr, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

knnFit3
plot(knnFit3)
