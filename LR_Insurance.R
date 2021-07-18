#If God is for us, who can be against us?

library(psych)
library(ggplot2)
library(RColorBrewer)
library(car)
library(HH)

#Loading the data
ins_df = read.csv("C:/Users/tonyk/Documents/DSP 34/Task/Linear Regression/LinearRegression/insurance.csv")

#Checking the dimensions
dim(ins_df)
'1338    7'

#Names of the variables
colnames(ins_df)
'[1] "age"      "sex"      "bmi"      "children" "smoker"   "region"   "charges" '

#Checking the missing values
sapply(ins_df, function(x) sum(is.na(x))) 
'     age      sex      bmi children   smoker   region  charges 
       0        0        0        0        0        0        0 '
'No missing values'

#Target Variable
#Charges
str(ins_df$charges)
'num [1:1338] 16885 1726 4449 21984 3867 ...'

summary(ins_df$charges)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1122    4740    9382   13270   16640   63770 '

describe(ins_df$charges)
'   vars    n     mean       sd  median  trimmed     mad     min      max
X1    1 1338 13270.42 12110.01 9382.03 11076.02 7440.81 1121.87 63770.43
      range skew kurtosis     se
X1 62648.55 1.51     1.59 331.07'

#Histogram
hist(ins_df$charges,
     col = 'royalblue',
     xlab = 'Insurance Charges',
     main = 'Histogram of Insurance Charges')

#Box Plot
boxplot(ins_df$charges,
        horizontal = TRUE,
        col = 'royalblue',
        xlab = 'Insurance Charges',
        main = 'Boxplot of Insurance Charges')

#Outliers
#Getting the upper boundary
ch_ub = quantile(ins_df$charges, .75)+1.5*IQR(ins_df$charges)
print(ch_ub)
'     75% 
 34489.35'

#Checking the number of outliers
length(ins_df$charges[ins_df$charges > ch_ub])
'139'

for (i in seq(ch_ub,max(ins_df$charges),2000)){
  j = length(ins_df$charges[ins_df$charges > i])
  print(paste('No of outliers with ub as',round(i,0), 'is',j))
}

'
[1] "No of outliers with ub as 34489 is 139"
[1] "No of outliers with ub as 36489 is 118"
[1] "No of outliers with ub as 38489 is 98"
[1] "No of outliers with ub as 40489 is 74"
[1] "No of outliers with ub as 42489 is 57"
[1] "No of outliers with ub as 44489 is 41"
[1] "No of outliers with ub as 46489 is 29"
[1] "No of outliers with ub as 48489 is 15"
[1] "No of outliers with ub as 50489 is 7"
[1] "No of outliers with ub as 52489 is 6"
[1] "No of outliers with ub as 54489 is 5"
[1] "No of outliers with ub as 56489 is 4"
[1] "No of outliers with ub as 58489 is 4"
[1] "No of outliers with ub as 60489 is 2"
[1] "No of outliers with ub as 62489 is 2"'

#Age
str(ins_df$age)
'int [1:1338] 19 18 28 33 32 31 46 37 37 60 ...'

summary(ins_df$age)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  18.00   27.00   39.00   39.21   51.00   64.00 '

describe(ins_df$age)
'   vars    n  mean    sd median trimmed   mad min max range skew kurtosis
X1    1 1338 39.21 14.05     39   39.01 17.79  18  64    46 0.06    -1.25
     se
X1 0.38'

#Histogram
hist(ins_df$age,
     col = 'turquoise4',
     xlab = 'Age',
     main = 'Histogram of Age')

#Boxplot
boxplot(ins_df$age,
        horizontal = TRUE,
        col = 'turquoise4',
        xlab = 'Age',
        main = 'Boxplot of Age')

age_sq = ins_df$age^2

#Histogram
hist(age_sq,
     col = 'turquoise4',
     xlab = 'Age',
     main = 'Histogram of Age')

age_log = log(ins_df$age)
#Histogram
hist(age_log,
     col = 'turquoise4',
     xlab = 'Age',
     main = 'Histogram of Age')

'After applying square & log still the age distribution is not normal'

#Checking Linearity - Charges vs Age
ggplot(ins_df, aes(x=age, y=charges))+
  geom_point()

#Sex
str(ins_df$sex)
'Factor w/ 2 levels "female","male": 1 2 2 2 2 1 1 1 2 1 ...'

summary(ins_df$sex)
'female   male 
   662    676'

tab_sx = table(ins_df$sex)
print(tab_sx)
'female   male 
   662    676 '

#Barplot
barplot(tab_sx,
        col = c('chocolate1','cadetblue'),
        main = 'Barplot of Sex')

#Checking Linearity - Charges vs Sex
ggplot(ins_df, aes(x=sex, y=charges))+
  geom_point()
    
#bmi    
str(ins_df$bmi)
'num [1:1338] 27.9 33.8 33 22.7 28.9 ...'

summary(ins_df$bmi)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  15.96   26.30   30.40   30.66   34.69   53.13 '

describe(ins_df$bmi)
'   vars    n  mean  sd median trimmed mad   min   max range skew
 X1    1 1338 30.66 6.1   30.4    30.5 6.2 15.96 53.13 37.17 0.28
   kurtosis   se
X1    -0.06 0.17'

#Histogram
hist(ins_df$bmi,
     col = 'forestgreen',
     xlab = 'Body Mass Index',
     main = 'Histogram of BMI')
     
#Boxplot
boxplot(ins_df$bmi,
        horizontal = TRUE,
        col = 'forestgreen',
        xlab = 'Body Mass Index',
        main = 'Histogram of BMI')

#Outliers
#Getting the upper boundary
bmi_ub = quantile(ins_df$bmi, .75)+1.5*IQR(ins_df$bmi)
print(bmi_ub)
'  75% 
 47.29 '

#Checking the number of outliers
length(ins_df$bmi[ins_df$bmi > bmi_ub])
'9'

for (i in seq(bmi_ub,max(ins_df$bmi),2)){
  j = length(ins_df$bmi[ins_df$bmi > i])
  print(paste('No of outliers with ub as',round(i,0), 'is',j))
}
'
[1] "No of outliers with ub as 47 is 9"
[1] "No of outliers with ub as 49 is 3"
[1] "No of outliers with ub as 51 is 2"'

#Checking Linearity - Charges vs BMI
ggplot(ins_df, aes(x=bmi, y=charges))+
  geom_point()

#children 
str(ins_df$children)
'int [1:1338] 0 1 3 0 0 0 1 3 2 0 ...'

summary(ins_df$children)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   2.000   2.095   3.000   6.000 '

child_tab = table(ins_df$children)
print(child_tab)
'  0   1   2   3   4   5 
 574 324 240 157  25  18'

#Barplot
barplot(child_tab,
        col = brewer.pal(6, 'Dark2'),
        ylim = c(0,600),
        xlab = 'No of Children',
        main = 'Barplot of Children')

#Checking Linearity
ggplot(ins_df, aes(x=children, y=charges))+
  geom_point()

#smoker   
str(ins_df$smoker)
'Factor w/ 2 levels "no","yes": 2 1 1 1 1 1 1 1 1 1 ...'

smo_tab = table(ins_df$smoker)
print(smo_tab)
'  no  yes 
 1064  274'

#Barplot
barplot(smo_tab,
        col = c('limegreen', 'brown2'),
        main = 'Barplot of Smoker')

#Diving the data frame based on smoker and non smokers
sm_yes = ins_df[ins_df$smoker=='yes',]
sm_no = ins_df[ins_df$smoker=='no',]

par(mfrow=c(1,2))
#Histogram of insurance charges - smokers & non smokers
#Observe the insurance charges
hist(sm_yes$charges,
     col = 'red1',
     main = 'Insurance Charges of Smokers',
     xlab = 'Charges')
hist(sm_no$charges,
     col = 'royalblue2',
     main = 'Insurance Charges of Non - Smokers',
     xlab = 'Charges')

#Age vs charges - smokers and non smokers
plot(sm_yes$age, 
     sm_yes$charges,
     xlab = 'Age',
     ylab = 'Charges',
     main = 'Age vs Charges for Smokers')

plot(sm_no$age, 
     sm_no$charges,
     xlab = 'Age',
     ylab = 'Charges',
     main = 'Age vs Charges for Non-Smokers')

par(mfrow=c(1,1))

#Checking Linearity
ggplot(ins_df, aes(x=smoker, y=charges))+
  geom_point()

#region
str(ins_df$region)
'Factor w/ 4 levels "northeast","northwest",..: 4 3 3 2 2 3 3 2 1 2 ...'

reg_tab = table(ins_df$region)
print(reg_tab)
'northeast northwest southeast southwest 
      324       325       364       325 '

#Barplot
barplot(reg_tab,
        col = brewer.pal(4,'Set1'),
        main = 'Barplot of Region')

#Checking Linearity
ggplot(ins_df, aes(x=region, y=charges))+
  geom_point()

#Getting the correlation
cor(ins_df[c('age', 'bmi', 'children', 'charges')])

#Pairs Panels
pairs.panels(ins_df[c('age', 'bmi', 'children', 'charges')])

#Building the model
model = lm(charges~., ins_df)
summary(model)
'Multiple R-squared:  0.7509,	Adjusted R-squared:  0.7494 
F-statistic: 500.8 on 8 and 1329 DF,  p-value: < 2.2e-16'

#Residual vs filtted & Normal Q-Q plot
plot(model)

#Prediction
model_pred = predict(model, type = 'response')
head(model_pred)

RMSE(model_pred, ins_df$charges)
'[1] 6041.68'

names(model)

#Checking the normality of error
error = model$residuals
hist(error,
     col = brewer.pal(10, 'Set2'))

#Constant error variance
plot(model_pred, error)

#Independence of observation
plot(model_pred)

#Multicolinearity
vif(model)

#Auto Correlation
durbinWatsonTest(model)
'P value is greater than 0.05, therefore fail to reject Null Hypothesis
H0 - There is no correlation among the residuals'