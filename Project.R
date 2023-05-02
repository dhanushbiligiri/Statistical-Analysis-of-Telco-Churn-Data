#install.packages('ggplot2')
library(ggplot2)
#install.packages('gridExtra')
library(gridExtra)
#install.packages('dplyr')
library(dplyr)
#install.packages('forcats')
library(forcats)
#install.packages("GGally")
library(GGally)
#install.packages('nortest')
library(nortest)
library(PMCMRplus)


#Read the data
data <- read.csv('/Users/rachanatanneeru/Documents/Statistical Methods/Telco-Customer-Churn.csv',stringsAsFactors = TRUE)
n <- nrow(data)
data
names(data)
str(data)
#Exploratory data analysis
dim(data)
#7043*23


is.null(data$TotalCharges)
head(data,n=10)
sort(data$tenure)
data[rowSums(is.na(data))>0,]
data <- na.omit(data) #Removing null values
dim(data)
#7032*21


dim(distinct(data))#checking for duplicate rows
glimpse(data)
summary(data)

#EDA
dim(data %>% filter(data$Churn == 'Yes', data$Contract=='Month-to-month'))
#Churn, yes to using PaperlessBilling , and on a month-to-month contract
# The output shows 1655 out of the 1869 Churn observations matched these restrictions so roughly 88.5% of the Churn observations matched

dim(data %>% filter( data$Contract=='Month-to-month'))
## we see 3875 of the total sample has Paperlessbilling and Month-to-month 
## Thats 3875-1655= 2220 observations that are not what we want


#numerical variables, grouped by gender
data %>% group_by(gender) %>% summarize ("Number of observations"=n(),"Average Tenure in Months"=round(mean(tenure),0),"Monthly Charges"=round(mean(MonthlyCharges),2),"Average Total Charges"=round(mean(TotalCharges),2))
#From the summary above we can see that there are slightly more male than female (1.9%). Both female and male have average tenure of around 32 months, with male having 1 more months in average tenure (33).Female has slightly higher (1.3%) monthly charges than male. Both male and female have around $2283 average total charges

#Histograms for numerical variables





g1 <- data %>%
  mutate(Churn = as.factor(Churn)) %>%
  ggplot(aes(x = Churn, y = tenure, fill = Churn)) +
  geom_bar(stat = "summary", fun = "mean", alpha = 0.6, show.legend = FALSE) +
  {stat_summary(aes(label = paste0(round(..y.., 0), " months"), x = Churn, y = tenure),
                fun = mean, geom = "text", size = 3.5, vjust = -0.5)} +
  labs(title = "Average Tenure")

g2 <- data %>%
  mutate(Churn = as.factor(Churn)) %>%
  ggplot(aes(x = Churn, y = MonthlyCharges, fill = Churn)) +
  geom_bar(stat = "summary", fun = "mean", alpha = 0.6, show.legend = FALSE) +
  {stat_summary(aes(label = paste0(round(..y.., 0), " months"), x = Churn, y = MonthlyCharges),
                fun = mean, geom = "text", size = 3.5, vjust = -0.5)} +
  labs(title = "Average Monthly Charges")

gridExtra::grid.arrange(g1, g2, ncol = 2)





#From the histograms above, we can see that customers Churn are having fewer months of tenure and higher average monthly charges.


data %>%
  mutate(Churn = as.factor(Churn)) %>%
  # Group by Contract and Churn, calculate count of each group
  count(Contract, Churn) %>%
  ggplot(aes(x = Contract, y = n, fill = fct_rev(Churn))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  labs(title = "Customer Churn by Contract Type", y = "Count of Contract Type") -> g3

data %>%
  mutate(Churn = as.factor(Churn)) %>%
  # Group by PaymentMethod and Churn, calculate count of each group
  count(PaymentMethod, Churn) %>%
  ggplot(aes(x = PaymentMethod, y = n, fill = fct_rev(Churn))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  labs(title = "Customer Churn by Payment Method", y = "Count of Payment Method") -> g4

data %>%
  mutate(Churn = as.factor(Churn)) %>%
  # Group by InternetService and Churn, calculate count of each group
  count(InternetService, Churn) %>%
  ggplot(aes(x = InternetService, y = n, fill = fct_rev(Churn))) +
  geom_bar(stat = "identity", alpha = 0.6) +
  labs(title = "Customer Churn by Internet Service", y = "Count of Internet Service") -> g5

grid.arrange(g3, g4, g5)

#From the histograms we can see that more customers pay by month-to-month using electronic checks, and these type of customers have the largest percentage of customers Churned. Customer under one-year or two-year contracts are much less likely to Churn. Customers using automatic payment methods (bank transfer and credit card), as well as mailed check are less likely to Churn. Customers usiong Fiber Optic are most likely to Churn.

data %>%
  ggplot(aes(x=ifelse(SeniorCitizen==1, "Senior", "Not Senior"), fill=fct_rev(Churn))) + 
  geom_bar(alpha=0.6) + 
  labs(title="Customer Churn on Senior Citizens", y="Count of Senior Citizen") 

data %>%
  ggplot(aes(x=gender, fill=fct_rev(Churn))) + 
  geom_bar(alpha=0.6) + 
  labs(title="Customer Churn on Gender", y="Count of Gender") 

data %>%
  ggplot(aes(x=Partner, fill=fct_rev(Churn))) + 
  geom_bar(alpha=0.6) + 
  labs(title="Customer Churn on Partner", y="Count of Partner") 

data %>%
  ggplot(aes(x=Dependents, fill=fct_rev(Churn))) + 
  geom_bar(alpha=0.6) + 
  labs(title="Customer Churn on Dependents", y="Count of Dependents") 

grid.arrange(grobs = list(g1, g2, g3, g4), ncol = 2)


#corelation plot
data %>% dplyr::select(tenure, MonthlyCharges, TotalCharges, Churn) %>% ggpairs(aes(color=fct_rev(Churn)),diag = list(continuous = wrap("densityDiag", alpha = 0.6), discrete = wrap("barDiag", alpha = 0.7, color="grey30")))


#Oneway ANOVA

#Monthly charges based on Internet services
glimpse(data)
#Parallel boxplots of your response variable split by the levels of the independent variable
boxplot(data$MonthlyCharges~data$InternetService,main='Internet Service V/s Monthly Charges',ylab='Monthly Charges',xlab = 'Internet Service')

#table which includes the sample size, sample mean, sample median, and sample standard deviation of your response variable for each of the k samples defined by your independent variable
X <- table(data$InternetService)
X
# DSL     Fiber optic     No 
#2416        3096        1520

xbar <- tapply(data$MonthlyCharges, data$InternetService, mean)
xbar

#DSL        Fiber optic    No 
#58.08802    91.50013    21.07628 

med <- tapply(data$MonthlyCharges, data$InternetService, median)
med
# DSL     Fiber optic      No 
#56.150      91.675      20.150

std <- tapply(data$MonthlyCharges, data$InternetService, sd)
std

#   DSL      Fiber optic    No 
#16.266167   12.663039    2.161599 

#ANOVA table 
model <- aov(MonthlyCharges ~ InternetService, data)
print(model)
#modelling <-MonthlyCharges ~ InternetService, data
anova(model)

#Discussion on the validity of the assumptions of the one-way ANOVA 
coefficients(model)
confint(model)
sigma(model) # point estimate for sigma
data$fitted1 <- fitted.values(model)
data$resid1 <- residuals(model)
names(data)
View(data)
#Assumptions
#normal q-q plot of the residuals
qqnorm(data$resid1)
qqline(data$resid1,col='red',lwd =2)
#It is normally distributed

#p-value from the Shapiro-Wilk test applied to the residuals
shapiro.test(data$resid1[1:5000])
#We cannot use this as the dataset contains more than 7000 records
#But when performed on the first 5000 records the p value obtained is very low(p-value < 2.2e-16) 

ad.test(data$resid1)
#plot of the residuals against the fitted values
plot(model)

#p-value of Levene’s test applied to the squared residuals
resid <- residuals(model)
DescTools::LeveneTest(MonthlyCharges ~ InternetService, data)
anova(lm(abs(resid^2) ~ InternetService, data))
#p-value is very less < 2.2e-16. The assumption of homoscedesticity is violated


#Welch's ANOVA
oneway.test(MonthlyCharges ~ InternetService, data)
#There exist differences in the mean of the MonthlyCharges  between the different InternetServices (p-value < 2.2e-16)
pairwise.t.test(data$MonthlyCharges, data$InternetService, pool.sd = FALSE)

gm <- gamesHowellTest(MonthlyCharges ~ InternetService,data)
summaryGroup(gm)
#Fiber optic has significantly greater mean of monthlycharges than both DSL and No internet services

#For us to perform One way anova, We need to meet certain assumptions which are - 
#• IID: we have k random samples collected from their respective populations
#• Independent Samples : the k samples are collected independently of one another
#• Normal Pop. : the k populations are all normally distributed
#• Homogeneity of Var. : the k populations possess the property of homoscedasticity (they all have the same population variance)

#From the qqplot we conclude that the datapoints are normal. But from the levenes test we observe that the p-value is lesser than the significant value(0.05). We conclude that
#the data violates the assumption of homoscedasticity. This is the reason we select Welch's ANOVA as the assumption of the test matches with the assumptions of our data

#conclusions you may make based off of the Welch’s ANOVA
#We select the significance level of 0.05
#We conclude that 'Fiber optic' has significantly greater mean of monthlycharges than both 'DSL' and 'No' internet services


#posthoc comparisons
pairwise.t.test(data$MonthlyCharges, data$InternetService, pool.sd = FALSE)


#One way ANova 2

#Monthly charges based on Contract
glimpse(data)
##Parallel boxplots of your response variable split by the levels of the independent variable
boxplot(data$MonthlyCharges~data$Contract,main='Contract vs Monthly Charges',ylab='Monthly Charges',xlab = 'Contract')

#table which includes the sample size, sample mean, sample median, and sample standard deviation of your response variable for each of the k samples defined by your independent variable
Y <- table(data$Contract)
Y
#Month-to-month       One year       Two year 
#3875                  1473           1695

xbar1 <- tapply(data$MonthlyCharges, data$Contract, mean)
xbar1

#Month-to-month       One year       Two year 
#66.39849             65.04861       60.77041 

med1 <- tapply(data$MonthlyCharges, data$Contract, median)
med1
#Month-to-month       One year       Two year 
#73.25                 68.75          64.35 

std1 <- tapply(data$MonthlyCharges, data$Contract, sd)
std1

#Month-to-month       One year       Two year 
#26.92660             31.84054       34.67887 

#ANOVA table 
model2 <- aov(MonthlyCharges ~ Contract, data)
print(model2)
anova(model2)

#Assumptions
coefficients(model2)
confint(model2)
sigma(model2) # point estimate for sigma
data$fitted2 <- fitted.values(model2)
data$resid2 <- residuals(model2)
names(data)
View(data)

#a normal q-q plot of the residuals
qqnorm(data$resid2)
qqline(data$resid2,col='red',lwd =2)
#It is not normally distributed

#Shapiro wilk test
shapiro.test(data$resid2[1:5000])
#We cannot use this as the dataset contains more than 7000 records
#But when performed on the first 5000 records the p value obtained is very low(p-value < 2.2e-16) 
ad.test(data$resid1)
#Plot of residuals vs fitted
plot(model2)

#Levenes test
resid2 <- residuals(model2)
DescTools::LeveneTest(MonthlyCharges ~ Contract, data)
anova(lm(abs(resid2^2) ~ Contract, data))
#p-value is very less < 2.2e-16. The assumption of homoscedesticity is violated


#Kruskal-Wallis test
kruskal.test(MonthlyCharges ~ Contract, data)
#p-value= 2.459e-05 which means we can conclude that there is a statistically significant difference in the median MonthlyCharges among the Contract groups.
#The four three different contract types does do not have the same distribution of monthlycharges
pairwise.wilcox.test(data$MonthlyCharges, data$Contract)
dunn.test::dunn.test(data$MonthlyCharges, data$Contract)
pair <- kwAllPairsDunnTest(MonthlyCharges ~ Contract, data = data)
summary(pair)
summaryGroup(pair)

#Not normal from QQ plot, Homoscedesticity fail from Lavene 

#conclusions you may make based off of the Kruskal wallis ANOVA

#We select the significance level of 0.05
#we can conclude that there is a statistically significant difference in the median MonthlyCharges among the Contract groups.The four three different contract types does do not have the same distribution of monthlycharges


#posthoc comparisons
dunn.test::dunn.test(data$MonthlyCharges, data$Contract,confidence_interval)
pair <- kwAllPairsDunnTest(MonthlyCharges ~ Contract, data = data)
summary(pair)
summaryGroup(pair)


