data1 <- data.frame(read.table("./00_raw_data/Data1.txt", header = T))  
data2 <- data.frame(read.table("./00_raw_data/Data2.txt", header = T))

# Use dplyr to merge data to keep positions of their ID
library(dplyr)

# Merge the data frames on the ID column
merged_data <- left_join(data1, data2, by = "id")
View(merged_data)

# Rename merged_data
project <- merged_data

# Check for duplicate
sum(duplicated(project))

# Check for Na's
colnames(project)[apply(project, 2, anyNA)]

# Replace Na's in all variables/columns with median

# For heart Rate
median(project$heartRate, na.rm = T)
project$heartRate[is.na(project$heartRate)] <- median(project$heartRate, na.rm = T)

# For test A
median(project$testA, na.rm = T)
project$testA[is.na(project$testA)] <- median(project$testA, na.rm = T)

# For test B
median(project$testB, na.rm = T)
project$testB[is.na(project$testB)] <- median(project$testB, na.rm = T)

# For test C
median(project$testC, na.rm =T)
project$testC[is.na(project$testC)] <- median(project$testC, na.rm = T)

# For test D
median(project$testD, na.rm = T)
project$testD[is.na(project$testD)] <- median(project$testD, na.rm = T)

# For test E
median(project$testE, na.rm = T)
project$testE[is.na(project$testE)] <- median(project$testE, na.rm = T)

# For disease
median(project$disease, na.rm = T)
project$disease[is.na(project$disease)] <- median(project$disease, na.rm = T)


# check for Na's 
colnames(project)[apply(project, 2, anyNA)]

# independent variable = age and dependent variable = disease

summary(project$age)
# The summary of the age variable gives insight to the distribution of ages in 
# dataset:
# The age ranges from 11 years to 76 years
# The average is 43 years
# Half of the participants are younger than 44 years (median) and half are older
# 25% of the participants are younger than 37 years (1st quartile).
# 25% of the participants are older than 49 years (3rd quartile).
# The mean value is less than the median, indicating the data is negatively skewed

# View the internal structure of the project
str(project)

# Convert the independent variable to a factor / levels  and view the structure
project[,'disease'] <- as.factor(project[,'disease'])

# Using box plot to visualize the data
library(ggplot2)
ggplot(project, aes(x=disease, y=age, fill=disease))+geom_boxplot()+
  labs(title = "Box Plot of age by disease", x = "Disease", y ="Age")

# Analysis from the Box Plot
# The Box plot compared the age for two groups: those without the disease(0) and 
# those with the disease(1)
# The median age for those with the disease appears to be slightly higher than
# for those without the disease
# The box for those without the disease is smaller indicating less variability in 
# age.
# The box for those with the disease is larger suggesting more age variability
# among those with the disease
# Both  groups have outliers, represented by individual points on beyond the whiskers
# The group with no disease has more visible outliers
# The age range for those without the disease seems to be more concentrated.
# Those with the disease show a wider spread of ages, with a higher median and 
# and more variability
# The group with the disease appears slightly skewed positively and the group
# without the disease is evenly distributed.
# In summary while there are some differences in age between those with and 
# without the disease or while age may play a role, the presence of outliers 
# suggest age alone may not be a definite predictor of disease status


# using histogram
ggplot(project, aes(x=age, fill=factor(disease))) +
  geom_histogram(position="identity", alpha=0.7, binwidth = 20) +
  facet_wrap(~disease) +
  labs(title = "Histogram of age by disease", x = "Age", y = "Count")

# Analysis from the graph
# The plot is divided into two panels, one for each disease(0 and 1)
# The x-axis represent the age, ranging from 0 to 100 years
# The y-axis shows the count of individuals in each age group
# # Disease = 0(left panel; pink)
# The absence of no disease distribution appears to be skewed
# Also there is a very high peak in the 25 - 50 age range. A sharp decline
# occurs after age 50. 
# Very few individuals are the oldest age group(50 - 75)
# # Disease = 1(right pink: blue)
# There's a noticeable peak in the 25-50 age range, but it's less pronounced
# than in the disease=0 group.
# There's a more gradual decline in the 50-75 age range.
# 
# In summary, those with the disease group has a more even distribution across 
# age groups. Those without the disease is heavily concentrated in the younger age
# range. Younger individuals (25-50) without the disease are over represented in the sample.
# The presence of the disease seems to be associated with a more even age distribution, 
# possibly suggesting it affects people across a broader age spectrum.
# The disease=0 group's sharp decline after 50 could indicate that older individuals without 
# the disease are underrepresented in the sample, or that the disease becomes 
# more common with age.
# There might be potential biases; The sample might not be representative of
# the general population, given the strong skew in the absence of disease  group.
# There could be age-related factors influencing disease diagnosis or study participation.
# This complex study relationship between age and disease warrant further
# investigation


# Scatter Plot

ggplot(project, aes(x = age, y = weight)) + 
  geom_point() + 
  labs(title = "Scatter plot of Weight vs Age", x = "Age", y = "Weight")

# Analysis from the scatter plot
# The age ranges from 15 to 75 years.
# Weight ranges from 125 to 225 kgs
# From the Plot there is a dense concentration of data points, indicating a large
# sample size
# The data forms a roughly oval shape widening as the age increases
# The appears to be a slightly positive correlation between age and weight.
# As the age increases it appears the upper limit tends to increase more 
# noticeably than the lower limit
# As evidenced by the widening spread of points, weight variability increases age
# There is noticeably a dense cluster of points in the middle age range (30 - 50years)
# A few individuals have weights above 200 kgs across different age groups.
# The increasing variability in weight with age suggests that factors influencing 
# weight become more diverse or impactful as people get older.




# Further Analysis using confirmatory data analysis
# logistic regression(lg)
# Ho = Age is not a factor to contracting the disease
# H1 = Age is a factor to contract the disease

lg <- glm(disease ~ age,  family = "binomial", data = project)
summary(lg)

# Based on the results, the p value:  < 2e-16 is less than the sign. level(0.05)
# Therefore we reject the null hypothesis and accept the H1, which confirms that
# age is a factor to contract the disease


# CHI-SQUARE TEST
# Ho = No Significant association between marital status and ethnic group
# H1 = There is significant association between marital status and ethnic group

chisq = chisq.test(project$ethnic, project$marital)
chisq

# Based on the analysis, the p value (0.2641) is greater than the sign. level(0.05)
# Therefore we accept the null hypothesis and reject the alternative hypothesis,
# which confirms that  there is no Significant association between marital 
# status and ethnic group


# Linear Regression
# Ho = Age does not contribute to height
# H1 = Age does contribute to height

lr <- lm(age ~ height, project)
summary(lr)
# Based on the analysis, the p value(0.7706) is greater than the sign. value(0.05)
# Therefore we accept the null hypothesis, therefore Age does not contribute to height.


# USING DECISION TREE (MACHINE LEARNING) 

# Check the structure of your data
str(project)

# levels
levels(project$disease) = c(0,1)

str(project)


# let n be the variable that holds the number of rows
n = nrow(project)
n
# 80% for training
trainIndex = sample(1:n, size = round(0.8*n), replace = FALSE)
train = project[trainIndex,]

# 20% for testing
test = project[-trainIndex,]

# set seed
set.seed(1)

# rpart
library(rpart)
library(rpart.plot)
mod <- rpart(disease~., train)
mod

# predict
rpart.plot(mod)
pred <- predict(mod, test, type = "vector")
pred

# actual
act <- test[,'disease']

# accuracy
table(pred, act)

# accuracy = ((1337 + 2080)/(1337 + 238 + 345 + 2080))
# accuracy = 85.43%

# The model seems to perform reasonably well overall, with an accuracy of 85.43%
# This confusion matrix provides valuable insights into the model's 
# performance and can help guide further refinement or decision-making based 
# on the model's predictions.


# clustering using K-MEANS

# set.seed(123)
# km <- kmeans(x = project[,-16], centers = 3)
# km$cluster
# km$centers


# write.csv(project, file = "patientdata.csv")
