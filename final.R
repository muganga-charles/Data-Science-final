# importing packages and libraries
library(readxl)
library(tidyverse)
library(dplyr)

work <- read_excel(("D:/R/Assignment2/Assignment2_dataset.xlsx"))
View(work)
# Question one(1)
#transform the dataset to excude missing values
new_work <- na.omit(work)
View(new_work)
# Question two(2)
#calculate the perception of current diamond prices
prices <- work$price
#prices <- as.numeric(prices)
prices <- sum(prices)
prices
prices <- prices/53930
prices
# Question three(3)
##calculate the perception of current diamond prices vs the quality of the diamond
quality <- work$cut
#without repeating the qualities and their occurence
quality <- unique(quality)
quality
#calculate the occurence of each quality
quality_occurence <- table(work$cut)
quality_occurence
#graph the occurence of each quality
barplot(quality_occurence, main = "Diamond Quality", xlab = "Quality", ylab = "Occurence", col = "green")
#calculate the total occurence of the qualities
quality_occurence_total <- sum(quality_occurence)
quality_occurence
#calculate the percentage of each quality
quality_percentage <- (quality_occurence/quality_occurence_total)*100
quality_percentage
#calculate average price of each quality
quality_price <- aggregate(work$price, by=list(work$cut), FUN=mean)
quality_price
#graph the average price of each quality
barplot(quality_price$x, main = "Diamond Quality", xlab = "Quality", ylab = "Price", col = "blue")

# Question four(4)
#defining the general boxplot of the data
boxplot(work$price, work$carat, work$depth, work$x, work$y, names=c("price", "carat", "depth", "x", "y"), horizontal=TRUE, col="red", main="Boxplot of the variables of the dataset", xlab="Values", ylab="Variables")

###
# Question five(5)
#identify the outliers
#selecting the boxplot of the price to examine the outliers
boxplot(work$price)
#2.Using the IQR
#calculate the IQR
######Specificaly for price.####
IQRPrice <- IQR(work$price)
IQRPrice
#calculate the quartiles
quartilesPrice <- quantile(work$price, probs=c(.25, .75), na.rm = FALSE)
quartilesPrice
#calculate the upper and lower limits
LowerPrice <- quartilesPrice[1] - 1.5*IQRPrice
UpperPrice <- quartilesPrice[2] + 1.5*IQRPrice
LowerPrice
UpperPrice
#filter the outliers
no_outliersPrice <- subset(work, work$price > UpperPrice | work$price < LowerPrice)
no_outliersPrice
#box plot of the data without outliers
boxplot(no_outliersPrice$price)

######Specificaly for carat.####
IQRcarat <- IQR(work$carat)
IQRcarat
#calculate the quartiles
quartilescarat <- quantile(work$carat, probs=c(.25, .75), na.rm = FALSE)
quartilescarat
#calculate the upper and lower limits
Lowercarat <- quartilescarat[1] - 1.5*IQRcarat
Uppercarat <- quartilescarat[2] + 1.5*IQRcarat
Lowercarat
Uppercarat
#filter the outliers
no_outlierscarat <- subset(work, work$carat > Uppercarat | work$carat < Lowercarat)
no_outlierscarat
#box plot of the data without outliers
boxplot(no_outlierscarat$carat)

######Specificaly for x.####
IQRx <- IQR(work$x)
IQRx
#calculate the quartiles
quartilesx <- quantile(work$x, probs=c(.25, .75), na.rm = FALSE)
quartilesx
#calculate the upper and lower limits
Lowerx <- quartilesx[1] - 1.5*IQRx
Upperx <- quartilesx[2] + 1.5*IQRx
Lowerx
Upperx
#filter the outliers
no_outliersx <- subset(work, work$x > Upperx | work$x < Lowerx)
no_outliersx
#box plot of the data without outliers
boxplot(no_outliersx$x)
##### Specificaly for y.####
IQRy <- IQR(work$y)
IQRy
#calculate the quartiles
quartilesy <- quantile(work$y, probs=c(.25, .75), na.rm = FALSE)
quartilesy
#calculate the upper and lower limits
Lowery <- quartilesy[1] - 1.5*IQRy
Uppery <- quartilesy[2] + 1.5*IQRy
Lowery
Uppery
#filter the outliers
no_outliersy <- subset(work, work$y > Uppery | work$y < Lowery)
no_outliersy
#box plot of the data without outliers
boxplot(no_outliersy$y)
##### Specificaly for depth.####
IQRdepth <- IQR(work$depth)
IQRdepth
#calculate the quartiles
quartilesdepth <- quantile(work$depth, probs=c(.25, .75), na.rm = FALSE)
quartilesdepth
#calculate the upper and lower limits
Lowerdepth <- quartilesdepth[1] - 1.5*IQRdepth
Upperdepth <- quartilesdepth[2] + 1.5*IQRdepth
Lowerdepth
Upperdepth
#filter the outliers
no_outliersdepth <- subset(work, work$depth > Upperdepth | work$depth < Lowerdepth)
no_outliersdepth
#box plot of the data without outliers
boxplot(no_outliersdepth$depth)

# Question six(6)
#create a new dataset with the outliers and missing data removed
#save the new dataset as a csv file
# removing the outliers from the whole dataset for the quantitative variables
new_work <- work[work$price < UpperPrice & work$price > LowerPrice,]
new_work <- work[work$carat < Uppercarat & work$carat > Lowercarat,]
new_work <- work[work$x < Upperx & work$x > Lowerx,]
new_work <- work[work$y < Uppery & work$y > Lowery,]
new_work <- work[work$depth < Upperdepth & work$depth > Lowerdepth,]

#removing the missing values from the whole dataset was done in the first part of the assignment
#saving the new dataset as a csv file

write.csv(new_work, file="D:/R/Assignment2/Assignment2_MugangaCharles.csv")

# Question seven(7)
#7. Display the relationship between one qualitative variable and one finite variable in the dataset. (Define 

#display the relationship between the cut and the price using a bar chart 

library(ggplot2)
ggplot(new_work, aes(x=cut, y=price)) + geom_bar(stat="identity", fill="blue") + labs(title="Relationship between the cut and the price", x="Cut", y="Price")

#display the relationship between the cut and the price

# Question eight(8)
#Show statistical tests to assess for normal distribution of all the variables (carat to PC) in the new_work dataset.
#using the Kolmogorov-Smirnov test

carat <- new_work$carat
new_carat <- carat[!duplicated(carat)]

# shapiro.test(new_carat)
ks.test(new_carat, "pnorm", mean(new_carat), sd(new_carat))
#plot a histogram of the data
hist(new_carat)

#comment on the results
#he p-value is 0.4397 which is greater than 0.05 and therefore we fail to reject the null hypothesis

depth <- new_work$depth
new_depth <- depth[!duplicated(depth)]
ks.test(new_depth, "pnorm", mean(new_depth), sd(new_depth))
#plot a histogram of the data with the density curve

hist(new_depth)
#comment on the results
#The p-value is 0.9558 greater than 0.05and therefore we fail to reject the null hypothesis

###calculating the p-value for the price variable
price <- new_work$price
new_price <- price[!duplicated(price)]
ks.test(new_price, "pnorm", mean(new_price), sd(new_price))
#plot a histogram of the data
hist(new_price)
#comment on the results
#the value of the p-value is 2.2e-16 which is less than 0.05 and therefore the null hypothesis is rejected

###calculating the p-value for the x variable
x <- new_work$x
new_x <- x[!duplicated(x)]
ks.test(new_x, "pnorm", mean(new_x), sd(new_x))
#plot a histogram of the data
hist(new_x)
#comment on results
#the p-value is less than 0.05 and the test statistic is greater than the critical value
#comment on the results
#the value of the p-value is 2.2e-16 which is less than 0.05 and therefore the null hypothesis is rejected


###calculating the p-value for the y variable
y <- new_work$y
new_y <- y[!duplicated(y)]
ks.test(new_y, "pnorm", mean(new_y), sd(new_y))
#comment on the results
#the value of the p-value is 2.2e-16 which is less than 0.05 and therefore the null hypothesis is rejected


#the defined data can be tested for normality.

#Question nine(9)
# Compute the variance between three groups; diamond carat, perception change and price
#view the groups
new_work <- na.omit(new_work)
groups <- new_work[,c("carat", "PC", "price")] 
groups
#generate ramdom sample of the data
sample <- sample_n(groups, 90)
sample
#the factors are the groups
#show the levels
#use factor function to define the groups
x <- factor(sample$PC)
x
levels(x)
#show the summary of the data by the groups
x <- ordered(x, levels=c("Negative","Positive","Positve","NR","SN","SP"))
library(dplyr)
group_by(groups, PC) %>% 
  summarise(
    mean=mean(price), 
    sd=sd(price), n=n())

#plot the data
library(ggpubr)
#plot the price by the Pc and the carat by the PC
#ggboxplot(groups, x = "PC", y = "price", color = "PC", palette = "jco",add="jitter",order = c("carat", "PC", "price"), xlab = "Perception Change", ylab = "Price", title = "Price by the groups") 

#plot as a line graph
ggline(groups, x = "PC", y = "price", 
       add = c("mean_se"),
       order = c("Negative","Positive","Positve","NR","SN","SP"),
       ylab = "Price", xlab="Perception Change", 
       title = "Line plot of the groups")

#plot a boxplot
#ggboxplot(groups, x = "PC", y = "price", color = "PC", palette = "jco",add="jitter",order = c("Negative","Positive","Positve","NR","SN","SP"), xlab = "Perception Change", ylab = "Price", title = "Price by the groups")


#plot as a line grap
ggline(groups, x = "PC", y = "carat", 
       add = c("mean_se"),
       order = c("Negative","Positive","Positve","NR","SN","SP"),
       ylab = "carat", xlab="Perception Change", 
       title = "Line plot of the groups")
#compute the anova to anwser the question
anova <- aov(price ~ PC, data=groups)
summary(anova)
#Comment on the results
