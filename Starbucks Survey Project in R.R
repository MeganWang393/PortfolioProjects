##################################################
#Starbucks Customer Survey - Descriptive Analysis#
##################################################

#getwd()
#setwd()

#Load Packages
library(Hmisc)
#library(ggplot2)
library(dplyr)
library(ltm)

# Load data
mydata <- read.csv("Starbucks satisfactory survey.csv")

# Investigate data
class(mydata)
head(mydata)
str(mydata) 
summary(mydata)
colnames(mydata)

# Rename columns of mydata 
colnames(mydata)[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)] <- c(
  "gender_q", "age_q", "status_q", "income_q", "visitNo_q", "method_q", "timeSpend_q", 
  "location_q", "membershipCard_q", "itemPurchase_q", "spendPurchase_q", "productRate_q", 
  "priceRate_q", "promoRate_q",     "ambianceRate_q", "wifiRate_q", "serviceRate_q",
  "chooseRate_q", "promoMethod_q","loyal_q")

# loyal-q: "Will you continue buying at Starbucks?"

# check if there are any duplicates
mydata <- unique(mydata)
mydata["Id"] <- row.names(mydata)
#length(unique(mydata$id)) == nrow(mydata)

# filter out records of people never visted starbucks
mydata <- subset(mydata, !mydata["visitNo_q"]=="Never")

# check if there are any missing values
any(is.na(mydata))

#########################
# check data reliablity #
#########################

#Cronbach’s Alpha helps us to measure the internal consistency of a group of data. 
#It is a coefficient of reliability. It helps us to validate the consistency of a 
#questionnaire or survey. The Cronbach’s Alpha ranges between 0 and 1. 
#The higher value for Cronbach’s Alpha means more reliable the group of data is.
#<0.5: Unacceptable; 0.5-0.6: poor; 0.6-0.7: questionable.

cronbach.alpha(mydata, CI=TRUE)

########################
# Descriptive Analysis #
########################

## descriptive analysis from demographic, geographic, behavior, service rating, product, promotion etc.

# Define function for pie chart 
pcnt_pie <- function(v2, v3, v4, v6, v1=mydata, v5="bottomright")
  {pcnt1<- round(prop.table(table(v1[[v2]])), 4) *100
  pie(pcnt1, labels = paste0(pcnt1, "%"), main=v6)
  legend(v5, legend = v3, cex=0.7, fill =  v4)}

# Percentage of gender
pcnt_pie("gender_q", c("Female", "Male"), c("white", "lightblue"), "Pie chart of Gender") 

# Percentage of loyalty
pcnt_pie("loyal_q", c("No", "Yes"), c("white", "lightblue"), "Pie chart of Loyalty") 

# Define function for bar chart 

myfreq_count <- function(b, c, d, e, f, g, a=mydata) {
  myfreq <- table(a[[b]], a[[c]])
  legend("topright", legend=f,  fill=g, 
         ncol=3, cex=0.4)
  b <- barplot(myfreq, beside=TRUE, 
               ylim=c(0, max(myfreq) + 15),
               col=g, 
               cex.names = 0.8,
               xlab=d,  ylab="Count", main=e, legend.text=TRUE)
  text(b, myfreq + 5, myfreq, font=1.5, col=g)
  round(prop.table(myfreq),2)}

# Age Group by gender
mydata["age_q"] <- factor(mydata$age_q, levels = c("Below 20", 
                                                       "From 20 to 29", "From 30 to 39", 
                                                       "40 and above"), ordered=T)
myfreq_count("gender_q", "age_q", "Age Group", 
             "Age Group by Gender", c("Female", "Male"), c("darkred", "darkblue"))

# Employment Satus by gender
mydata["status_q"] <- factor(mydata$status_q, levels = c("Employed", "Student", 
                                                             "Self-employed", 
                                                             "Housewife"), ordered=T)
myfreq_count("gender_q", "status_q", "Employment Status",  
             "Employment Status by Gender", c("Female", "Male"), c("darkred", "darkblue"))

# Percentage of income level
mydata["income_q"] <- factor(mydata$income_q, 
                             levels = c("Less than RM25,000", "RM25,000 - RM50,000", 
                                        "RM50,000 - RM100,000","RM100,000 - RM150,000", 
                                        "More than RM150,000"), 
                             labels=c("< 25k", "25k-50k", "50k-100k","100k-150k","> 150k"), 
                             ordered=T)
myfreq_count("gender_q", "income_q", "Income Level",  
             "Income Level by Gender", c("Female", "Male"), c("darkred", "darkblue"))

# Employment Satus by loyalty
myfreq_count("loyal_q", "status_q", "Employment Status",  
             "Loyalty by Employment Status", c("No", "Yes"), c("darkgrey", "darkgreen"))

# Income level by loyalty
myfreq_count("loyal_q", "income_q", "Income Level", 
             "Loyalty by Income Level", c("No", "Yes"), c("darkgrey", "darkgreen"))

# Age Group by loyalty
myfreq_count("loyal_q", "age_q", "Age Group", 
             "Loyalty by Age Group", c("No", "Yes"), c("darkgrey", "darkgreen"))

# Average amount spent by loyalty
mydata["spendPurchase_q"] <- factor(mydata$spendPurchase_q, 
                                      levels = c("Zero","Less than RM20", 
                                       "Around RM20 - RM40","More than RM40"), 
                                      labels=c("Zero", "< 20", "Around 20-40",  "> 40"))
myfreq_count("loyal_q", "spendPurchase_q", "Average Amout Spent", 
             "Loyalty by Average Amount Spent", c("No", "Yes"), c("darkgrey", "darkgreen"))

# Frequency of Starbucks visits by loyalty
myfreq_count("loyal_q", "visitNo_q", 
             "How often do you visit Starbucks?", 
             "Loyalty by Frequency of Starbucks Visits",
             c("No", "Yes"), c("darkgrey", "darkgreen"))

# Distance by loyalty
myfreq_count("loyal_q", "location_q", 
             "How far is the nearest Starbucks to you?", 
             "Loyalty by Distance to The Nearest Starbucks",
             c("No", "Yes"), c("darkgrey", "darkgreen"))

# Loyalty by Service Quality
myfreq_count("loyal_q", "serviceRate_q", 
             "How would you rate the service at Starbucks?", 
             "Loyalty by Severice Rating on Starbucks",
             c("No", "Yes"), c("darkgrey", "darkgreen"))

# Loyalty by Importance of Promotion
myfreq_count("loyal_q", "promoRate_q", 
             "How important are sales and promotions in your purchase decision?", 
             "Loyalty by Importance of Promotion",
             c("No", "Yes"), c("darkgrey", "darkgreen"))

# Loyalty by Rating on Product
myfreq_count("loyal_q", "productRate_q", 
             "How would you rate the quality of Starbucks \n compared to other brands(e.g., Coffee Beane)?", 
             "Loyalty by Rating on Product",
             c("No", "Yes"), c("darkgrey", "darkgreen"))

# Loyalty by Rating on Product Pricer
myfreq_count("loyal_q", "priceRate_q", 
             "How would you rate the price range at Starbucks?", 
             "Loyalty by Rating on Product Price",
            c("No", "Yes"), c("darkgrey", "darkgreen"))

