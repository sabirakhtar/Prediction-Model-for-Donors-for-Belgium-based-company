
##############################
# 2 - Test Data Preparation #
##############################


# Load-in the libraries required
library(readr)
library(dplyr)
library(lubridate)
library(caret)

# Read the training data
campaign_train <- read_delim("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/data/campaign20140115.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)

# Read the donors data
donors <- read_delim("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/data/donors.csv", 
                     ";", escape_double = FALSE, na = "Missing", 
                     trim_ws = TRUE)

# Read the gifts data
gifts <- read_delim("C:/Users/user/Downloads/IESEG Studies/Descriptive  Predictive Analytics/Prediction of Donors/data/gifts.csv", 
                    ";", escape_double = FALSE, col_types = cols(date = col_date(format = "%d/%m/%Y")), 
                    trim_ws = TRUE)

# Assumption : There is no issue in data (Duplicate rows can be present)
# Taking the sum of amount per donor ID to remove duplicate IDs
campaign_train <- campaign_train %>%
  group_by(donorID) %>%
  summarise(amount = sum(amount))


# Renaming the amount column in train and gifts so as to clearly differentiate
colnames(campaign_train) <- c("donorID","amount_reactivation")
colnames(gifts) <- c("campID","donorID","amount_gifts","date" )

# Take only data present till 2014, because the training data is till 2014-01-15
gifts$year <- year(gifts$date)
gifts <- subset(gifts, date < as.Date("2014-01-15"))

# Create new variable:
# Donation_Per_Year - What amount has a person given in one year
gifts <- gifts %>%
  group_by(donorID,year) %>%
  mutate(Donation_Per_Year = sum(amount_gifts))


# Create new variables based on Recency - Frequency - Monetary
# Recency :
#   R_Years_Since_Last_Donation - How many year before (from 2013), the donor's last donation
#   R_Last_Donation_Amount - What was the last donation in amount
#   R_Years_Since_First_Donation - How many year before (from 2013), the donor's first donation
# Frequency :
#   Freq_Donations - What is the frequency of donations
#   Freq_Donations_3Years - Were there any donations in last 3 years (from 2013) (1 - yes, 0- no)
#   Freq_Donations_5Years - Were there any donations in last 5 years (from 2013) (1 - yes, 0- no)
#   Freq_Camps_Attended - What is the frequency of camps attended
#   Freq_Camps_3Years - What is the frequency of camps attended in last 3 years (from 2013)
#   Freq_Camps_5Years - What is the frequency of camps attended in last 5 years (from 2013)
#   Freq_Min_35_Donation_Per_Year - Is the donation more than 35 per year
#   Freq_Min_100_Donation_Per_Year - Is the donation more than 100 per year
# Monetary :
#   M_Total_Donation - Total amount donated till date
#   M_Max_Donated - Maximum amount donated till date
#   M_Min_Donated - Minimum amount donated till date
#   M_Median_Donated - Median amount donated till date
#   M_Total_Last_3Years - What is the total amount of donations in last 3 years (from 2013)
#   M_Total_Last_5Years - What is the total amount of donations in last 5 years (from 2013)

gifts <- gifts %>%
  group_by(donorID) %>%
  mutate(R_Years_Since_Last_Donation = 2014 - max(year),
         R_Last_Donation_Amount = sum(amount_gifts[date == max(date)]),
         R_Years_Since_First_Donation = 2014 - min(year),
         Freq_Donations = n(),
         Freq_Donations_3Years = max(ifelse(year %in% c(2011,2012,2013,2014),1,0)),
         Freq_Donations_5Years = max(ifelse(year %in% c(2009,2010,2011,2012,2013,2014),1,0)),
         Freq_Camps_Attended = n_distinct(campID),
         Freq_Camps_3Years = n_distinct(campID[year %in% c(2011,2012,2013,2014)]),
         Freq_Camps_5Years = n_distinct(campID[year %in% c(2009,2010,2011,2012,2013,2014)]),
         Freq_Min_35_Donation_Per_Year = max(ifelse(Donation_Per_Year>35,1,0)),
         Freq_Min_100_Donation_Per_Year = max(ifelse(Donation_Per_Year>100,1,0)),
         M_Total_Donation = sum(amount_gifts),
         M_Max_Donated = max(amount_gifts),
         M_Min_Donated = min(amount_gifts),
         M_Median_Donated = median(amount_gifts),
         M_Total_Last_3Years = sum(amount_gifts[year %in% c(2011,2012,2013,2014)]),
         M_Total_Last_5Years = sum(amount_gifts[year %in% c(2009,2010,2011,2012,2013,2014)]))

# Remove the varibales which are not required anymore
gifts$campID <- NULL
gifts$amount_gifts <- NULL
gifts$date <- NULL
gifts$year <- NULL
gifts$Donation_Per_Year <- NULL

# Take only unique fields so that now we have data with all unique donor IDs
# Which creates one-to-one mapping with the train data
gifts <- unique(gifts)


# Create a new varible for mapping zipcode based on Belgium's postal code
# Reference - https://en.wikipedia.org/wiki/List_of_postal_codes_in_Belgium
donors$zipcode_region <- "Missing"
donors$zipcode_region <- ifelse(donors$zipcode >= 1000 & donors$zipcode <= 1299, "Brussels", donors$zipcode_region)
donors$zipcode_region <- ifelse(donors$zipcode >= 1300 & donors$zipcode <= 1499, "Wallonia", donors$zipcode_region)
donors$zipcode_region <- ifelse(donors$zipcode >= 1500 & donors$zipcode <= 1999, "Flanders", donors$zipcode_region)
donors$zipcode_region <- ifelse(donors$zipcode >= 2000 & donors$zipcode <= 2999, "Flanders", donors$zipcode_region)
donors$zipcode_region <- ifelse(donors$zipcode >= 3000 & donors$zipcode <= 3499, "Flanders", donors$zipcode_region)
donors$zipcode_region <- ifelse(donors$zipcode >= 3500 & donors$zipcode <= 3999, "Flanders", donors$zipcode_region)
donors$zipcode_region <- ifelse(donors$zipcode >= 4000 & donors$zipcode <= 4999, "Wallonia", donors$zipcode_region)
donors$zipcode_region <- ifelse(donors$zipcode >= 5000 & donors$zipcode <= 5999, "Wallonia", donors$zipcode_region)
donors$zipcode_region <- ifelse(donors$zipcode >= 6000 & donors$zipcode <= 6599, "Wallonia", donors$zipcode_region)
donors$zipcode_region <- ifelse(donors$zipcode >= 6600 & donors$zipcode <= 6999, "Wallonia", donors$zipcode_region)
donors$zipcode_region <- ifelse(donors$zipcode >= 7000 & donors$zipcode <= 7999, "Wallonia", donors$zipcode_region)
donors$zipcode_region <- ifelse(donors$zipcode >= 8000 & donors$zipcode <= 8999, "Flanders", donors$zipcode_region)
donors$zipcode_region <- ifelse(donors$zipcode >= 9000 & donors$zipcode <= 9999, "Flanders", donors$zipcode_region)
donors$zipcode_region <- ifelse(is.na(donors$zipcode), "Missing", donors$zipcode_region)

# Remove the varibales which are not required anymore
donors$zipcode <- NULL

# There are too many regions present in data along with many missing fields
# So replace the region values with with missing(0) and non-missing(1) values
donors$region <- ifelse(!is.na(donors$region),1,0)

# Replace french(1) speakers with non-french(0) speakers
donors$language <- ifelse(donors$language == "F",1,0)


##############################
#  2 - Base Table Creation   #
##############################

# After cleaning all the data we can merge the data base on Donor ID
d13 <- merge(x=campaign_train, y=donors, by = "donorID", all.x = T)
d123 <- merge(x=d13, y=gifts, by = "donorID", all.x = T)

# Creating target variable
d123$dependant <- ifelse(d123$amount_reactivation > 35, 1,0)

d123$gender <- as.factor(d123$gender)
d123$zipcode_region <- as.factor(d123$zipcode_region)
d123$dependant <- as.factor(d123$dependant)

# Replacing Na values with 0
# For the columns Donors which were not present in Gifts
d123[is.na(d123)] <- 0

# Creating dummy varibles for the discrete variables in the data
df_dummies <- dummyVars(~ gender + zipcode_region,d123)
df_dummies = as.data.frame(predict(df_dummies , d123))

# Adding all dummy variables
final = cbind(d123,df_dummies)

# Removing all discrete variables
final$gender <- NULL
final$language <- NULL
final$zipcode_region <- NULL

test <- final

# Removing all the environment varibles apart from train
rm(list=setdiff(ls(), c("train","test")))
