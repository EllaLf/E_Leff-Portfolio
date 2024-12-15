# Load and view data
data = read.csv("C:/Users/ae7le/OneDrive/Documents/Sara Schenirer/Intro to Data Science/Final_Project/Call_Center.csv")
summary(data)
head(data)


# Is the type of customer service channel significant to customer satisfaction?

# View the different options of customer service channel
unique(data$Channel)

# Drop the N/As in the Csat score column
data1 = na.omit(data)
summary(data1)

# Create a linear regression model for Csat score by channel
model1 <- lm(Csat.Score ~ Channel, data = data1)
summary(model1)

# Show a bar plot of the mean Csat score by channel
library(ggplot2)
ggplot(data1, aes(x = factor(Channel), y = Csat.Score)) + 
  stat_summary(fun = "mean", geom = "bar")

# Use the chi-squared test to look for correlation between Sentiment and Channel (no need to drop rows)
chisq.test(data$Sentiment, data$Channel)


# Are some call centers better than others i.e. is the call center city significant in the two customer satisfaction scores?

# Linear regression model for Csat score by Call Centre City
model2 <- lm(Csat.Score ~ Call.Centres.City, data = data1)
summary(model2)

# Chi-squared test between Sentiment and Call Centre City
chisq.test(data$Sentiment, data$Call.Centres.City)


# What is the most significant factor for customer satisfaction?

# Create a linear model for CSat score with all likely factors for customer satisfaction
model3 <- lm(Csat.Score ~ Call.Centres.City + Channel + Reason + Response.Time + Call.Duration.In.Minutes, data = data1)
summary(model3)