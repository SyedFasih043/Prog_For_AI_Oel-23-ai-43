# Import necessary library for data handling
library(dplyr)

# Loading dataset into the R environment
data <- read.csv("customer_churn.csv")

# Selecting customers who have churned
churned_customers <- filter(data, Churn == "Yes")
cat("These are the customers who have churned:\n")
print(churned_customers)

# Adding a new column for charge gap calculation
data <- mutate(data, ChargeGap = TotalCharges - (MonthlyCharges * Tenure))

# Remove rows with missing ChargeGap values
data <- data[!is.na(data$ChargeGap), ]
cat("Data after adding the ChargeGap column:\n")
print(data)

# Find customers who have been with the company for over 2 years and haven't churned
active_customers <- data[data$Tenure > 24 & data$Churn == "No", ]
cat("Here are the active customers who stayed more than 2 years:\n")
print(active_customers)

# Calculate the average monthly charges grouped by contract type
average_charges <- data %>% 
  group_by(ContractType) %>%
  summarise(AverageMonthlyCharges = mean(MonthlyCharges))
cat("Average Monthly Charges by Contract Type:\n")
print(average_charges)

# Categorize customers based on their age group
data$AgeGroup <- ifelse(data$Age < 25, "Youth", 
                        ifelse(data$Age >= 25 & data$Age <= 55, "Adult", "Senior"))
cat("Data after categorizing by Age Group:\n")
print(data)

# Find top 5 cities with the most churned customers
top_churn_cities <- data %>% 
  filter(Churn == "Yes") %>%
  count(City) %>%
  arrange(desc(n)) %>%
  head(5)
cat("Top 5 cities with the most churned customers:\n")
print(top_churn_cities)

# Extract names and cities of customers who meet specific conditions
special_customers <- filter(data, TotalCharges > 3000, 
                            ContractType == "Month-to-Month", 
                            Churn == "Yes") %>%
  select(Name, City)
cat("Customers with high charges and a month-to-month contract who have churned:\n")
print(special_customers)

# Create a summary table for each contract type showing average tenure and total revenue
summary_table <- data %>% 
  group_by(ContractType) %>%
  summarise(AverageTenure = mean(Tenure), 
            TotalRevenue = sum(TotalCharges))
cat("Summary of contract types with average tenure and total revenue:\n")
print(summary_table)
