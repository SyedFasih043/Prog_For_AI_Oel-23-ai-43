!pip install -q rpy2
#TASK # 1
df <- read.csv("customer_churn.csv", stringsAsFactors = FALSE)

churned_customers <- df[df$Churn == "Yes", ]
number_of_churned <- nrow(churned_customers)
print(number_of_churned)


#Task 2: Create ChargeGap column
df$ChargeGap <- df$TotalCharges - (df$MonthlyCharges * df$Tenure)

# Replace NA values in ChargeGap with 0
df$ChargeGap[is.na(df$ChargeGap)] <- 0

#Task 3: Filter customers with tenure > 24 and churn == "No"

active_customers <- df[df$Tenure > 24 & df$Churn == "No", ]

#Task 4: Average MonthlyCharges per ContractType

avg_month_month <- mean(df$MonthlyCharges[df$ContractType == "Month-to-month"], na.rm = TRUE)
avg_one_year <- mean(df$MonthlyCharges[df$ContractType == "One year"], na.rm = TRUE)
avg_two_year <- mean(df$MonthlyCharges[df$ContractType == "Two year"], na.rm = TRUE)

print(avg_month_month)
print(avg_one_year)
print(avg_two_year)

#Task 5: Categorize customers by Age
df$AgeGroup <- ifelse(df$Age < 25, "Youth",
                 ifelse(df$Age <= 55, "Adult", "Senior"))

#Task 6: Top 5 cities with most churned customers

churned <- df[df$Churn == "Yes", ]
city_counts <- table(churned$City)
sorted_cities <- sort(city_counts, decreasing = TRUE)
top_5_cities <- head(sorted_cities, 5)

print(top_5_cities)

#Task 7: Filter customers with TotalCharges > 3000, Month-to-Month, and churned

filtered_customers <- df[df$TotalCharges > 3000 & 
                         df$ContractType == "Month-to-month" & 
                         df$Churn == "Yes", ]

selected_info <- filtered_customers[, c("Name", "City")]
print(selected_info)


#Task 8: Table showing average tenure and total revenue per ContractType

contract_types <- unique(df$ContractType)

for (contract in contract_types) {
  contract_data <- df[df$ContractType == contract, ]
  avg_tenure <- mean(contract_data$Tenure, na.rm = TRUE)
  total_revenue <- sum(contract_data$TotalCharges, na.rm = TRUE)
  
  print(paste("Contract Type:", contract))
  print(paste("Average Tenure:", round(avg_tenure, 2)))
  print(paste("Total Revenue:", round(total_revenue, 2)))
}
