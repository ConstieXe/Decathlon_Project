data_2022 <- read_parquet("C:/Users/ADMINS/Downloads/trans_2022.parquet")
data_2023 <- read_parquet("C:/Users/ADMINS/Downloads/trans_2023.parquet")
data_2024 <- read_parquet("C:/Users/ADMINS/Downloads/trans_2024.parquet")
data <- rbind(data_2022, data_2023, data_2024)

today <- as.Date("2024-12-31")

churn_data <- data %>%
  group_by(anonymous_cus_id) %>%
  summarise(
    InitialPurchase = min(transaction_amount),                         # Initial purchase amount
    LastPurchaseDate = max(date_transaction),                         # Last purchase date
    LastPurchaseAmount = transaction_amount[which.max(date_transaction)], # Last purchase amount
    Churn = ifelse(
      as.numeric(difftime(today, max(date_transaction), units = "days")) > 365 & # No purchase in 12 months
        (transaction_amount[which.max(date_transaction)] < 0.4 * min(transaction_amount)), # Repurchase < 40% of initial
      TRUE, # Churned
      FALSE # Not churned
    )
  )

churn_data_avg <- data %>%
       group_by(anonymous_cus_id) %>%
       summarise(
             InitialPurchase = min(avg_price),                         # Initial purchase amount
             LastPurchaseDate = max(date_transaction),                         # Last purchase date
             LastPurchaseAmount = avg_price[which.max(date_transaction)], # Last purchase amount
             Churn = ifelse(
                   as.numeric(difftime(today, max(date_transaction), units = "days")) > 365 & # No purchase in 12 months
                         (avg_price[which.max(date_transaction)] < 0.4 * min(avg_price)), # Repurchase < 40% of initial
                   TRUE, # Churned
                   FALSE # Not churned
               )
      )

churn_data_avg_or <- data %>%
       group_by(anonymous_cus_id) %>%
       summarise(
             InitialPurchase = min(avg_price),                         # Initial purchase amount
             LastPurchaseDate = max(date_transaction),                         # Last purchase date
             LastPurchaseAmount = avg_price[which.max(date_transaction)], # Last purchase amount
             Churn = ifelse(
                   as.numeric(difftime(today, max(date_transaction), units = "days")) > 365 | # No purchase in 12 months
                         (avg_price[which.max(date_transaction)] < 0.4 * min(avg_price)), # Repurchase < 40% of initial
                  TRUE, # Churned
                   FALSE # Not churned
               )
        )