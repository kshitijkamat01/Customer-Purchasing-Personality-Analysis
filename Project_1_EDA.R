# Read the data, suppressing the warning
#lines <- readLines("E:/Data Analytics for Process Improvement/customer_data.txt", warn = FALSE)


customer_data <- read.table(file = "E:/Data Analytics for Process Improvement/DAPI Project 1/customer_data.txt", header = TRUE, sep = ",")# stringsAsFactors = FALSE)

head(customer_data)

View(customer_data)


customer_data = na.omit(customer_data) # Removes Null Value

names(customer_data)

library(ggplot2)


viz_1_education = ggplot(customer_data, aes(x=Education, y=NumDealsPurchases)) +
  geom_boxplot(outlier.colour = "Blue", fill="SteelBlue", color = "Black") +
  labs(title="Discount Purchases by Education Level", x="Education Level", y="Number of Discount Purchases")

viz_1_education
viz_1 = function(customer_data){viz_1_education}

viz_1
customer_data$FamilySize = customer_data$Kidhome + customer_data$Teenhome



summary(customer_data$Income)
library(dplyr)
customer_data = customer_data%>%
  mutate(
    IncomeInterval = case_when(
      Income <= 1730 ~ "Very  Less",
      Income <= 3503 ~ "Less",
      Income <= 52247 ~ "Neutral",
      Income <= 68522 ~ "High",
      Income <= 666666 ~ "Very High",
    )
  )

avg_discount_purchases <- customer_data %>%
  group_by(IncomeInterval) %>%
  summarise(AvgNumDealsPurchases = mean(NumDealsPurchases, na.rm = TRUE))


#sum_discount_purchases = customer_data %>%
#  group_by(IncomeInterval) %>%
#  summarise(Sum_Deals_Purchases = sum(NumDealsPurchases, na.rm = TRUE))
  
#ggplot(sum_discount_purchases, aes(x=IncomeInterval, y= Sum_Deals_Purchases))+
#  geom_bar(stat = "identity", fill = "yellow")+
#  theme_minimal()+
#  labs(title="Sum Discounted Purchases by Income Level",
#       x="Income Level",
#       y="Summing Number of Discounted Purchases") +
#  theme(axis.text.x = element_text(angle=45, hjust=1))
  

viz_2_income = ggplot(avg_discount_purchases, aes(x=IncomeInterval, y=AvgNumDealsPurchases)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  labs(title="Average Discounted Purchases by Income Level",
       x="Income Level",
       y="Average Number of Discounted Purchases") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# Cannot use bar plot on counts since it is incorrect analysis
#Total_discount_purchases <- customer_data %>%
#  group_by(IncomeInterval) %>%
#  summarise(Total_discounts = sum(NumDealsPurchases, na.rm = TRUE))

#ggplot(Total_discount_purchases, aes(x=IncomeInterval, y=Total_discounts)) +
#  geom_bar(stat="identity", fill="darkblue") +
#  theme_minimal() +
#  labs(title="Total Discounted Purchases by Income Level",
#       x="Income Level",
#       y="Total Number of Discounted Purchases") +
#  theme(axis.text.x = element_text(angle=45, hjust=1)) # This rotates x-axis labels for better readability

# Prepare the data

campaign_sums <- sapply(customer_data[c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "Response")], sum, na.rm = TRUE)

# Convert to a data frame for ggplot
campaign_data <- data.frame(
  Campaign = names(campaign_sums),
  Acceptances = as.numeric(campaign_sums)
)

head(campaign_data)
# Plotting
viz_3_campaign = ggplot(campaign_data, aes(x=Campaign, y=Acceptances, fill=Campaign)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Acceptances Per Campaign", x="Campaign", y="Count of Acceptances") +
  scale_fill_brewer(palette="Pastel1") + # Optional: Adds a color palette
  theme(axis.text.x = element_text(angle=45, hjust=1)) # Improve readability of x-axis labels

customer_segments = customer_data %>%
  filter(Response == 1) %>%
  mutate(Minors = ifelse(FamilySize > 0, "Yes", "No")) %>%
  group_by(Education, Minors) %>%
  summarise(AverageIncome = mean(Income, na.rm = TRUE),
            Count = n()) %>%
  ungroup()

viz_4_minors = ggplot(customer_segments, aes(x=Education, y=Count, fill=Minors)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Response Campaign Participation by Education and Minors at Home",
       x="Education Level",
       y="Count of Participants") +
  scale_fill_brewer(palette="Set2") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  theme(plot.title = element_text(size = 10))

viz_5_segs = ggplot(customer_segments, aes(x=Education, y=AverageIncome, color=Minors)) +
  geom_point(size=4, alpha=0.6) +
  geom_text(aes(label=Count), vjust=1.5, color="black") +
  labs(title="Average Income and Participation by Education Level",
       x="Education Level",
       y="Average Income") +
  scale_color_brewer(palette="Set1") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

customer_segments <- customer_data %>%
  filter(Response == 1) %>%  # Assuming AcceptedCmp1 is the focus
  mutate(Children = case_when(
    Kidhome > 0 & Teenhome == 0 ~ "Kids Only",
    Kidhome == 0 & Teenhome > 0 ~ "Teens Only",
    Kidhome > 0 & Teenhome > 0 ~ "Both Kids and Teens",
    TRUE ~ "No Kids or Teens"
  )) %>%
  group_by(Education, Children) %>%
  summarise(AverageIncome = mean(Income, na.rm = TRUE),
            Count = n(),
            .groups = 'drop')

ggplot(customer_segments, aes(x=Education, y=Count, fill=Children)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label = round(Count, 2)), # Round to 2 decimal places
            position = position_dodge(width = 1), 
            vjust = -0.25, # Adjust this to position the text above the bars
            hjust = 0.5,
            color = "black")+
  labs(title="Campaign Participation by Education and Household Type",
       x="Education Level",
       y="Count of Participants") +
  scale_fill_brewer(palette="Set3") +
  theme(axis.text.x = element_text(angle=45, hjust=1))


customer_data <- customer_data %>%
  mutate(TotalSpending = MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds,
         UsedDiscount = ifelse(NumDealsPurchases > 0, "Yes", "No"))

average_spending <- customer_data %>%
  group_by(UsedDiscount) %>%
  summarise(AverageTotalSpending = mean(TotalSpending, na.rm = TRUE))

ggplot(average_spending, aes(x=UsedDiscount, y=AverageTotalSpending, fill=UsedDiscount)) +
  geom_bar(stat="identity") +
  labs(title="Average Total Spending Comparison",
       x="Used Discount",
       y="Average Total Spending") +
  scale_fill_brewer(palette="Set2") +
  theme_minimal()

summary_stats <- customer_data %>%
  group_by(UsedDiscount) %>%
  summarise(
    MeanTotalSpending = mean(TotalSpending, na.rm = TRUE),
    MedianTotalSpending = median(TotalSpending, na.rm = TRUE),
    SDTotalSpending = sd(TotalSpending, na.rm = TRUE),
    MinTotalSpending = min(TotalSpending, na.rm = TRUE),
    MaxTotalSpending = max(TotalSpending, na.rm = TRUE),
    VarianceTotalSpending = var(TotalSpending, na.rm = TRUE)
  )

print(summary_stats)


total_spending_by_income <- customer_data %>%
  group_by(IncomeInterval) %>%
  summarise(TotalSpending = sum(TotalSpending, na.rm = TRUE))

ggplot(total_spending_by_income, aes(x=IncomeInterval, y=TotalSpending, fill=IncomeInterval)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = round(TotalSpending, 2)), # Round to 2 decimal places
            position = position_dodge(width = 1), 
            vjust = -0.25, # Adjust this to position the text above the bars
            hjust = 0.5,
            color = "black")+
  labs(title="Total Spending by Income Level",
       x="Income Level",
       y="Total Spending") +
  scale_fill_brewer(palette="Spectral") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

View(summary_stats)
