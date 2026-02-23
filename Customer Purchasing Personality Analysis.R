# Read customer dataset from local file path
customer_data <- read.table(
  file = "E:/Data Analytics for Process Improvement/DAPI Project 1/customer_data.txt",
  header = TRUE,          # First row contains column names
  sep = ","               # Data is comma-separated
)

# Preview first 6 rows
head(customer_data)

# Open dataset in RStudio data viewer
View(customer_data)

# Remove rows containing any missing values
customer_data = na.omit(customer_data)

# Display column names
names(customer_data)

# Load visualization library
library(ggplot2)

# -------------------------------
# Visualization 1: Education vs Discount Purchases
# -------------------------------

# Create boxplot showing number of discount purchases by education level
viz_1_education = ggplot(customer_data, aes(x=Education, y=NumDealsPurchases)) +
  geom_boxplot(outlier.colour = "Blue", fill="SteelBlue", color = "Black") +
  labs(title="Discount Purchases by Education Level",
       x="Education Level",
       y="Number of Discount Purchases")

# Display plot
viz_1_education

# Define function returning visualization (not necessary but wraps plot)
viz_1 = function(customer_data){viz_1_education}

viz_1

# -------------------------------
# Feature Engineering
# -------------------------------

# Create FamilySize variable by summing children and teens at home
customer_data$FamilySize = customer_data$Kidhome + customer_data$Teenhome

# Summary statistics of Income
summary(customer_data$Income)

# Load data manipulation library
library(dplyr)

# Create income category buckets using conditional logic
customer_data = customer_data %>%
  mutate(
    IncomeInterval = case_when(
      Income <= 1730 ~ "Very Less",
      Income <= 3503 ~ "Less",
      Income <= 52247 ~ "Neutral",
      Income <= 68522 ~ "High",
      Income <= 666666 ~ "Very High"
    )
  )

# -------------------------------
# Average Discount Purchases by Income Group
# -------------------------------

# Compute average number of discount purchases per income category
avg_discount_purchases <- customer_data %>%
  group_by(IncomeInterval) %>%
  summarise(AvgNumDealsPurchases = mean(NumDealsPurchases, na.rm = TRUE))

# Create bar chart of average discount purchases by income level
viz_2_income = ggplot(avg_discount_purchases, aes(x=IncomeInterval, y=AvgNumDealsPurchases)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  labs(title="Average Discounted Purchases by Income Level",
       x="Income Level",
       y="Average Number of Discounted Purchases") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# -------------------------------
# Campaign Acceptance Analysis
# -------------------------------

# Sum total acceptances across campaign columns
campaign_sums <- sapply(
  customer_data[c("AcceptedCmp1","AcceptedCmp2","AcceptedCmp3",
                  "AcceptedCmp4","AcceptedCmp5","Response")],
  sum,
  na.rm = TRUE
)

# Convert campaign totals into dataframe for plotting
campaign_data <- data.frame(
  Campaign = names(campaign_sums),
  Acceptances = as.numeric(campaign_sums)
)

head(campaign_data)

# Plot campaign acceptance counts
viz_3_campaign = ggplot(campaign_data, aes(x=Campaign, y=Acceptances, fill=Campaign)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(title="Acceptances Per Campaign",
       x="Campaign",
       y="Count of Acceptances") +
  scale_fill_brewer(palette="Pastel1") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# -------------------------------
# Customer Segmentation by Education & Minors
# -------------------------------

# Filter only customers who responded to campaign
customer_segments = customer_data %>%
  filter(Response == 1) %>%
  mutate(Minors = ifelse(FamilySize > 0, "Yes", "No")) %>%
  group_by(Education, Minors) %>%
  summarise(AverageIncome = mean(Income, na.rm = TRUE),
            Count = n()) %>%
  ungroup()

# Bar chart: Participation by education and minors
viz_4_minors = ggplot(customer_segments, aes(x=Education, y=Count, fill=Minors)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Response Campaign Participation by Education and Minors at Home",
       x="Education Level",
       y="Count of Participants") +
  scale_fill_brewer(palette="Set2") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# Scatter plot: Income vs participation count
viz_5_segs = ggplot(customer_segments, aes(x=Education, y=AverageIncome, color=Minors)) +
  geom_point(size=4, alpha=0.6) +
  geom_text(aes(label=Count), vjust=1.5, color="black") +
  labs(title="Average Income and Participation by Education Level",
       x="Education Level",
       y="Average Income")

# -------------------------------
# Refined Segmentation by Children Type
# -------------------------------

customer_segments <- customer_data %>%
  filter(Response == 1) %>%
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

# Plot participation by education and household type
ggplot(customer_segments, aes(x=Education, y=Count, fill=Children)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Campaign Participation by Education and Household Type",
       x="Education Level",
       y="Count of Participants")

# -------------------------------
# Spending Analysis
# -------------------------------

# Create total spending variable across product categories
customer_data <- customer_data %>%
  mutate(
    TotalSpending = MntWines + MntFruits + MntMeatProducts +
      MntFishProducts + MntSweetProducts + MntGoldProds,
    UsedDiscount = ifelse(NumDealsPurchases > 0, "Yes", "No")
  )

# Calculate average spending by discount usage
average_spending <- customer_data %>%
  group_by(UsedDiscount) %>%
  summarise(AverageTotalSpending = mean(TotalSpending, na.rm = TRUE))

# Plot average spending comparison
ggplot(average_spending, aes(x=UsedDiscount, y=AverageTotalSpending, fill=UsedDiscount)) +
  geom_bar(stat="identity") +
  labs(title="Average Total Spending Comparison")

# -------------------------------
# Detailed Spending Statistics
# -------------------------------

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

# -------------------------------
# Total Spending by Income Level
# -------------------------------

total_spending_by_income <- customer_data %>%
  group_by(IncomeInterval) %>%
  summarise(TotalSpending = sum(TotalSpending, na.rm = TRUE))

# Plot total spending by income bracket
ggplot(total_spending_by_income, aes(x=IncomeInterval, y=TotalSpending, fill=IncomeInterval)) +
  geom_bar(stat="identity") +
  labs(title="Total Spending by Income Level")

# View final summary statistics
View(summary_stats)