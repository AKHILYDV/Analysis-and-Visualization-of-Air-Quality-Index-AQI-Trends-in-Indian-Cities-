# 📦 Step 1: Load Required Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

# 📂 Step 2: Load Excel File (Update path as needed)
aqi_data <- read_excel("C:/Users/AKHIL YADAV/Downloads/InfosysDataset.xlsx")

# 🧹 Step 3: Select Needed Columns
aqi_data <- aqi_data %>%
  select(City = StationId, Date, AQI, AQI_Bucket)

# ⏱️ Step 4: Clean & Convert Data
aqi_data$Date <- as.Date(aqi_data$Date)
aqi_data$AQI <- as.numeric(aqi_data$AQI)
aqi_data <- na.omit(aqi_data)

# 📊 Step 5: Bar Graph - Average AQI by City
avg_aqi_by_city <- aqi_data %>%
  group_by(City) %>%
  summarise(Avg_AQI = mean(AQI))

ggplot(avg_aqi_by_city, aes(x = reorder(City, -Avg_AQI), y = Avg_AQI, fill = City)) +
  geom_col() +
  labs(title = "Average AQI in Each City", x = "City", y = "Average AQI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 📈 Step 6: Line Graph - AQI Over Time
ggplot(aqi_data, aes(x = Date, y = AQI, color = City)) +
  geom_line(alpha = 0.6) +
  labs(title = "AQI Trend Over Time", x = "Date", y = "AQI") +
  theme_minimal()

# 📅 Step 7: Monthly Trends by City
aqi_data$Month <- month(aqi_data$Date, label = TRUE)

monthly_trend <- aqi_data %>%
  group_by(City, Month) %>%
  summarise(Avg_AQI = mean(AQI))

ggplot(monthly_trend, aes(x = Month, y = Avg_AQI, color = City, group = City)) +
  geom_line() +
  labs(title = "Monthly AQI Trend by City", x = "Month", y = "Average AQI") +
  theme_minimal()

# 🧾 Step 8: AQI Category Count
category_count <- aqi_data %>%
  group_by(AQI_Bucket) %>%
  summarise(Days = n())

ggplot(category_count, aes(x = AQI_Bucket, y = Days, fill = AQI_Bucket)) +
  geom_col() +
  labs(title = "Distribution of AQI Categories", x = "AQI Category", y = "Number of Days") +
  theme_minimal()

# 📋 Step 9: Summary Table
summary_table <- aqi_data %>%
  group_by(City) %>%
  summarise(
    Average_AQI = mean(AQI),
    Max_AQI = max(AQI),
    
    Min_AQI = min(AQI)
  )
print(summary_table)
