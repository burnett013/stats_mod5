# ||============ Begin Analysis of Beer Production Data ============||
# Repo: https://github.com/burnett013/stats_mod5.git

# === Load Required Libraries ===
# install.packages(c("lubridate", "ggplot2", "dplyr", "car", "readxl", "httpgd"))

library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(car)

# === DEFINE YOUR FILE PATH ===
file_path <- "/Users/andyburnett/Library/Mobile Documents/com~apple~CloudDocs/Desktop/X03.27.25/Education/Graduate/USF Grad/Classes/SU25/QMB6304_Stats_1/module_5/assignment_5"
full_path <- file.path(file_path, "6304 Assignment 5 Data.xlsx")

# === Load Data ===
if (!file.exists(full_path)) stop("Check the file path!")

data <- read_excel(full_path)

# === Standardize column names if needed ===
if (!all(c("index","date","production") %in% colnames(data))) {
  colnames(data) <- c("index", "date", "production")
  message("Coluns renamed to: index, date, production")
}

# === Ensure date column is a proper Date ===
if (!inherits(data$date, "Date")) {
  data$date <- as.Date(data$date)
}

# === Now safely add year & month ===
if (!"year" %in% colnames(data)) {
  data$year  <- year(data$date)
  data$month <- month(data$date)
  message("Year & month columns created")
}

# === Debug print ===
glimpse(data)

# === Line Plot ===
ggplot(data, aes(x = index, y = production)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Monthly Beer Production in Australia (1956–1978)",
    x = "Index",
    y = "Beer Production (Megaliters)"
  )

# === Simple Regression Model ===
model1 <- lm(production ~ index, data = data)
summary(model1)

slope <- coef(model1)[["index"]]
predicted <- predict(model1)
correlation <- cor(data$production, predicted)
cat("Slope of regression line:", slope, "\n")
cat("Correlation coefficient:", correlation, "\n")

# === Plot with Regression Line ===
ggplot(data, aes(x = index, y = production)) +
  geom_line(color = "gray") +
  geom_line(aes(y = predicted), color = "red") +
  labs(
    title = "Beer Production with Simple Linear Regression Line",
    x = "Index",
    y = "Beer Production (Megaliters)"
  )

# === Durbin-Watson Test ===
dw_result <- durbinWatsonTest(model1)
print(dw_result)

# === Seasonal Index + Deseasonalization ===
data$month <- factor(data$month, levels = 1:12)
monthly_avg <- data %>%
  group_by(month) %>%
  summarise(avg_monthly_prod = mean(production), .groups = "drop")

data <- left_join(data, monthly_avg, by = "month")
data$seasonal_index <- data$avg_monthly_prod
data$deseasonalized <- data$production / data$seasonal_index * mean(data$seasonal_index)

# === Two Regression Models on Deseasonalized Data ===
model_a <- lm(deseasonalized ~ index, data = data)
model_b <- lm(deseasonalized ~ index + I(index^2), data = data)

data$fit_a <- predict(model_a)
data$fit_b <- predict(model_b)
data$reseason_a <- data$fit_a * data$seasonal_index / mean(data$seasonal_index)
data$reseason_b <- data$fit_b * data$seasonal_index / mean(data$seasonal_index)

# === Plot Original vs Reseasonalized Models ===
ggplot(data, aes(x = index)) +
  geom_line(aes(y = production), color = "black", linewidth = 1, alpha = 0.6) +
  geom_line(aes(y = reseason_a), color = "blue", linetype = "dashed") +
  geom_line(aes(y = reseason_b), color = "green", linetype = "dotted") +
  labs(
    title = "Original vs Fitted Values from Two Models",
    x = "Index",
    y = "Beer Production (Megaliters)",
    caption = "Black = Original, Blue = Linear Model, Green = Polynomial Model"
  )
  # ||============ END Analysis of Beer Production Data ============||