#title: "Avocados Capstone Project"
#subtitle: "Data Science Professional Certificate Program (HarvardX - EdX Platform)"
#author: "Luis Sousa"
#date: "December 2021"

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = T)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", dependencies = T)
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org", dependencies = T)
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org", dependencies = T)
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org", dependencies = T)
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org", dependencies = T)
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org", dependencies = T)
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org", dependencies = T)
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org", dependencies = T)
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org", dependencies = T)
if(!require(corrgram)) install.packages("corrgram", repos = "http://cran.us.r-project.org", dependencies = T)
if(!require(elasticnet)) install.packages("corrgram", repos = "http://cran.us.r-project.org", dependencies = T)

library(tidyverse)
library(caret)
library(data.table)
library(stringi)
library(dplyr)
library(knitr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(corrgram)
library(elasticnet)

# Original Kaggle link with the Avocado dataset
Kaggle_data_link <- 
    "https://www.kaggle.com/neuromusic/avocado-prices/download/"

# Mirrow Avocado dataset on gitHub
gitHub_data_link <- 
  "https://github.com/luisfdsousa/EdX_Capstone_Avocado/raw/main/avocado.csv"

# Download link
data_weblink <- gitHub_data_link

# File name containing the desired dataset
data_file_name <- "avocado.csv"

# Create an empty avocados data frame
avocados <- NULL

#Check if data exists within the working directory
if (file.exists(data_file_name) == F) {
  
  #Info message
  cat("Avocado data not found.",
      "Next step: Download data from Kaggle")
  
  # Create temporary file to store the data
  temp_file <- tempfile()
  
  # Download the data from the web
  download.file(data_weblink, temp_file)
  
  # Open and load .cvs data 
  avocados <- read.csv(temp_file,
                       header = T)
  
  # Free memory, remove temp file
  rm(temp_file)
  
} else {
  
  # Open and load .cvs data 
  avocados <- read.csv(data_file_name,
                       header = T)
}

# Drop the first column (gibberish)
# drop also the bags column (description not clear)
avocados <- 
  avocados %>%            # pipe the avocados data set
  subset(select = -c(X,
                     Total.Bags,
                     Small.Bags,
                     Large.Bags,
                     XLarge.Bags))

# Rename columns to simplify reading
names(avocados)[names(avocados) == "Total.Volume"] <- "TotalVolume"
names(avocados)[names(avocados) == "X4046"] <- "T4046"
names(avocados)[names(avocados) == "X4225"] <- "T4225"
names(avocados)[names(avocados) == "X4770"] <- "T4770"
names(avocados)[names(avocados) == "type"] <- "Type"
names(avocados)[names(avocados) == "year"] <- "Year"
names(avocados)[names(avocados) == "region"] <- "Region"

#avocados$Year = as.factor(avocados$Year)
avocados$Type = as.factor(avocados$Type)
avocados$Region = as.factor(avocados$Region)
avocados$Year = as.factor(avocados$Year)
#avocados$Date = as.Date(avocados$Date)
avocados$Date = ymd(avocados$Date)
avocados <- add_column(avocados, Month = factor(months(avocados$Date), levels = month.name))
avocados <- add_column(avocados, Day = factor(format(avocados$Date, format = "%d")))
avocados <- avocados %>% mutate(NonHaasVolume = TotalVolume - T4046 - T4225 - T4770)

# Remove NA entries
avocados <- avocados %>% drop_na()

# Initiate seed for repeated results
# if using R 3.5 or earlier, use `set.seed(1)`
set.seed(1, sample.kind = "Rounding")

# 90/10 % ratio for test/validation datasets
training_samples <- 
  avocados$AveragePrice %>%
  createDataPartition(p = 0.9, 
                      list = FALSE)

# Create respective train and test datasets 
train_data  <- avocados[training_samples, ]
test_data <- avocados[-training_samples, ]


########################################
# BASIC DATA EXPLORATION
# Summary of the data
data_basics_metrics <- c(
  nrow(avocados),
  ncol(avocados),
  anyNA(avocados),
  n_distinct(avocados$Type),
  n_distinct(avocados$Region),
  min(year(avocados$Date)),
  max(year(avocados$Date)))

data_basics_description <- c(
  "Number of Rows",
  "Number of Columns",
  "Number of Invalid Data Points",
  "Distinct Types of Avocados",
  "Distinct Produce Regions",
  "Data Start Date",
  "Data End Date"
)

# Build data frame with columns + description
data_basics <- data.frame(data_basics_description,
                          data_basics_metrics)

# Name the columns of the table
names(data_basics)<-c("Metric","Result")

# Draw the table
data_basics %>%
  kbl(caption = "Basic Data Exploration: Avocado Dataset") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

########################################
# Build column description Table - START
# Save column names
avocados_col <- colnames(avocados)

# Add column meaning
avocados_col_meaning <- c(
  "The date of the observation",
  "The average price of a single avocado",
  "Total number of avocados sold",
  "Total number of avocados: Small/medium Haas",
  "Total number of avocados: Large Haas",
  "Total number of avocados: Extra large Haas",
  "Type of avocado",
  "Produce year",
  "Produce region"
)

# Build data frame with columns + description
results <- data.frame(avocados_col[1:(length(avocados_col) - 3)], 
                      avocados_col_meaning)

# Name the columns of the table
names(results) <- c("Columns", "Description")

# Draw the table
results %>%
  kbl(caption = "Avocado dataset: Columns description") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
# Build column description Table - END
########################################
########################################
# Advanced Data Exploration: Data Visualization
########################################
# Plot #1: Histograms of Average Avocado Prices
par(mar=c(1,1,1,1))

# Calculate mean average  prices per type of avocado
mean_avg_prices <- avocados %>%
  group_by(Type) %>%
  summarise(avg_price = mean(AveragePrice))

# Average Prices histogram
plot1_hist1 <- avocados %>% 
  ggplot(aes(x = AveragePrice, color = Type)) + 
  geom_histogram(binwidth = 0.05, position = "dodge", fill = "white", alpha = 0.5) + 
  facet_grid(Type ~ .) +
  geom_vline(data = mean_avg_prices, aes(xintercept = avg_price, color = Type), linetype = "dashed") + 
  labs(title = "Average Avocado Price", x = "Average Price per Avocado", y = "Count") + 
  theme_classic() + 
  theme(legend.position = "bottom")

# Change line colors by groups
plot1_hist2 <- avocados %>% 
  ggplot(aes(x = AveragePrice, color = Type, fill = Type)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, position = "dodge", alpha = 0.2) +
  facet_grid(Type ~ .) +
  geom_density(alpha = 0.3) +
  geom_vline(data = mean_avg_prices, aes(xintercept = avg_price, color = Type), linetype = "dashed") +
  labs(title = "Average Avocado Price (Density)", x = "Average Price per Avocado", y = "Density") +
  theme_classic() + 
  theme(legend.position = "bottom")

# Display plots
grid.arrange(plot1_hist1, plot1_hist2, ncol = 2) # Apply grid.arrange function

########################################
# Plot #2: Histograms of Average Avocado Prices
plot2_boxplot_price <- avocados %>%
  ggplot(aes(Type, AveragePrice, color = Year)) +
  geom_boxplot() +
  labs(title = "Average Avocado Price", x = "Type of Avocado", y = "Average Price per Avocado") +
  theme_classic() + 
  theme(legend.position = "bottom")

plot2_boxplot_volume <- avocados %>% 
  ggplot(aes(Type, TotalVolume, color = Year)) +
  geom_boxplot() +
  scale_y_continuous(trans='log2') +
  labs(title = "Total Volume Sold", x = "Type of Avocado", y = "Total Volume Sold") +
  theme_classic() + 
  theme(legend.position = "bottom")

# Display plots
grid.arrange(plot2_boxplot_price, plot2_boxplot_volume, ncol = 2) # Apply grid.arrange function


########################################
# Plot #3 Total volume sold by Type & Year
plot3_scatterplot_volPrice <- avocados %>%
  ggplot(aes(TotalVolume, AveragePrice, colour = Type)) +
  geom_point(alpha = 0.05) + 
  geom_smooth(color = "black", method = "lm", linetype = "dashed") +
  scale_x_continuous(trans = 'log2') +
  facet_grid(Type ~ .) +
  labs(title = "Average Avocado Price", x = "Total Volume Sold", y = "Average Price per Avocado") +
  theme_classic()

plot3_lineplot_TotalMonthlyRevenue <- avocados %>%
  group_by(Year, Type, Month) %>%
  select(AveragePrice, Year, Month, Type, TotalVolume) %>%
  mutate(MonthlyRevenue = AveragePrice*TotalVolume) %>%
  summarise(TotalMonthlyRevenue = sum(MonthlyRevenue)) %>%
  ggplot(aes(x = Month, y = TotalMonthlyRevenue, colour = Year, group = Year)) +
  geom_line() + 
  facet_grid(. ~Type) + 
  scale_y_continuous(trans = 'log2') +
  labs(x = "Month", y = "Monthly Revenue (in mn)", title = "Average Monthly Avocado Prices") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

# Display plots
grid.arrange(plot3_scatterplot_volPrice, plot3_lineplot_TotalMonthlyRevenue, nrow = 2)

########################################
# Plot #4: Total Rev sold by Size, Year & Type
plot4 <- avocados %>%
  gather(AvocadoSize, Volume, c(T4046:T4770,NonHaasVolume)) %>%
  mutate(Revenue = AveragePrice * Volume) %>% 
  group_by(Year, Month, Type, AvocadoSize) %>%
  summarise(TotalRevenue = sum(Revenue)/1000000) %>%
  ggplot(aes(x = Month, y = TotalRevenue, colour = Year, group = Year)) +
  facet_grid(vars(Type),vars(AvocadoSize), scales="free") +
  geom_line() +
  scale_y_continuous(trans = 'log2') +
  labs(y = "Monthly Revenue (in mn)", x ="Month", title = "Average Monthly Avocado Prices by Size", colour = 'Year') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
        panel.grid.minor = element_line(size = 0.4, linetype = 1),
        panel.grid.major = element_line(size = 0.75, linetype = 3))

# Display plot
plot4

# Plot #5: Average Price per date: Conventional/organic Avocados
plot5 <- avocados %>%
  ggplot(aes(x = Date, y = AveragePrice, color = Type)) +
  geom_line(alpha = 0.8) +
  theme_classic() +
  scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Display plot
plot5

# Plot #6: Total Rev sold by Size, Year & Type
plot6 <-  avocados %>% 
  ggplot(aes(x = Region, y = AveragePrice, colour = Type)) + #, group = Year
  geom_boxplot() + 
  facet_grid(.~Year, scales = "free") +
  scale_y_continuous(breaks = c(seq(round(min(avocados$AveragePrice),1),
                                    round(max(avocados$AveragePrice),1),0.2)), 
                     limits = c(round(min(avocados$AveragePrice),1),
                                round(max(avocados$AveragePrice),1))) +
  labs(x = "Produce Region", y ="Average Avocado Price", 
       title = "Average prices by region & year") +
  coord_flip() + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0),
        panel.grid.minor = element_line(size = 0.4, linetype = 1),
        panel.grid.major = element_line(size = 0.75, linetype = 3))

# Display plot
plot6

########################################
# Modeling
########################################
# MODEL 1: MEAN Model
# RMSE Function
RMSE <- function(observed_ratings, expected_ratings){
  sqrt(mean((observed_ratings - expected_ratings)^2))
}

# Compute the dataset's mean rating
mean_avocado_price <- mean(train_data$AveragePrice)
cat("The overall mean avocado price is: ", round(mean_avocado_price, 2))

# Validate model 1
model1.RMSE <- RMSE(test_data$AveragePrice, mean_avocado_price)
cat("RMSE =", model1.RMSE, "Assuming the same mean price", 
    round(mean_avocado_price, 2), "for all avocados")

# MODEL 1: Print results
train_data %>%
  ggplot(aes(x = Date, y = AveragePrice)) + 
  geom_point(color = "gray", alpha = 0.1) +
  scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y", expand = c(0,0)) +
  geom_hline(yintercept = mean_avocado_price, 
             linetype = 1, 
             size = 1) +
  labs(title = "MODEL 1: Average Model", x = "Date", y = "Average Avocado Price") + 
  theme_minimal() +
  geom_text(aes(median(Date), 
                round(mean_avocado_price, 2), 
                label = round(mean_avocado_price, 2),
                vjust = - 1))

# MODEL 2: Linear Regression
# Correlation matrix: Understand variables correlation
corrgram(avocados, order = TRUE, upper.panel = panel.cor, 
         main = "Correlation matrix: Avocados Dataset")

# Train Model
model2_linear = lm(AveragePrice ~ TotalVolume, data = avocados)
model2_linear.summary <- summary(model2_linear)
cat("R-squared =", model2_linear.summary$r.squared, "considering a linear model as a function of the total volume sold")

# Validate model
model2_linear.validation <- model2_linear %>% predict(test_data)
model2_linear.RMSE <- RMSE(test_data$AveragePrice, model2_linear.validation)
cat("RMSE =", model2_linear.RMSE, "considering a linear model as a function of the total volume sold")

# MODEL 2: Print results
test_data %>% 
  mutate(model2_price_prediction = model2_linear.validation) %>%
  ggplot() + 
  geom_point(aes(x = Date, y = AveragePrice, color = Type)) + 
  geom_line(aes(x = Date, y = model2_price_prediction)) +
  scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y", expand = c(0,0)) +
  labs(title = "MODEL 2: Linear Regression", x = "Date", y = "Average Avocado Price") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0),
        panel.grid.minor = element_line(size = 0.4, linetype = 1),
        panel.grid.major = element_line(size = 0.75, linetype = 3),
        legend.position = "bottom")

# MODEL 3: Multi Linear Regression
# Train Model: version 1 (TotalVolume + Type)
model3_multiLinear.1 = lm(AveragePrice ~ TotalVolume + Type, data = avocados)
model3_multiLinear.1.summary <- summary(model3_multiLinear.1)
model3_multiLinear.1.summary

# the lm function has taken conventional type as being 0

cat("R-squared =", model3_multiLinear.1.summary$r.squared, "considering a multi linear model (total volume & type) as a function of the total volume sold")

# Validate model: version 1 (TotalVolume + Type)
model3_multiLinear.1.validation <- model3_multiLinear.1 %>% predict(test_data)
model3_multiLinear.1.RMSE <- RMSE(test_data$AveragePrice, model3_multiLinear.1.validation)
cat("RMSE =", model3_multiLinear.1.RMSE, "considering a multi linear model as a function of the total volume sold")

# MODEL 3: Print results: version 1 (TotalVolume + Type)
model3_multiLinear.1.plot1 <- test_data %>% 
  mutate(model3_price_prediction = model3_multiLinear.1.validation) %>%
  ggplot() + 
  geom_point(aes(x = Date, y = AveragePrice, color = Type)) + 
  geom_line(aes(x = Date, y = model3_price_prediction)) +
  scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y", expand = c(0,0)) +
  labs(title = "MODEL 3: Multi Linear Regression (TotalVolume + Type)", x = "Date", y = "Average Avocado Price") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0),
        panel.grid.minor = element_line(size = 0.4, linetype = 1),
        panel.grid.major = element_line(size = 0.75, linetype = 3),
        legend.position = "bottom")

# Train Model: version 2 (TotalVolume + Type + Month + Year)
model3_multiLinear.2 = lm(AveragePrice ~ TotalVolume + Type + Month + Year, data = avocados)
model3_multiLinear.2.summary <- summary(model3_multiLinear.2)
model3_multiLinear.2.summary

cat("R-squared =", model3_multiLinear.2.summary$r.squared, "considering a multi linear model (Total volume + Type + Month + Year) as a function of the total volume sold")

# Validate model: version 2 (TotalVolume + Type + Month + Year)
model3_multiLinear.2.validation <- model3_multiLinear.2 %>% predict(test_data)
model3_multiLinear.2.RMSE <- RMSE(test_data$AveragePrice, model3_multiLinear.2.validation)
cat("RMSE =", model3_multiLinear.2.RMSE, "considering a multi linear model as a function of the total volume sold")

# MODEL 3: Print results: version 2 (TotalVolume + Type + Month + Year)
model3_multiLinear.2.plot2 <- test_data %>% 
  mutate(model3_price_prediction = model3_multiLinear.2.validation) %>%
  ggplot() + 
  geom_point(aes(x = Date, y = AveragePrice, color = Type)) + 
  geom_line(aes(x = Date, y = model3_price_prediction)) +
  scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y", expand = c(0,0)) +
  labs(title = "MODEL 3: Multi Linear Regression (TotalVolume + Type + Month + Year)", x = "Date", y = "Average Avocado Price") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0),
        panel.grid.minor = element_line(size = 0.4, linetype = 1),
        panel.grid.major = element_line(size = 0.75, linetype = 3),
        legend.position = "bottom")

# Train Model: version 3 (TotalVolume + Type + Month + Year + Region)
model3_multiLinear.3 = lm(AveragePrice ~ TotalVolume + Type + Month + Year + Region, data = avocados)
model3_multiLinear.3.summary <- summary(model3_multiLinear.3)
model3_multiLinear.3.summary

cat("R-squared =", model3_multiLinear.3.summary$r.squared, "considering a multi linear model (Total volume + Type + Month + Year + Region) as a function of the total volume sold")

# Validate model: version 3 (TotalVolume + Type + Month + Year + Region)
model3_multiLinear.3.validation <- model3_multiLinear.3 %>% predict(test_data)
model3_multiLinear.3.RMSE <- RMSE(test_data$AveragePrice, model3_multiLinear.3.validation)
cat("RMSE =", model3_multiLinear.3.RMSE, "considering a multi linear model as a function of the total volume sold")

# MODEL 3: Print results: version 3 (TotalVolume + Type + Month + Year + Region)
model3_multiLinear.3.plot3 <- test_data %>% 
  mutate(model3_price_prediction = model3_multiLinear.3.validation) %>%
  ggplot() + 
  geom_point(aes(x = Date, y = AveragePrice, color = Type)) + 
  geom_line(aes(x = Date, y = model3_price_prediction)) +
  scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y", expand = c(0,0)) +
  labs(title = "MODEL 3: Multi Linear Regression (TotalVolume + Type + Month + Year + Region)", x = "Date", y = "Average Avocado Price") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0),
        panel.grid.minor = element_line(size = 0.4, linetype = 1),
        panel.grid.major = element_line(size = 0.75, linetype = 3),
        legend.position = "bottom")

# Display multi linear regression plots
grid.arrange(model3_multiLinear.1.plot1, 
             model3_multiLinear.2.plot2, 
             model3_multiLinear.3.plot3, 
             nrow = 3)

# MODEL 4: Ridge regression
# Prepare data
model4.train_data <- train_data %>% select(-c(Date, T4046, T4225, T4770, Day, NonHaasVolume))
model4.test_data <- test_data %>% select(-c(Date, T4046, T4225, T4770, Day, NonHaasVolume))

# Train Model: Ridge Regression (TotalVolume + Type + Month + Year + Region)
model4.1 <- train(AveragePrice ~ ., 
                  data = model4.train_data,
                  method = "ridge")

# Validate model: Ridge Regression (TotalVolume + Type + Month + Year + Region)
model4.1.validation <- model4.1 %>% predict(model4.test_data)
model4.1.RMSE <- RMSE(model4.test_data$AveragePrice, model4.1.validation)
model4.1.R2 <- R2(model4.1.validation, model4.test_data$AveragePrice)

cat("Model 4: R-squared =", model4.1.R2)
cat("Model 4: RMSE =", model4.1.RMSE)

# MODEL 4: Ridge Regression (TotalVolume + Type + Month + Year + Region)
model4.1.plot1 <- test_data %>% 
  mutate(model4_price_prediction = model4.1.validation) %>%
  ggplot() + 
  geom_point(aes(x = Date, y = AveragePrice, color = Type)) + 
  geom_line(aes(x = Date, y = model4_price_prediction)) +
  scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y", expand = c(0,0)) +
  labs(title = "MODEL 4: Ridge Regression", x = "Date", y = "Average Avocado Price") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0),
        panel.grid.minor = element_line(size = 0.4, linetype = 1),
        panel.grid.major = element_line(size = 0.75, linetype = 3),
        legend.position = "bottom")

# print model results
model4.1.plot1

# Train Model: Lasso Regression (TotalVolume + Type + Month + Year + Region)
model4.2 <- train(AveragePrice ~ ., 
                  data = model4.train_data,
                  method = "lasso")

# Validate model: Lasso Regression (TotalVolume + Type + Month + Year + Region)
model4.2.validation <- model4.2 %>% predict(model4.test_data)
model4.2.RMSE <- RMSE(model4.test_data$AveragePrice, model4.2.validation)
model4.2.R2 <- R2(model4.2.validation, model4.test_data$AveragePrice)

cat("Model 4: R-squared =", model4.2.R2)
cat("Model 4: RMSE =", model4.2.RMSE)

# MODEL 4: Lasso Regression (TotalVolume + Type + Month + Year + Region)
model4.2.plot1 <- test_data %>% 
  mutate(model4_price_prediction = model4.2.validation) %>%
  ggplot() + 
  geom_point(aes(x = Date, y = AveragePrice, color = Type)) + 
  geom_line(aes(x = Date, y = model4_price_prediction)) +
  scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y", expand = c(0,0)) +
  labs(title = "MODEL 4: Lasso Regression", x = "Date", y = "Average Avocado Price") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0),
        panel.grid.minor = element_line(size = 0.4, linetype = 1),
        panel.grid.major = element_line(size = 0.75, linetype = 3),
        legend.position = "bottom")

# print model results
model4.1.plot1

# Train Model: Lasso Regression (TotalVolume + Type + Month + Year + Region)
control <- trainControl(method = "repeatedcv", # k-fold cross-validation
                        number = 10,   # number of folds
                        repeats = 10)  # repeated ten times

# train 
model4.3 <- train(AveragePrice ~ ., 
                  data = model4.train_data,
                  method = "lasso",
                  trControl = control)

# Validate model: Lasso Regression (TotalVolume + Type + Month + Year + Region)
model4.3.validation <- model4.3 %>% predict(model4.test_data)
model4.3.RMSE <- RMSE(model4.test_data$AveragePrice, model4.3.validation)
model4.3.R2 <- R2(model4.3.validation, model4.test_data$AveragePrice)

cat("Model 4: R-squared =", model4.3.R2)
cat("Model 4: RMSE =", model4.3.RMSE)

# MODEL 4: Lasso Regression (TotalVolume + Type + Month + Year + Region)
model4.3.plot1 <- test_data %>% 
  mutate(model4_price_prediction = model4.3.validation) %>%
  ggplot() + 
  geom_point(aes(x = Date, y = AveragePrice, color = Type)) + 
  geom_line(aes(x = Date, y = model4_price_prediction)) +
  scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y", expand = c(0,0)) +
  labs(title = "MODEL 4: Lasso Regression w/ k-fold", x = "Date", y = "Average Avocado Price") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0),
        panel.grid.minor = element_line(size = 0.4, linetype = 1),
        panel.grid.major = element_line(size = 0.75, linetype = 3),
        legend.position = "bottom")

# print model results
model4.3.plot1