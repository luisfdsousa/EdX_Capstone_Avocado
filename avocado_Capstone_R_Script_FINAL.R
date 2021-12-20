##########################################################
##########################################################
#title: "Avocados Pricing Engine: Capstone Project"
#subtitle: "Data Science Professional Certificate Program (HarvardX - EdX Platform)"
#author: "Luis Sousa"
#date: "December 2021"

##########################################################
##########################################################
# INSTALL AND LOAD NECESSARY PACKAGES
##########################################################
##########################################################
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", dependencies = T)
if(!require(caret)) install.packages("caret", dependencies = T)
if(!require(data.table)) install.packages("data.table", dependencies = T)
if(!require(stringi)) install.packages("stringi", dependencies = T)
if(!require(dplyr)) install.packages("dplyr", dependencies = T)
if(!require(knitr)) install.packages("knitr", dependencies = T)
if(!require(kableExtra)) install.packages("kableExtra", dependencies = T)
if(!require(ggplot2)) install.packages("ggplot2", dependencies = T)
if(!require(gridExtra)) install.packages("gridExtra", dependencies = T)
if(!require(lubridate)) install.packages("lubridate", dependencies = T)
if(!require(corrgram)) install.packages("corrgram", dependencies = T)
if(!require(elasticnet)) install.packages("elasticnet", dependencies = T)
if(!require(glmnet)) install.packages("glmnet", dependencies = T)
if(!require(tinytex)) install.packages("tinytex", dependencies = T)
if(!require(rmarkdown)) install.packages("rmarkdown", dependencies = T)
if(!require(rpart)) install.packages("rpart", dependencies = T)
if(!require(rpart.plot)) install.packages("rpart.plot", dependencies = T)

library(caret)
library(data.table)
library(stringi)
library(knitr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(corrgram)
library(elasticnet)
library(glmnet)
library(tinytex)
library(rmarkdown)
library(rpart)
library(rpart.plot)
library(tidyverse)
library(dplyr)

##########################################################
##########################################################
# DOWNLOAD AVOCADO DATASET
##########################################################
##########################################################

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

##########################################################
##########################################################
# DATA TRANSFORMATION AND CLEANING
##########################################################
##########################################################

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
names(avocados)[names(avocados) == "X4046"] <- "T4046S"
names(avocados)[names(avocados) == "X4225"] <- "T4225L"
names(avocados)[names(avocados) == "X4770"] <- "T4770XL"
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
avocados <- avocados %>% mutate(NonHaasVolume = TotalVolume - T4046S - T4225L - T4770XL)

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

##########################################################
##########################################################
# BASIC DATA EXPLORATION
##########################################################
##########################################################

# Build basic data metrics table - START
# Summary of the data
data_basics_metrics <- c(
  nrow(avocados),
  ncol(avocados),
  anyNA(avocados),
  round(mean(avocados$AveragePrice), 2),
  round(sd(avocados$AveragePrice), 2),
  n_distinct(avocados$Type),
  n_distinct(avocados$Region),
  min(year(avocados$Date)),
  max(year(avocados$Date)))

data_basics_description <- c(
  "Number of Rows",
  "Number of Columns",
  "Number of Invalid Data Points",
  "Average avocado price (whole dataset)",
  "SD avocado price  (whole dataset)",
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

##########################################################
##########################################################
# ADVANCED DATA EXPLORATION
##########################################################
##########################################################

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
  labs(title = "Total Volume Sold", x = "Type of Avocado", y = "Total Volume Sold (in units)") +
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
  summarise(TotalMonthlyRevenue = sum(MonthlyRevenue) / 1000000) %>%
  ggplot(aes(x = Month, y = TotalMonthlyRevenue, colour = Year, group = Year)) +
  geom_line() + 
  facet_grid(. ~Type) + 
  scale_y_continuous(trans = 'log2') +
  labs(x = "Month", y = "Monthly Revenue (in mn $)", title = "Average Monthly Avocado Prices") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

# Display plots
grid.arrange(plot3_scatterplot_volPrice, plot3_lineplot_TotalMonthlyRevenue, nrow = 2)

########################################
# Plot #4: Total Rev sold by Size, Year & Type
plot4 <- avocados %>%
  gather(AvocadoSize, Volume, c(T4046S:T4770XL,NonHaasVolume)) %>%
  mutate(Revenue = AveragePrice * Volume) %>% 
  group_by(Year, Month, Type, AvocadoSize) %>%
  summarise(TotalRevenue = sum(Revenue)/1000000) %>%
  ggplot(aes(x = Month, y = TotalRevenue, colour = Year, group = Year)) +
  facet_grid(vars(Type),vars(AvocadoSize), scales="free") +
  geom_line() +
  scale_y_continuous(trans = 'log2') +
  labs(y = "Monthly Revenue (in mn $)", x ="Month", title = "Average Monthly Avocado Prices by Size", colour = 'Year') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
        panel.grid.minor = element_line(size = 0.4, linetype = 1),
        panel.grid.major = element_line(size = 0.75, linetype = 3))

# Display plot
plot4

########################################
# Plot #5: Average Price per date: Conventional/organic Avocados
plot5 <- avocados %>%
  ggplot(aes(x = Date, y = AveragePrice, color = Type)) +
  geom_line(alpha = 0.8) +
  theme_classic() +
  scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Display plot
plot5

########################################
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

##########################################################
##########################################################
# MODEL ENGINEERING
##########################################################
##########################################################

########################################
# MODEL 1: MEAN Model
# MODEL 1: Train model
mean_avocado_price <- mean(train_data$AveragePrice)

# Validate model 1
model1.RMSE <- RMSE(test_data$AveragePrice, mean_avocado_price)
cat("Assuming the same average avocado price of: ", 
    round(mean_avocado_price, 2),
    " the RMSE (Model 1: Average) = ",
    model1.RMSE)

# MODEL 1: Plot results
model1.plot1 <- test_data %>%
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

########################################
# MODEL 2: Linear Regression
# Build Model Results Plot Function
build_validation_plot <- function(test_dataset, estimated_dataset, title, RMSE, R_squared) {
  plot <- test_dataset %>% 
    mutate(model4_price_prediction = estimated_dataset) %>%
    ggplot() + 
    geom_point(aes(x = Date, y = AveragePrice, color = Type)) + 
    geom_line(aes(x = Date, y = model4_price_prediction)) +
    scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y", expand = c(0,0)) +
    labs(title = title, x = "Date", y = "Average Avocado Price") + 
    theme_minimal() + 
    geom_text(aes(median(Date), 
                  max(AveragePrice), 
                  label = paste("RMSE = ", round(RMSE, 4), "R_squared = ",round(R_squared, 4))),
              color = "black") +
    theme(axis.text.x = element_text(angle = 0, vjust = 0),
          panel.grid.minor = element_line(size = 0.4, linetype = 1),
          panel.grid.major = element_line(size = 0.75, linetype = 3),
          legend.position = "bottom")
  return(plot)
}

########################################
# Correlation matrix: Understand variables correlation
corrgram(avocados, 
         order = TRUE, 
         lower.panel = panel.shade, 
         upper.panel = panel.pie,
         diag.panel = panel.minmax, 
         text.panel = panel.txt, 
         main = "Correlation matrix: Avocados Dataset")

# Prepare data
new_train_data <- train_data %>% select(-c(Date, T4046S, T4225L, T4770XL, Day, NonHaasVolume))
new_train_data <- new_train_data %>% filter(!Region %in% c("RaleighGreensboro", 
                                                           "Chicago", 
                                                           "Boston", 
                                                           "BaltimoreWashington"))
new_train_data.all <- model.matrix(~., new_train_data)[,-1]
new_train_data.x <- model.matrix(AveragePrice~., new_train_data)[,-1]
new_train_data.y <- new_train_data$AveragePrice
new_test_data <- test_data %>% select(-c(T4046S, T4225L, T4770XL, Day, NonHaasVolume))
new_test_data <- new_test_data %>% filter(!Region %in% c("RaleighGreensboro", 
                                                         "Chicago", 
                                                         "Boston", 
                                                         "BaltimoreWashington"))
new_test_data.all <- new_test_data
new_test_data <- new_test_data.all %>% select(-c(Date))
new_test_data.xy <- model.matrix(~., new_test_data)[,-1]
new_test_data.x <- model.matrix(AveragePrice~., new_test_data)[,-1]
new_test_data.y <- new_test_data$AveragePrice

########################################
# MODEL 2: Train model
model2 = lm(AveragePrice ~ TotalVolume, data = new_train_data)
model2.summary <- summary(model2)

# Validate model
model2.validation <- model2 %>% predict(new_test_data) %>% as.vector()
model2.RMSE <- RMSE(new_test_data$AveragePrice, model2.validation)
model2.R2 <- model2.summary$r.squared
cat("Model 2 - Linear Model: RMSE =", 
    model2.RMSE,
    "Model 2 - Linear Model: R-squared =", 
    model2.R2)

# MODEL 2: Plot results
model2.plot1 <- build_validation_plot(new_test_data.all, 
                                      model2.validation, 
                                      "MODEL 2 - Linear Regression",
                                      model2.RMSE,
                                      model2.R2)

########################################
# MODEL 3: Multi Linear Regression
# MODEL 3: T model: (V, T, M, Y, R)
model3 = lm(AveragePrice ~ TotalVolume + Type + Month + Year + Region, data = new_train_data)
model3.summary <- summary(model3)

# MODEL 3: Validate model: (V, T, M, Y, R)
model3.validation <- model3 %>% predict(new_test_data) %>% as.vector()
model3.RMSE <- RMSE(new_test_data$AveragePrice, model3.validation)
model3.R2 <- model3.summary$r.squared
cat("Model 3.3 - Multi Linear Model (V, T, M, Y, R): RMSE =", 
    model3.RMSE,
    "Model 3.3 - Multi Linear Model (V, T, M, Y, R): R-squared =", 
    model3.R2)

# MODEL 3: Plot results
model3.plot1 <- build_validation_plot(new_test_data.all,
                                      model3.validation,
                                      "MODEL 3.3 - Multi Linear Regression (V, T, M, Y, R)",
                                      model3.RMSE,
                                      model3.R2)

########################################
# MODEL 4: Other Methods
# MODEL 4.1: Ridge regression (V, T, M, Y, R)
# Evaluate best lambda using cross-validation
model4.1.cv <- cv.glmnet(new_train_data.x, 
                new_train_data.y, 
                alpha = 0) #alpha = 0 for ridge regression

# Fit the final model on the training data w/ min lambda
model4.1 <- glmnet(new_train_data.x, 
                   new_train_data.y, 
                   alpha = 0, 
                   lambda = model4.1.cv$lambda.min)

########################################
# MODEL 4.1: Validate model: (V, T, M, Y, R)
model4.1.validation <- model4.1 %>% predict(new_test_data.x) %>% as.vector()
model4.1.RMSE <- RMSE(new_test_data$AveragePrice, model4.1.validation)
model4.1.R2 <- R2(model4.1.validation, new_test_data$AveragePrice)

cat("Model 4.1 - Ridge Model: RMSE =", 
    model4.1.RMSE,
    "Model 4.1 - Ridge Model: R-squared =", 
    model4.1.R2)

# MODEL 4.1: Plot results
model4.1.plot1 <- build_validation_plot(new_test_data.all, 
                                        model4.1.validation, 
                                        "MODEL 4.1 - Ridge Regression",
                                        model4.1.RMSE,
                                        model4.1.R2)

########################################
# MODEL 4.2: Lasso regression (V, T, M, Y, R)
# Evaluate best lambda using cross-validation
model4.2.cv <- cv.glmnet(new_train_data.x, 
                         new_train_data.y, 
                         alpha = 1) #alpha = 1 for lasso regression

# Fit the final model on the training data w/ min lambda
model4.2 <- glmnet(new_train_data.x, 
                   new_train_data.y, 
                   alpha = 1, #alpha = 1 for lasso regression
                   lambda = model4.2.cv$lambda.min)

# MODEL 4.2: Validate model: (V, T, M, Y, R)
model4.2.validation <- model4.2 %>% predict(new_test_data.x) %>% as.vector()
model4.2.RMSE <- RMSE(new_test_data$AveragePrice, model4.2.validation)
model4.2.R2 <- R2(model4.2.validation, new_test_data$AveragePrice)

cat("Model 4.2 - Lasso Model: RMSE =", 
    model4.2.RMSE,
    "Model 4.2 - Lasso Model: R-squared =", 
    model4.2.R2)

# MODEL 4.2: Plot results
model4.2.plot1 <- build_validation_plot(new_test_data.all, 
                                        model4.2.validation, 
                                        "MODEL 4.2 - Lasso Regression",
                                        model4.2.RMSE,
                                        model4.2.R2)

########################################
# MODEL 4.3: Elastic Net Regression (V, T, M, Y, R)
# Build model using caret package
model4.3 <- train(AveragePrice ~., 
                  data = new_train_data.all, 
                  method = "glmnet",
                  trControl = trainControl("cv", number = 10),
                  tuneLength = 10)

# MODEL 4.3: Validate model: (V, T, M, Y, R)
model4.3.validation <- model4.3 %>% predict(new_test_data.xy) %>% as.vector()
model4.3.RMSE <- RMSE(new_test_data$AveragePrice, model4.3.validation)
model4.3.R2 <- R2(model4.3.validation, new_test_data$AveragePrice)

cat("Model 4.3 - Elastic Net Regression: RMSE =", 
    model4.3.RMSE,
    "Model 4.3 - Elastic Net Regression: R-squared =", 
    model4.3.R2)

# MODEL 4.3: Plot results
model4.3.plot1 <- build_validation_plot(new_test_data.all, 
                                        model4.3.validation, 
                                        "Model 4.3 - Elastic Net Regression",
                                        model4.3.RMSE,
                                        model4.3.R2)

########################################
# MODEL 5: Decision Tree (Regression) (V, T, M, Y, R)
# MODEL 5: Train model: (V, T, M, Y, R)
model5 <- rpart(AveragePrice ~ ., 
                data = new_train_data,
                method = "anova")

# Summary 
# from summary(model5)
cat("Predictor importance
     Type TotalVolume      Region       Month        Year 
     39          33          19           5           4 ")

# Build tree & cp
model5.plot1 <- rpart.plot(model5)
model5.plot2 <- plotcp(model5)

# MODEL 3: Validate model: (V, T, M, Y, R)
model5.validation <- model5 %>% predict(new_test_data) %>% as.vector()
model5.RMSE <- RMSE(new_test_data$AveragePrice, model5.validation)
model5.R2 <- R2(model5.validation, new_test_data$AveragePrice)

cat("Model 5 - Decision Tree (Regression): RMSE =", 
    model5.RMSE,
    "Model 5 - Decision Tree (Regression): R-squared =", 
    model5.R2)

# MODEL 5: Plot results
model5.plot3 <- build_validation_plot(new_test_data.all, 
                                      model5.validation, 
                                      "Model 5 - Decision Tree (Regression)",
                                      model5.RMSE,
                                      model5.R2)

##########################################################
##########################################################
# CONSLUSIONS
##########################################################
##########################################################

# Print final table with R-squared and RMSE values for all models
results <- data.frame(
  c("MODEL 1 - Mean Model", 
    "MODEL 2 - Linear Regression",
    "MODEL 3 - Multi Linear Regression", 
    "MODEL 4 - Lasso Regression",
    "MODEL 5 - Decision Tree (Regression)"),
  c(model1.RMSE, 
    model2.RMSE, 
    model3.RMSE,
    model4.2.RMSE,
    model5.RMSE),
  c(" N/A ", 
    round(model2.R2, 7), 
    round(model3.R2, 7),
    round(model4.2.R2, 7),
    round(model5.R2, 7)))

names(results) <- c("Models", "RMSE Value", "R-squared")

# Draw the table
results %>%
  kbl(caption = "Model Results: Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Print Model Results (plots + table summary)
# Display multi linear regression plots
model1.plot1
model2.plot1
model3.plot1
model4.2.plot1
model5.plot3

