getwd()
wrf_data <-read.csv('WRFdata_May2018.csv')
str(wrf_data)
View(wrf_data)
nrow(wrf_data)
ncol(wrf_data)
tail(names(wrf_data),30)


#replacing last missing date and time value
library(lubridate)
library(tidyverse)
# Function to increment the datetime by 3 hours
increment_time <- function(time_value) {
  time_format <- "%d.%m.%Y.%H.%M"
  datetime <- as.POSIXct(time_value, format = time_format)
  new_datetime <- datetime + 3 * 3600 # 3 hours in seconds
  return(format(new_datetime, time_format))
}

# Loop over the column names
for (i in seq(3, length(names(wrf_data)), by = 10)) {
  current_datetime <- gsub("X", "", names(wrf_data[i])) # Remove the 'X' to get the datetime
  expected_datetime <- paste0("X", increment_time(current_datetime))
  
  # If the next expected datetime column name does not match, replace it
  if (i + 10 <= length(names(wrf_data)) && names(wrf_data)[i + 10] != expected_datetime) {
    names(wrf_data)[i + 10] <- expected_datetime
  }
}

# Verify the change
tail(names(wrf_data),30)


# replacing the pattern found with NA
pattern_to_replace <- "[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
wrf_data[wrf_data == pattern_to_replace] <- NA


#replacing NA values in parameters row(2nd row)
#checking NA values before 
sum(is.na(wrf_data[1, ])) 

pattern <- c("TSK", "PSFC", "\"U10\"", "\"V10\"", "\"Q2\"", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS")
# Function to replace NA values in the parameters row 
replace_NA_with_pattern <- function(row, pattern) {
  # loop for pattern starting from the third column
  for (i in 3:length(row)) {
    if (is.na(row[i]) || row[i] == "NA") {
      # Finding correct position in the pattern and replace "NA"
      pattern_index <- ((i - 3) %% length(pattern)) + 1
      row[i] <- pattern[pattern_index]
    }
  }
  return(row)
}
# Apply the function to parameters row 
wrf_data[1, ] <- replace_NA_with_pattern(wrf_data[1, ], pattern)
#checking NA values after
sum(is.na(wrf_data[1, ])) 

View(wrf_data)




#RESHAPING DATA
library(dplyr)
library(lubridate)
library(purrr)  #  map functions

# Extracting  datetime values 
datetime_values <- names(wrf_data)[grepl("^X\\d{2}\\.\\d{2}\\.\\d{4}\\.\\d{2}\\.\\d{2}$", names(wrf_data))]
parsed_datetimes <- datetime_values %>%
  map_chr(~ format(lubridate::dmy_hm(str_replace_all(.x, "X", "")), "%Y-%m-%d %H:%M:%S")) %>%
  ymd_hms()  # Convert to POSIXct datetime format

# Set column names based on the first row (paramters row)
colnames(wrf_data) <- as.character(unlist(wrf_data[1, ]))
# Remove the first row as it is the header
data <- wrf_data[-1, ]

# Convert columns to their appropriate data types
data[] <- lapply(data, function(x) {
  numeric_attempt <- as.numeric(as.character(x))
  if (all(is.na(numeric_attempt) == is.na(x))) {
    return(numeric_attempt)
  } else {
    return(x)
  }
})

# Filter rows for specific location
specific_wrf_location <- subset(data, XLAT == 55.035 & XLONG == -1.836)

# Remove the 'XLAT' and 'XLONG' columns
specific_wrf_location <- specific_wrf_location[, !(names(specific_wrf_location) %in% c("XLAT", "XLONG"))]

# Split data into groups of 10 columns
wrf_columns <- lapply(split.default(specific_wrf_location, rep(1:ceiling(ncol(specific_wrf_location)/10), each=10, length.out=ncol(specific_wrf_location))), identity)

# Define the column names
column_names <- c("TSK", "PSFC", "U10", "V10", "Q2", "RAINC", "RAINNC", "SNOW", "TSLB", "SMOIS")

# Combine the data frames and add DateTime column
final_data <- do.call(rbind, lapply(seq_along(wrf_columns), function(i) {
  new_data <- wrf_columns[[i]]
  colnames(new_data) <- column_names[1:ncol(new_data)]
  new_data$DATETIME <- parsed_datetimes[i]
  return(new_data)
}))

# Reset row names(index) starting from 1
rownames(final_data) <- NULL
# View the final dataframe
View(final_data)
str(final_data)
summary(final_data)

#visulisation of NA values before interpolation
library(ggplot2)

# Calculate sum of NAs for each column
na_counts <- sapply(final_data, function(x) sum(is.na(x)))
# Create data frame for plotting
na_data <- data.frame(Column = names(na_counts), NA_Count = na_counts)
# Plot bar chart
ggplot(na_data, aes(x = Column, y = NA_Count)) +
  geom_bar(stat = "identity", fill = "green") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # This rotates the x-axis labels for better readability
  labs(title = "Sum of NA Values per Column", x = "Column", y = "Number of NAs")

#checking for missing values in selected dataset
sum(is.na(final_data)) 
colSums(is.na(final_data))

#interpolate missing value
install.packages("zoo")
library(zoo)

final_data_interpolated <- final_data
# Loop over the columns
for (column_name in names(final_data)) {
  # Skip interpolation for non-numeric columns
  if (!is.numeric(final_data[[column_name]])) next
    final_data_interpolated[[column_name]] <- na.approx(final_data[[column_name]], na.rm = FALSE)
}

# View the data frame with interpolated values
View(final_data_interpolated)

#checking missing values after interpolation
colSums(is.na(final_data_interpolated))


#detect outlier using IQR and boxplot
install.packages("outliers")
library(outliers)
library(ggplot2)

# numeric columns 
df_numeric <- final_data_interpolated[sapply(final_data_interpolated, is.numeric)]

# Prepare data for ggplot
df_long <- pivot_longer(df_numeric, cols = everything())

# Plot using ggplot2 with facet_wrap
ggplot(df_long, aes(x = factor(1), y = value)) +
  geom_boxplot(fill = "lightblue", colour = "darkblue", alpha = 0.7) +
  facet_wrap(~name, scales = "free_y") +
  labs(title = "Boxplots for All Numeric Columns", x = "", y = "Values") +
  theme_minimal() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())  




#IQR
detect_outliers_iqr <- function(data) {
  outliers_list <- list()
  
  for (column in names(data)) {
    if (is.numeric(data[[column]])) {
      # Calculate IQR
      Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      # Define bounds for outliers
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      # Identify outliers
      outliers <- which(data[[column]] < lower_bound | data[[column]] > upper_bound)
      outliers_list[[column]] <- outliers
    }
  }
  return(outliers_list)
}

outliers_detected <- detect_outliers_iqr(final_data_interpolated)
print(outliers_detected)

#handling outliers with Winzorization
# install.packages("DescTools")
# library(DescTools)
# handle_outliers_winsorize <- function(data, lower_perc = 0.05, upper_perc = 0.95) {
#   for (column in names(data)) {
#     if (is.numeric(data[[column]])) {
#       # Apply winsorization
#       data[[column]] <- Winsorize(data[[column]], probs = c(lower_perc, upper_perc), na.rm = TRUE)
#     }
#   }
#   return(data)
# }
# # Apply the function to the dataset
# df_winsorized <- handle_outliers_winsorize(final_data_interpolated)
# View(df_winsorized)
# sum(is.na(df_winsorized))

# Calculate Wind Speed
final_data_interpolated$WINDSPEED <- sqrt(final_data_interpolated$U10^2 + final_data_interpolated$V10^2)
View(final_data_interpolated)

# Summary to compare raw data and clean(interpolated) data
summary_raw <- summary(final_data)
summary_cleaned <- summary(final_data_interpolated)
print(summary_raw)
print(summary_cleaned)

# Function to plot histograms for all numeric columns
plot_histograms <- function(final_data_interpolated) {
  # Filter numeric columns
  numeric_cols <- final_data_interpolated[, sapply(final_data_interpolated, is.numeric)]
  # Get the number of columns
  num_cols <- ncol(numeric_cols)
  # Create a list to store plots
  plots_list <- vector("list", length = num_cols)
  # Plot histograms for each numeric column
  for (i in 1:num_cols) {
    col_name <- names(numeric_cols)[i]
    plots_list[[i]] <- ggplot(data = numeric_cols, aes(x = .data[[col_name]])) +
      geom_histogram(fill = "purple", color = "black") +
      labs(title = paste("Histogram of", col_name), x = col_name, y = "Frequency")
  }
  
  # Arrange plots in a grid
  plot_grid <- cowplot::plot_grid(plotlist = plots_list, nrow = 3)  
  # Return the combined plot
  return(plot_grid)
}
# Call the function to plot histograms for all numeric columns
plot_histograms(final_data_interpolated)

#choosing subset of parameters to be used furthure on basis of the histogram frequency
subset_wrf_data <-  final_data_interpolated[c("DATETIME", "TSK", "PSFC","Q2","TSLB","SMOIS","WINDSPEED")]
View(subset_wrf_data)

#downloading reshaped, subset and clean data to my working directory
#write.csv(subset_wrf_data, file = "finalwrfdata.csv",row.names = FALSE)



#subset_wrf_data <- read.csv("finalwrfdata.csv")
#UNIVARIATE ANALYSIS
library(ggplot2)
library(cowplot)

# Function to plot histogram with normal curve for each numeric variable and arrange them in a grid
plot_histograms_with_normal_curve <- function(subset_wrf_data) {
  # Filter numeric columns
  numeric_cols <- subset_wrf_data[, sapply(subset_wrf_data, is.numeric)]
  # Get the number of columns
  num_cols <- ncol(numeric_cols)
  # Create a list to store plots
  plots_list <- vector("list", length = num_cols)
  # Plot histograms with normal curves for each numeric column
  for (i in 1:num_cols) {
    col_name <- names(numeric_cols)[i]
    mean_var <- mean(numeric_cols[[col_name]], na.rm = TRUE)
    sd_var <- sd(numeric_cols[[col_name]], na.rm = TRUE)
    plots_list[[i]] <- ggplot(numeric_cols, aes(x = .data[[col_name]])) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
      stat_function(fun = dnorm, args = list(mean = mean_var, sd = sd_var), color = "red", size = 1) +
      labs(title = paste("Distribution of", col_name), x = col_name, y = "Density")
  }
  # Arrange plots in a grid, you can adjust 'nrow' to better fit your display or number of variables
  plot_grid <- cowplot::plot_grid(plotlist = plots_list, nrow = 3)  
  # Return the combined plot
  return(plot_grid)
}
# Apply function to your data frame
plot_histograms_with_normal_curve(subset_wrf_data)


#NORMALITY TEST USING SHAPIRO TEST
# Function to perform Shapiro-Wilk test
test_normality <- function(subset_wrf_data, variable) {
  if (is.numeric(subset_wrf_data[[variable]])) {
    test_result <- shapiro.test(subset_wrf_data[[variable]][!is.na(subset_wrf_data[[variable]])])
    cat("Shapiro-Wilk test for", variable, ":\n W =", test_result$statistic, "p-value =", test_result$p.value, "\n\n")
  }
}

# Apply the normality test function to each numeric variable
if (is.data.frame(subset_wrf_data)) {
  for (variable in names(subset_wrf_data)) {
    test_normality(subset_wrf_data, variable)
  }
}
#in the result of shapiro and histogram curve\
#not normal(significantly deviates from a normal distribution)
#p-value less than 0.05 indicates significant deviation from normality.
# TSK: Not normal
# PSFC: Not normal
# Q2: Normal
# TSLB: Normal
# SMOIS: Not normal
# WINDSPEED: Not normal

#statistics of columns
library(e1071)

calculate_statistics <- function(subset_wrf_data) {
  # Define an empty data frame with the correct structure
  results <- data.frame(Variable = character(),
                        Mean = numeric(),
                        Median = numeric(),
                        Mode = numeric(),
                        Range = I(list()),
                        Variance = numeric(),
                        SD = numeric(),
                        IQR = numeric(),
                        Skewness = numeric(),
                        Kurtosis = numeric(),
                        stringsAsFactors = FALSE)
  
  mode_value <- function(x) {
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }
  
# Calculate the statistics for each variable
for (variable in names(subset_wrf_data)) {
    if (is.numeric(subset_wrf_data[[variable]])) {
      this_data <- na.omit(subset_wrf_data[[variable]])
      
      results <- rbind(results, data.frame(
        Variable = variable,
        Mean = mean(this_data),
        Median = median(this_data),
        Mode = mode_value(this_data),
        Range = I(list(range(this_data))),
        Variance = var(this_data),
        SD = sd(this_data),
        IQR = IQR(this_data),
        Skewness = skewness(this_data),
        Kurtosis = kurtosis(this_data)
      ))
    }
  }
  return(results)
}
stats_results <- calculate_statistics(subset_wrf_data)
View(stats_results)


#BIVARIATE ANALYSIS

#Q1 TSK VS TSLB (data not normally distributed for TSK, TSLB is normally distributed)

# Step 1: Scatter Plot
library(ggplot2)
ggplot(subset_wrf_data, aes(x = TSK, y = TSLB)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Scatter Plot of TSK vs TSLB", x = "Surface Temperature (TSK)", y = "Soil Temperature(TSLB")

# Step 2: Correlation Coefficient
spearman_cor <- cor.test(subset_wrf_data$TSK, subset_wrf_data$TSLB, method = "spearman")
print(spearman_cor)

install.packages("Hmisc")
library(Hmisc)
rcorr(subset_wrf_data$TSK, subset_wrf_data$TSLB, type = "spearman")

# Strong positive correlation , one increase other increase, same direction


#Q2 CORRELATION between  soil moisture, soil temperature (soil temeprature- normally distributed , smois- not normal)

# Step 1: Scatter Plot
ggplot(subset_wrf_data, aes(x = SMOIS, y = TSLB)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Scatter Plot of SMOIS vs TSLB", x = "Soil Moisture (SMOIS)", y = "Soil Temperature(TSLB")

# Step 2: Correlation Coefficient
kendall_cor <- cor.test(subset_wrf_data$SMOIS, subset_wrf_data$TSLB, method = "kendall")
print(kendall_cor)


# Strong postive corelation , one increasse other increase, same direction


#Q3 humidity and soil moisture (humidity- normal, soil moisture- not normal )
# Step 1: Scatter Plot
ggplot(subset_wrf_data, aes(x = SMOIS, y = Q2)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Scatter Plot of SMOIS vs Q2", x = "Humidity (Q2)", y = "Soil Temperature(TSLB")

# Step 2: Correlation Coefficient
spearman_corr <- cor.test(subset_wrf_data$SMOIS, subset_wrf_data$Q2, method = "spearman")
print(spearman_corr)


#MULTIVARIATE ANALYSIS

#CORRELATION MATRIX
library(reshape2)
# Filter numeric columns
numeric_data <- subset_wrf_data[, sapply(subset_wrf_data, is.numeric)]

cor_matrix <- cor(numeric_data,method="spearman")
# Reshape the correlation matrix into a long format
melted_cor_matrix <- melt(cor_matrix)
#Generate the heatmap
ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1), 
        axis.text.y = element_text(angle = 45, vjust = 1)) +
  coord_fixed()

#subset_wrf_data<-read.csv("finalwrfdata.csv")
#MACHINE LEARNING
library(caret)      # for model training
library(randomForest) # for random forest algorithm
library(e1071)      # for support vector regression
library(rpart)      # for decision trees
library(ggplot2)    # for data visualization
library(gridExtra)  # for arranging plots
library(forecast)   #accuracy function for metrics

#splitting into test and train
install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(subset_wrf_data$TSLB, SplitRatio = 0.8)
train_data <- subset(subset_wrf_data, split == TRUE)
test_data<- subset(subset_wrf_data, split== FALSE)

View(train_data)
View(test_data)
nrow(train_data)
nrow(test_data)


#Q4
#correlation between variables
cor(train_data[, c("TSLB", "SMOIS", "TSK", "Q2")])

# 1. Multiple Linear Regression
mlr_model <- lm(TSLB ~ SMOIS + TSK + Q2, data = train_data)
summary(mlr_model)



#linearity test
library(lmtest)
raintest(mlr_model)#linear relation

# Checking  multicollinearity
library(car)
vif(mlr_model)  # Variance Inflation Factor

# Checking residuals' normality
plot(mlr_model, which = 2)  # Normal Q-Q plot
shapiro.test(resid(mlr_model))
 
# Checking for homoscedasticity and independence
# Residuals vs Fitted values plot
bptest(mlr_model)  # Breusch-Pagan test for homoscedasticity
dwtest(mlr_model)  # Durbin-Watson test for autocorrelation

# Predicting on test data
mlr_predictions <- predict(mlr_model, newdata = test_data)

# Evaluating the model
mlr_acc <- accuracy(mlr_predictions, test_data$TSLB)
mlr_acc

# 2.Random Forest
rf_mod <- randomForest(TSLB ~ SMOIS + TSK + Q2, data = train_data, ntree = 500)
plot(rf_mod)
rf_predict <- predict(rf_mod, test_data)
rf_predict
rf_acc <- accuracy(rf_predict, test_data$TSLB)
rf_acc


# 3.SVR with scaled data
train_data_scale <- scale(train_data[, c("SMOIS", "TSK", "Q2")])
train_scale <- as.data.frame(train_data_scale)
train_scale$TSLB <- train_data$TSLB 

test_data_scale <- scale(test_data[, c("SMOIS", "TSK", "Q2")])
test_scale <- as.data.frame(test_data_scale)
test_scale$TSLB <- test_data$TSLB 

svr_mod <- svm(TSLB ~ SMOIS + TSK + Q2, data = train_scale, type = 'eps-regression', kernel = 'linear')
svr_predict <- predict(svr_mod, test_scale)
svr_acc <- accuracy(svr_predict, test_data$TSLB)
svr_acc

#4. Decision Tree
dt_model <- rpart(TSLB ~ SMOIS + TSK + Q2, data = train_data, 
                  control = rpart.control(minsplit = 10, cp=0.01))
dt_pred <- predict(dt_model, test_data)
dt_acc <- accuracy(dt_pred, test_data$TSLB)
dt_acc
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(dt_model)
# Define file name and resolution settings
png(filename = "decision_tree.png", width = 1600, height = 1200, res = 300)

# Plot the decision tree
rpart.plot(dt_model)



# Visualisation
library(tidyr)
library(ggplot2)
# Create a dataframe with actual values and predictions from each model
results <- data.frame(
  Actual = test_data$TSLB,
  MLR_Predictions = mlr_predictions,
  RF_Predictions = rf_predict,
  SVR_Predictions = svr_predict,
  DT_Predictions=dt_pred)

View(results)

#reshaping for plot
results_long <- pivot_longer(results, cols = -Actual, names_to = "Model", values_to = "Predicted")


ggplot(results_long, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha=0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Comparison of Actual vs. Predicted Values",
       x = "Actual TSLB Values",
       y = "Predicted TSLB Values") +
  theme_minimal() +
  scale_color_manual(values = c("purple", "red", "green", "blue")) +
  theme(legend.title = element_blank())


# Extract RMSE values
dt_rmse <- dt_acc[1, "RMSE"]
mlr_rmse <- mlr_acc[1, "RMSE"]
rf_rmse <- rf_acc[1, "RMSE"]
svr_rmse <- svr_acc[1, "RMSE"]
# Create a data frame for ggplot
data <- data.frame(Model= c("SVR_MODEL","MLR_MODEL","DT_MODEL","RF_MODEL"), 
                   RMSE=c(svr_rmse,mlr_rmse,dt_rmse,rf_rmse))

# ggplot2 bar plot
ggplot(data, aes(x=Model, y=RMSE, fill=Model)) +
  geom_bar(stat="identity", color="black") +
  labs(title="RMSE of Prediction Models", x="Model", y="RMSE") +
  theme_minimal() +
  scale_fill_brewer(palette="Pastel1")


#extraquestion #non linear relation 
#correlation between variables
cor(train_data[, c("TSLB", "SMOIS", "TSK", "WINDSPEED")])
#1. Random Forest
rf_model <- randomForest(SMOIS ~ TSLB + TSK + WINDSPEED, data = train_data, ntree = 500)
rf_pred <- predict(rf_model, test_data)
rf_metric <- accuracy(rf_pred, test_data$SMOIS)
rf_metric
plot(rf_model)

#2. Decision Tree
dt_model <- rpart(SMOIS ~ TSLB + TSK + WINDSPEED, data = train_data, control = rpart.control(minsplit = 1))
dt_pred <- predict(dt_model, test_data)
dt_metric <- accuracy(dt_pred, test_data$SMOIS)
dt_metric

#3. SVR with scaled data
train_data_scaled <- scale(train_data[, c("SMOIS","TSLB", "TSK", "WINDSPEED")])
test_data_scaled <- scale(test_data[, c("SMOIS","TSLB", "TSK", "WINDSPEED")])
svr_model <- svm(SMOIS ~ TSLB + TSK + WINDSPEED , 
                 data = as.data.frame(train_data_scaled), type = 'eps-regression', kernel = 'radial')

svr_pred <- predict(svr_model, as.data.frame(test_data_scaled))
svr_metric <- accuracy(svr_pred, test_data$SMOIS)
svr_metric



# Visualisation
library(tidyr)
library(ggplot2)
# Create a dataframe with actual values and predictions from each model
result <- data.frame(
  Actual = test_data$SMOIS,
  DT_Predictions = dt_pred,
  RF_Predictions = rf_pred,
  SVR_Predictions = svr_pred
)
#reshaping for plot
result_long <- pivot_longer(result, cols = -Actual, names_to = "Model", values_to = "Predicted")

ggplot(result_long, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha=0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Comparison of Actual vs. Predicted Values",
       x = "Actual SMOIS Values",
       y = "Predicted SMOIS Values") +
  theme_minimal() +
  scale_color_manual(values = c("green", "purple", "blue")) +
  theme(legend.title = element_blank())



#TIMESERIES
#Q5 applying arima
library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(xts)

#subset for timeseries
ts_SMOIS <- subset_wrf_data[c("DATETIME", "SMOIS")]
View(ts_SMOIS)
class(ts_SMOIS)

#checking for seasonality
seasonal_plot<-ts(ts_SMOIS$SMOIS, frequency = 8)
plot(decompose(seasonal_plot))

#converting to timeseries
ts_SMOIS_data<-xts(ts_SMOIS$SMOIS, order.by=ts_SMOIS$DATETIME)
str(ts_SMOIS_data)
print(ts_SMOIS_data)
plot(ts_SMOIS_data)

#stationarity test
adf.test(ts_SMOIS_data) #non-stationary

diff_ts_SMOIS<-diff(ts_SMOIS_data)
clean_SMOIS_data<-na.aggregate(diff_ts_SMOIS, FUN=mean,na.rm=TRUE)

adf.test(clean_SMOIS_data)

# Outlier detection
tsoutliers(clean_SMOIS_data)

#spliting
train_end <- floor(0.8*length(clean_SMOIS_data))
train_SMOIS_data<-clean_SMOIS_data[1:train_end]
test_SMOIS_data<- clean_SMOIS_data[(train_end+1):length(clean_SMOIS_data)]
View(train_SMOIS_data)
View(test_SMOIS_data)

#plot
par(mfrow=c(2,1))
plot(train_SMOIS_data, main="Training Data", col="blue")
plot(test_SMOIS_data, main="Testing Data", col="red") 

#seasonal auto arima 
arima_model<- auto.arima(train_SMOIS_data, seasonal=TRUE)
summary(arima_model)

#MANUAL ARIMA MODEL
# Plotting ACF and PACF
par(mfrow=c(2,1))  # Organize plots in 2 rows, 1 column
acf(train_SMOIS_data, main="Autocorrelation Function")
pacf(train_SMOIS_data, main="Partial Autocorrelation Function")

Arima(train_SMOIS_data, order = c(0, 1, 1), seasonal=c(0,1,1,8))
Arima(train_SMOIS_data, order = c(0, 1, 1), seasonal=c(1,1,0,8))
Arima(train_SMOIS_data, order = c(0, 1, 1), seasonal=c(1,1,1,8))
Arima(train_SMOIS_data, order = c(1, 1, 0), seasonal=c(0,1,1,8))
Arima(train_SMOIS_data, order = c(1, 1, 0), seasonal=c(1,1,0,8))
Arima(train_SMOIS_data, order = c(1, 1, 0), seasonal=c(1,1,1,8))
Arima(train_SMOIS_data, order = c(1, 1, 1), seasonal=c(1,1,1,8))
Arima(train_SMOIS_data, order = c(1, 1, 1), seasonal=c(1,1,0,8))
Arima(train_SMOIS_data, order = c(0, 1, 0), seasonal=c(0,1,1,8))
Arima(train_SMOIS_data, order = c(0, 1, 0), seasonal=c(1,1,0,8))
Arima(train_SMOIS_data, order = c(0, 1, 0), seasonal=c(1,1,1,8))
Arima(train_SMOIS_data, order = c(2, 1, 0), seasonal=c(1,1,1,8))


SMOIS_model <- Arima(train_SMOIS_data, order = c(0, 1, 0), seasonal=TRUE)#less AICC
summary(SMOIS_model)



# Forecast using the ARIMA model
arima_forecast<- forecast(SMOIS_model,h=length(test_SMOIS_data))
accuracy(arima_forecast,test_SMOIS_data)
print(arima_forecast)

# Plot the forecast against the actual test data
plot(arima_forecast, main="SARIMA Forecast vs Actual Test Data")
# Convert forecast to a data frame
forecast_df <- data.frame(
  Time = index(test_SMOIS_data),  
  Forecast = arima_forecast$mean)

# Convert actual data to a data frame
actual_df <- data.frame(
  Time = index(test_SMOIS_data),
  Actual = test_SMOIS_data)

# Combine the data for plotting
combined_data <- merge(forecast_df, actual_df, by = "Time", all = TRUE)

# Create ggplot
ggplot(data = combined_data, aes(x = Time)) +
  geom_line(aes(y = Forecast, colour = "Forecast")) +
  geom_point(aes(y = Actual, colour = "Actual")) +
  scale_colour_manual(values = c("Forecast" = "red", "Actual" = "blue")) +
  labs(title = "Comparison of SARIMA Forecast and Actual Data",
       x = "Time", y = "SMOIS Value", colour = "Legend") +
  theme_minimal()


#linear regression
#creating time object
time <- 1:length(train_SMOIS_data)
time

#model
ts_linear <- lm(train_SMOIS_data ~ time)
summary(ts_linear)


#forecast
ts_test <- (length(train_SMOIS_data)+1):(length(train_SMOIS_data)+length(test_SMOIS_data))
ts_linear_forecast<- predict(ts_linear, newdata=data.frame(time=ts_test))
ts_linear_forecast
accuracy(ts_linear_forecast, test_SMOIS_data)
plot(ts_linear_forecast)

#actual vs predicted plot
# Converting test data and forecast into data frames for plotting
test_data_df <- data.frame(time = ts_test, value = test_SMOIS_data, type = "Actual")
forecast_data_df <- data.frame(time = ts_test, value = ts_linear_forecast, type = "Forecast")

# Combining the actual and forecast data
combined_data <- rbind(test_data_df, forecast_data_df)

# Plotting
ggplot(data = combined_data, aes(x = time, y = value, color = type)) +
  geom_line() +
  labs(title = "Actual vs Forecasted Values", x = "Time", y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "purple", "Forecast" = "red"))





