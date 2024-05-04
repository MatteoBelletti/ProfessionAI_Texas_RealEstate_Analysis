install.packages("tidyverse")
install.packages("moments")
install.packages("ggplot2")
install.packages("gridExtra")

library(gridExtra)
library(moments)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Define a function to load the dataset
load.dataset <- function() {
  # Read the CSV file
  df <- read.csv("C:/Users/matte_cc6iqkl/Statistica/Statistica descrittiva/Realstate_texas/Datasource/realestate_texas.csv")
  
  return(df)  # Return the dataset
}

# Define a function to calculate the geometric mean
geometric.mean <- function(x) {
  # Check for negative values
  if (any(x < 0)) {
    stop("Geometric mean is not defined for negative numbers.")
  }
  # Avoid overflow and underflow by taking the logarithm and exponentiating
  return(exp(mean(log(x))))
}

# Define a function to get the mean price from a dataset
get_mean_price <- function(df){
  # Calculate the mean price
  summary <- df %>%
    summarise(mean_price = sum(volume) / sum(sales) * 1000)
  
  return(summary)  # Return the summary
}

# Define a function to get summary statistics for a variable in a dataset
get_summary <- function(df, variable) {
  # Calculate summary statistics
  summary <- df %>%
    summarise(mean = mean({{variable}}),
              median = median({{variable}}),
              min = min({{variable}}),
              max = max({{variable}}),
              iqr = IQR({{variable}}),
              range = max({{variable}}) - min({{variable}}),
              std_deviation = sd({{variable}}),
              skewness_fisher = skewness({{variable}}),
              cv = sd({{variable}}) / mean({{variable}}),
              n = n())
  
  return(summary)  # Return the summary
}




################################################################
# Common graph theme
################################################################
theme_roboto <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Roboto"),  
      axis.text.x = element_text(size = 8),    
      axis.text.y = element_text(size = 8), 
      title = element_text(size = 12),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8)
    )
}

################################################################
# Point 2 + common grouped dataset
################################################################

realestate_texas <- load.dataset()

realestate_texas <- realestate_texas %>%
  mutate(
    mean_price = (volume / sales) * 1000)
View(realestate_texas)

#summary data for dataset
sales_summary <- get_summary(realestate_texas, sales)
volume_summary <- get_summary(realestate_texas, volume)
months_inventory <- get_summary(realestate_texas, months_inventory)
listings_summary <- get_summary(realestate_texas, listings)
median_price_summary <- get_summary(realestate_texas, median_price)

#summary data for city
realestate_grouped_by_city <- realestate_texas %>% group_by(city)
sales_summary_by_city <- get_summary(realestate_grouped_by_city, sales)
volume_summary_by_city <- get_summary(realestate_grouped_by_city, volume)
months_inventory_summary_by_city <- get_summary(realestate_grouped_by_city, months_inventory)
listings_summary_by_city <- get_summary(realestate_grouped_by_city, listings)
median_price_summary_by_city <- get_summary(realestate_grouped_by_city, median_price)


#summary data for city and year
realestate_grouped_by_city_year <- realestate_texas %>% group_by(city, year)
sales_summary_by_city_year <- get_summary(realestate_grouped_by_city_year, sales)
volume_summary_by_city_year <- get_summary(realestate_grouped_by_city_year, volume)
months_inventory_summary_by_city_year <- get_summary(realestate_grouped_by_city_year, months_inventory)
listings_summary_by_city_year <- get_summary(realestate_grouped_by_city_year, listings)
median_price_summary_by_city_year <- get_summary(realestate_grouped_by_city_year, median_price)


# Create a bar plot of the "mean" variable for each city and each year for sales summary
graph_sales_city_year <- ggplot(sales_summary_by_city_year, aes(x = city, y = mean, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Sales per City by Year", x = "City", y = "Mean Sales", fill = "Year") +
  scale_fill_brewer(palette = "Set2") + # Use the Set2 palette from RColorBrewer
  theme_roboto()  # Apply the minimalist theme of ggplot2
# Display the plot
print(graph_sales_city_year)

# Create a bar plot of the "mean" variable for each city and each year for volume summary
graph_volume_city_year <- ggplot(volume_summary_by_city_year, aes(x = city, y = mean, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Volumes per City by Year", x = "City", y = "Mean Volumes", fill = "Year") +
  scale_fill_brewer(palette = "Set2") + # Use the Set2 palette from RColorBrewer
  theme_roboto()  # Apply the minimalist theme of ggplot2
# Display the plot
print(graph_volume_city_year)

# Create a bar plot of the "mean" variable for each city and each year for median price summary
graph_median_price_city_year <- ggplot(median_price_summary_by_city_year, aes(x = city, y = mean, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean price per City by Year", x = "City", y = "Mean Prices", fill = "Year") +
  scale_fill_brewer(palette = "Set2") + # Use the Set2 palette from RColorBrewer
  theme_roboto()  # Apply the minimalist theme of ggplot2
# Display the plot
print(graph_median_price_city_year)

# Create a bar plot of the "mean" variable for each city and each year for month inventory summary
graph_months_inventory_city_year <- ggplot(months_inventory_summary_by_city_year, aes(x = city, y = mean, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean months inventory by Year", x = "City", y = "Mean months inventory", fill = "Year") +
  scale_fill_brewer(palette = "Set2") + # Use the Set2 palette from RColorBrewer
  theme_roboto()  # Apply the minimalist theme of ggplot2
# Display the plot
print(graph_months_inventory_city_year)

# Create a bar plot of the "mean" variable for each city and each year for listing summary
graph_months_listings_city_year <- ggplot(listings_summary_by_city_year, aes(x = city, y = mean, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean listings by Year", x = "City", y = "Mean year listings", fill = "Year") +
  scale_fill_brewer(palette = "Set2") + # Use the Set2 palette from RColorBrewer
  theme_roboto()  # Apply the minimalist theme of ggplot2
# Display the plot
print(graph_months_listings_city_year)


################################################################
# Point 4
################################################################
# Load the real estate dataset for Texas
realestate_texas <- load.dataset()

# Create classes for volume
class_breaks <- seq(8, 100000, by = 10)  # Specify desired class intervals

# Divide the "volume" variable into classes using the cut() function
realestate_texas_summary <- realestate_texas %>%
  mutate(volume_range = cut(realestate_texas$volume, breaks = class_breaks, 
                            labels = paste(class_breaks[-length(class_breaks)], "-", class_breaks[-1]), 
                            include.lowest = TRUE))

# Calculate the absolute frequencies of the classes
absolute_freq <- realestate_texas_summary %>%
  count(volume_range) %>%
  rename(absolute_freq = n)

# Calculate the relative frequencies of the classes
total_obs <- nrow(realestate_texas_summary)
relative_freq_table <- absolute_freq %>%
  mutate(relative_freq = absolute_freq / total_obs) 

relative_freq_table <- relative_freq_table %>%
  mutate(cumulative_relative_freq = cumsum(relative_freq_table$relative_freq))

# Calculate cumulative absolute and relative frequencies
summary_table <- absolute_freq %>%
  mutate(
    relative_freq = relative_freq_table$relative_freq,
    cumulative_absolute_freq = cumsum(absolute_freq),
    cumulative_relative_freq = relative_freq_table$cumulative_relative_freq,
  )

# Print the resulting table
print(summary_table)

ineq(realestate_texas$volume, type = "Gini")




# Bar plot of absolute frequencies
absolute_plot <- ggplot(summary_table, aes(x = volume_range, y = absolute_freq, group = 1)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Absolute Frequencies",
       x = "Classes",
       y = "Absolute Frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot of relative frequencies
relative_plot <- ggplot(summary_table, aes(x = volume_range, y = relative_freq, group = 1)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(title = "Relative Frequencies",
       x = "Classes",
       y = "Relative Frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot of cumulative absolute frequencies
cumulative_absolute_plot <- ggplot(summary_table, aes(x = volume_range, y = cumulative_absolute_freq, group = 1)) +
  geom_bar(stat = "identity", fill = "bisque") +
  labs(title = "Cumulative Absolute Frequencies",
       x = "Classes",
       y = "Cumulative Absolute Frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot of cumulative relative frequencies
cumulative_relative_plot <- ggplot(summary_table, aes(x = volume_range, y = cumulative_relative_freq, group = 1)) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(title = "Cumulative Relative Frequencies",
       x = "Classes",
       y = "Cumulative Relative Frequencies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Draw the plots
grid.arrange(absolute_plot, relative_plot, cumulative_absolute_plot, cumulative_relative_plot,
             ncol = 2, nrow = 2)

# Plot of relative frequencies
relative_plot <- ggplot(summary_table, aes(x = reorder(volume_range, as.numeric(gsub(".*- (.*)", "\\1", volume_range))), y = relative_freq, group = 1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Relative Frequencies 2",
       x = "Classes",
       y = "Relative Frequencies") +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.05)) +  # Modify y-axis scale
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(relative_plot)




################################################################
# Point 7
################################################################
# Load the real estate dataset for Texas
realestate_texas <- load.dataset()

# Calculate the mean price by city
mean_price_by_city <- get_mean_price(realestate_grouped_by_city)
print(mean_price_by_city)

# Calculate the mean price by city and year
mean_price_by_city_year <- get_mean_price(realestate_grouped_by_city_year)
print(mean_price_by_city_year)

# Calculate the mean price by city, year, and month
mean_price_by_city_year_month <- get_mean_price(realestate_grouped_by_city_month)
print(mean_price_by_city_year_month)


################################################################
# Point 8
################################################################
# Load the real estate dataset for Texas
realestate_texas <- load.dataset()

# Calculate efficiency of listings by dividing sales by listings
realestate_texas$efficiency_listing <- realestate_texas$sales / realestate_texas$listings 

# Convert year and month columns to a Date format
realestate_texas$date <- as.Date(paste(realestate_texas$year, realestate_texas$month, "01", sep = "-"))

# Arrange the dataset by city and date
realestate_texas <- arrange(realestate_texas, city, date)

# Find the minimum and maximum dates in the dataset
min_date <- min(realestate_texas$date)
max_date <- max(realestate_texas$date)

# Create a line plot to visualize the efficiency of listings over time by city
ggplot(realestate_texas, aes(x = date, y = efficiency_listing, group = city, color = city)) +
  geom_line() +  # Add lines connecting data points
  geom_point() +  # Add points for each data point
  labs(x = "Month/Year", y = "Efficiency listing", title = "Efficiency of listings by City and Month/Year Period") +
  scale_x_date(date_labels = "%m-%Y", breaks = seq(min_date, max_date, by = "1 month")) +  # Set x-axis labels
  theme_roboto() +  # Apply a theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")  # Adjust x-axis text angle and legend position


################################################################
# Point 9
# summary data for city and month
################################################################

# Load the real estate dataset for Texas
realestate_texas <- load.dataset()

# Group the dataset by city and month
realestate_grouped_by_city_month <- realestate_texas %>% group_by(city, month)

# Obtain summary statistics for sales, volume, months inventory, listings, and median price by city and month
sales_summary_by_city_month <- get_summary(realestate_grouped_by_city_month, sales)
volume_summary_by_city_month <- get_summary(realestate_grouped_by_city_month, volume)
months_inventory_summary_by_city_month <- get_summary(realestate_grouped_by_city_month, months_inventory)
listings_summary_by_city_month <- get_summary(realestate_grouped_by_city_month, listings)
median_price_summary_by_city_month <- get_summary(realestate_grouped_by_city_month, median_price)

# Print the summary statistics for sales, volume, months inventory, listings, and median price by city and month
print(sales_summary_by_city_month, n =1000)
print(volume_summary_by_city_month, n =1000)
print(months_inventory_summary_by_city_month, n =1000)
print(listings_summary_by_city_month, n =1000)
print(median_price_summary_by_city_month, n =1000)

################################################################
################################################################
################################################################
##
## Graph part
## 
################################################################
################################################################
################################################################
################################################################
# Point 1
################################################################

# Load the real estate dataset for Texas
realestate_texas <- load.dataset()

# Create a boxplot comparing the median price distribution by city
boxplot_median_price_city <- ggplot(realestate_texas, aes(x = as.factor(city), y = median_price)) +
  geom_boxplot() +
  labs(title = "Median price distribution by city",
       x = "City",
       y = "Median price distribution")+
  theme_roboto() 

# Print the boxplot for median price distribution by city
print(boxplot_median_price_city)



################################################################
# Point 2
################################################################

# Load the real estate dataset for Texas
realestate_texas <- load.dataset()

# Create a boxplot comparing the median price by city and year
boxplot_median_price_city_year <- ggplot(realestate_texas, aes(x = city, y = median_price, fill = as.factor(year))) +
  geom_boxplot() +
  labs(x = "City", y = "Median Price", title = "Median Price Comparison by City and Year") +
  theme_roboto() +
  scale_fill_manual(values = c("#90fcf9", "#63b4d1", "#7699d4", "#9448bc", "#480355"))

# Print the boxplot for median price by city and year
print(boxplot_median_price_city_year)

# Load the real estate dataset for Texas
realestate_texas <- load.dataset()

# Create a boxplot comparing sales by city and year
boxplot_sales_city_year <- ggplot(realestate_texas, aes(x = city, y = sales, fill = as.factor(year))) +
  geom_boxplot() +
  labs(x = "City", y = "Sales", title = "Sales Comparison by City and Year") +
  theme_roboto() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_manual(values = c("#90fcf9", "#63b4d1", "#7699d4", "#9448bc", "#480355"))

# Print the boxplot for sales by city and year
print(boxplot_sales_city_year)


################################################################
# Point 3 a)
################################################################

# Load the real estate dataset for Texas
realestate_texas <- load.dataset()

# Convert 'year' column to a factor
realestate_texas$year <- as.factor(realestate_texas$year)

# Convert the 'month' column to a factor with month labels
realestate_texas$month <- 
  factor(realestate_texas$month, 
         labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  )

# Create the grouped column bar plot for total sales
geom_col_sales <- ggplot(realestate_texas, aes(x = month, y = sales, fill = city)) +
  geom_col() +
  facet_wrap(~year, scales = "free_x") +
  labs(x = "Month", y = "Total sales", title = "Total sales comparison by City and Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the grouped column bar plot
print(geom_col_sales)


################################################################
# Point 3 b)
################################################################

# Load the real estate dataset for Texas
realestate_texas <- load.dataset()

# Convert the 'month' column to a factor with month labels
realestate_texas$month <- 
  factor(realestate_texas$month, 
         labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  )

# Calculate total sales for each year and month
df_totals <- aggregate(sales ~ year + month, data = realestate_texas, FUN = sum)

# Merge with the original dataframe to obtain normalized sales
realestate_texas <- merge(realestate_texas, df_totals, by = c("year", "month"), suffixes = c("", "_total"))
realestate_texas$normalized_sales <- realestate_texas$sales / realestate_texas$sales_total

# Create the normalized stacked bar plot
geom_bar_sales_normalized <- ggplot(realestate_texas, aes(x = month, y = normalized_sales, fill = city)) +
  geom_col(position = "fill") +
  facet_wrap(~year, scales = "free_x") +
  labs(x = "Month", y = "Normalized Sales", title = "Normalized Sales Comparison by Month and City") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

print(geom_bar_sales_normalized)




################################################################
# Point 4
################################################################
# Load the dataset containing real estate data for Texas
realestate_texas <- load.dataset()

# Convert the 'year' and 'month' columns to Date format and create a new 'date' column
realestate_texas$date <- as.Date(paste(realestate_texas$year, realestate_texas$month, "01", sep = "-"))

# Arrange the dataset by city and date
realestate_texas <- arrange(realestate_texas, city, date)

# Determine the minimum and maximum dates in the dataset
min_date <- min(realestate_texas$date)
max_date <- max(realestate_texas$date)

# Create a line plot to compare volume by city and month/year period
ggplot(realestate_texas, aes(x = as.factor(date), y = volume, color = city, group = city)) +
  geom_line() +
  geom_point() +
  labs(x = "Month/Year", y = "Volume", title = "Volume Comparison by City and Month/Year Period") +
  scale_x_date(date_labels = "%m-%Y", breaks = seq(min_date, max_date, by = "1 month")) +
  theme_roboto() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

# Create a line plot to compare sales by city and month/year period
ggplot(realestate_texas, aes(x = as.factor(date), y = sales, color = city, group = city)) +
  geom_line() +
  geom_point() +
  labs(x = "Month/Year", y = "Sales", title = "Sales Comparison by City and Month/Year Period") +
  scale_x_date(date_labels = "%m-%Y", breaks = seq(min_date, max_date, by = "1 month")) +
  theme_roboto() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")



