data<- region_04
threshold <- 0.6 * nrow(data)  # 60% of total rows
data_cleaned <- data[, colSums(is.na(data)) <= threshold]

colSums(is.na(data_cleaned))  # Check remaining missing values per column
dim(data_cleaned)             # Check new dimensions of the dataset

duplicates <- duplicated(data)
sum(duplicates)  # Count number of duplicate rows

install.packages("ggcorrplot")
library(ggcorrplot)  # For better visualization
numeric_data <- data[sapply(data, is.numeric)]
# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
ggcorrplot(cor_matrix, lab = FALSE, colors = c("red", "white", "blue"))



install.packages("zoo")
library(zoo)
numeric_data <- data[sapply(data, is.numeric)]
numeric_data <- data.frame(lapply(numeric_data, function(x) na.locf(x, na.rm = FALSE)))



numeric_data <- numeric_data[sapply(numeric_data, is.numeric)]
cols_with_negatives <- sapply(numeric_data, function(x) any(x < 0, na.rm = TRUE))
names(numeric_data)[cols_with_negatives]
# Drop columns with negative values
numeric_data_cleaned <- numeric_data[, !cols_with_negatives]
colnames(numeric_data_cleaned)



library(dplyr)
grouped_data <- data %>%
  group_by(iyear, success) %>%
  summarise(count = n(), .groups = 'drop')

success_data <- grouped_data %>% filter(success == 1)

library(ggplot2)

ggplot(success_data, aes(x = iyear, y = count)) +
  geom_line(aes(color = "success"), linewidth = 1, alpha = 0.7) +  # Updated for line width
  geom_point(aes(color = "success"), size = 3, alpha = 0.7) +
  labs(
    title = "Trend of Successful Terror Attacks by Year",
    x = "Year",
    y = "Number of Successful Attacks"
  ) +
  scale_x_continuous(breaks = seq(min(data$iyear), max(data$iyear), by = 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  theme(legend.title = element_text(size = 12)) +  # Corrected legend title
  theme(legend.position = "top") +
  theme(panel.grid.major = element_line(color = "grey", size = 0.5))




grouped_data <- data %>%
  group_by(iyear, success) %>%
  summarise(count = n(), .groups = 'drop')


# Plot the stacked bar chart
ggplot(grouped_data, aes(x = factor(iyear), y = count, fill = factor(success))) +
  geom_bar(stat = "identity") +  
  labs(
    title = "Stacked Bar Chart of Terror Attacks by Year",
    x = "Year",
    y = "Number of Attacks",
    fill = "Attack Outcome"
  ) +
  scale_fill_manual(values = c("red", "green"), labels = c("Failure", "Success")) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  
    panel.grid.major = element_line(color = "grey", size = 0.5),
    legend.position = "top"
  )




install.packages('tidyr')
library(tidyr)

rates <- data %>%
  group_by(country_txt) %>%
  summarise(success_rate = mean(success, na.rm = TRUE), 
            total_attacks = n())  

rates <- rates %>%
  mutate(loss_rate = 1 - success_rate)
rates_long <- rates %>%
  select(country_txt, success_rate, loss_rate) %>%
  pivot_longer(cols = c(success_rate, loss_rate), names_to = "rate_type", values_to = "rate")

# Plot the comparison of success and failure rates by country
ggplot(rates_long, aes(x = country_txt, y = rate, fill = rate_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of Success and Failure Terror Attacks by Country",
    x = "Country",
    y = "Rate",
    fill = "Rate Type"
  ) +
  scale_fill_manual(values = c("skyblue", "lightcoral")) +  # Set colors for success and failure
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels
    legend.position = "top",
    panel.grid.major = element_line(color = "grey", size = 0.5)
  )




# Group the data by country, year, and success, then count the occurrences
grouped_data <- data %>%
  group_by(country_txt, iyear, success) %>%
  summarise(count = n(), .groups = 'drop')

countries <- unique(data$country_txt)


for (country in countries) {
  country_data <- grouped_data %>% filter(country_txt == country)
  
 
  p <- ggplot(country_data, aes(x = iyear, y = count, fill = factor(success))) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.7) + 
    labs(
      title = paste(country, "- Success/Failure by Year"),
      x = "Year",
      y = "Count",
      fill = "Attack Outcome"
    ) +
    scale_fill_manual(values = c("lightcoral", "skyblue"), labels = c("Failure", "Success")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),  
      panel.grid.major = element_line(color = "grey", size = 0.5),
      legend.title = element_text(size = 12),
      legend.position = "top"
    ) +
    scale_x_continuous(breaks = seq(min(country_data$iyear), max(country_data$iyear), by = 1))  
  

  print(p)
}




install.packages("plotly")  
library(plotly)



country_attack_count <- data %>%
  group_by(country_txt) %>%
  summarise(attack_count = n(), .groups = 'drop')

# Create the choropleth map
fig <- plot_ly(country_attack_count, 
               locations = ~country_txt, 
               locationmode = 'country names', 
               z = ~attack_count, 
               colors = "viridis",  # Corrected color scale to lowercase
               type = 'choropleth', 
               colorbar = list(title = "Number of Attacks"),
               hoverinfo = 'location+z') 

# Update the layout
fig <- fig %>% layout(
  title = "Terror Attacks by Country",
  geo = list(
    showcoastlines = TRUE,
    coastlinecolor = "black",
    showland = TRUE,
    landcolor = "white",
    projection = list(type = "natural earth")
  )
)

# Show the map
fig




# Calculate the success rate by weapon type
weapon_success_rate <- data %>%
  group_by(weaptype1_txt) %>%
  summarise(success_rate = mean(success, na.rm = TRUE))  # Calculate mean success rate for each weapon type

# Create a bar plot for success rate by weapon type
ggplot(weapon_success_rate, aes(x = weaptype1_txt, y = success_rate)) +
  geom_bar(stat = 'identity', fill = 'blue', alpha = 0.7) +
  labs(title = 'Success Rate by Weapon Type', 
       x = 'Weapon Type', 
       y = 'Success Rate') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  






# Calculate the success rate by attack type
weapon_success_rate <- data %>%
  group_by(attacktype1_txt) %>%
  summarise(success_rate = mean(success, na.rm = TRUE))  # Calculate mean success rate for each attack type

# Create a bar plot for success rate by attack type
ggplot(weapon_success_rate, aes(x = attacktype1_txt, y = success_rate)) +
  geom_bar(stat = 'identity', fill = 'red', alpha = 0.7) +
  labs(title = 'Success Rate by Attack Type', 
       x = 'Attack Type', 
       y = 'Success Rate') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 




# Calculate the count of success and failure for each target type
success_counts <- data %>%
  group_by(targtype1_txt, success) %>%
  summarise(count = n(), .groups = 'drop')

# Convert 'success' to a factor for better handling in ggplot
success_counts$success <- factor(success_counts$success, levels = c(0, 1), labels = c("Failure", "Success"))

# Create the stacked bar plot
ggplot(success_counts, aes(x = targtype1_txt, y = count, fill = success)) +
  geom_bar(stat = 'identity', position = 'stack') +
  labs(title = "Stacked Bar Plot of Attacks by Target Type", 
       x = "Target Type", 
       y = "Number of Attacks") +
  scale_fill_manual(values = c("red", "green")) +  # Set colors for success/failure
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels
        legend.title = element_blank())  # Remove legend title




# Create the treemap by grouping the data
success_counts <- data %>%
  count(targtype1_txt)  # Count occurrences for each target type

# Create the treemap plot
fig <- plot_ly(
  data = success_counts,
  type = 'treemap',
  labels = ~targtype1_txt,
  values = ~n,
  textinfo = "label+value",  # Display both labels and values in the treemap
  marker = list(colors = ~n, colorscale = 'Viridis')  # Optionally use a colorscale for better visuals
) %>%
  layout(
    title = 'Treemap of Attacks by Target Type',  # Title of the treemap
    showlegend = TRUE  # Disable legend for cleaner visualization
  )

# Show the plot
fig





# Filter suicide attacks
suicide_attacks <- data %>% filter(suicide == 1)

# Count the success/failure occurrences
success_counts <- table(suicide_attacks$success)

# Convert the table into a data frame for ggplot
success_counts_df <- as.data.frame(success_counts)

# Calculate percentages
success_counts_df$percentage <- success_counts_df$Freq / sum(success_counts_df$Freq) * 100

# Create the pie chart
ggplot(data = success_counts_df, aes(x = "", y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("red", "green"), labels = c("Failure", "Success")) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
  labs(title = "Success/Failure Proportion of Suicide Attacks") +
  theme_void() +
  theme(legend.title = element_blank())  # Remove legend title










