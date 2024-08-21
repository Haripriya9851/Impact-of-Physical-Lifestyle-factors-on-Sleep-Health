# Name: Hari Priya Ramamoorthy
# Date: 6th Aug, 2024
# Class: ALY6000 - Introduction to Analytics
#Aim: Analyse sleep trend on working level people(aged 23-59) and their relationship between physical activity,age,heart_rate,bmi,stress level

################################### Data Pre-processing ##############################################
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
#library(gridExtra)
# Load the dataset
data <- read.csv("ss.csv")

#keep needed columns
# View the first few rows of the dataset
head(data)

# Check for missing values
sum(is.na(data))  #no missing values found

#Clean column names
library(janitor)
data<-janitor::clean_names(data)
names(data)

library(dplyr)
library(tibble)
library(knitr)
# Function to create a glimpse table
create_glimpse_table <- function(df) {
  tibble(
    Column_Name = names(df),
    Data_Type = sapply(df, class),
    Example_Value = sapply(df, function(x) if (length(x) > 0) x[1] else NA)
  )
}

data <- data %>% select(-c("person_id" ,"blood_pressure", "daily_steps","sleep_duration"))

# Create the glimpse table
glimpse_table <- create_glimpse_table(data)


# Summary of the dataset
sage=summary(data$age)
ssleep=summary(data$quality_of_sleep)
sact=summary(data$physical_activity_level)
sstress=summary(data$stress_level)
shr=summary(data$heart_rate)
cbind(sage,ssleep,sact,sstress,shr)

################################### Data Pre-processing ################################################################################

################################### Not Included in PPT: Plot-1 Boxplot distribution of Sleep Duration ##################################################################

# Calculate statistics for annotations
stats <- data %>%
  summarise(
    min_sleep_duration = min(quality_of_sleep, na.rm = TRUE),
    max_sleep_duration = max(quality_of_sleep, na.rm = TRUE),
    avg_sleep_duration = mean(quality_of_sleep, na.rm = TRUE),
    iqr_sleep_duration= IQR(quality_of_sleep, na.rm = TRUE),
    .groups = 'drop'
  )

# Create the box plot with added statistics
sleep_duration_dist <- ggplot(data, aes(x = "", y = quality_of_sleep)) +
  geom_boxplot() +
  geom_text(data = stats, aes(x = 1, y = min_sleep_duration, label = paste0("Min: ", round(min_sleep_duration, 1), " hrs")), vjust = -1, color = "red",size = 5) +
  geom_text(data = stats, aes(x = 1, y = max_sleep_duration, label = paste0("Max: ", round(max_sleep_duration, 1), " hrs")), vjust = -1, color = "green",size = 5) +
  geom_text(data = stats, aes(x = 1, y = avg_sleep_duration, label = paste0("Avg: ", round(avg_sleep_duration, 1), " hrs")), vjust = -1, color = "blue",size = 5) +
  geom_text(data = stats, aes(x = 1, y = avg_sleep_duration - iqr_sleep_duration / 2, label = paste0("IQR: ", round(iqr_sleep_duration, 1), " hrs")), vjust = -0.5, color = "purple",size = 5) +
  labs(title = "Distribution of Sleep Duration", x = "Quality Sleep Duration (hours)", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )
# Display the plot
#print(sleep_duration_dist)
################################### Boxplot of Sleep Duration ##################################################################

########################### Plot-2 Box plot of sleep duration by gender #########################################################
ggplot(data, aes(x=gender,fill = gender)) +
  geom_bar() + labs(title = "Proportional Distribution of Gender") +
  geom_text(stat='count', aes(label=..count..), vjust=-1,size=5) +
  theme_classic(base_size = 14)

# Calculate boxplot statistics
stats <- data %>%
  group_by(gender) %>%
  summarise(
    min_sleep_duration = min(quality_of_sleep, na.rm = TRUE),
    max_sleep_duration = max(quality_of_sleep, na.rm = TRUE),
    avg_sleep_duration = mean(quality_of_sleep, na.rm = TRUE),
    iqr_sleep_duration = IQR(quality_of_sleep, na.rm = TRUE),
    .groups = 'drop'
  )

# Create the box plot with added statistics
box_plot_with_stats <- ggplot(data, aes(x = gender, y = quality_of_sleep, fill = gender)) +
  geom_boxplot() +
  geom_text(data = stats, aes(x = gender, y = min_sleep_duration, label = paste0("Min: ", min_sleep_duration, " hrs")), vjust = -0.5, color = "red",size = 5) +
  geom_text(data = stats, aes(x = gender, y = max_sleep_duration, label = paste0("Max: ", max_sleep_duration, " hrs")), vjust = -0.5, color ="purple",size = 5) +
  geom_text(data = stats, aes(x = gender, y = avg_sleep_duration, label = paste0("Avg: ", round(avg_sleep_duration, 1), " hrs")), vjust = -0.5, color = "blue",size = 5) +
  geom_text(data = stats, aes(x = gender, y = avg_sleep_duration - iqr_sleep_duration / 2, label = paste0("IQR: ", round(iqr_sleep_duration, 1), " hrs")), vjust = -0.5, color = "green",size = 5) +
  labs(title = "Sleep Duration by Gender", x = "Gender", y = "Sleep Duration (hours)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 1)
  )

# Display the plot
#print(box_plot_with_stats)

#Inference: 
# Average Sleep Duration:Females tend to sleep slightly longer than males on average. The average sleep duration for females is 7.2 hours, while for males, it is 7 hours.
# Sleep Duration Range: The range of sleep duration for females is slightly broader than for males. The maximum sleep duration for females is 8.5 hours and the minimum is 5.8 hours. For males, the maximum is 8.1 hours and the minimum is 5.9 hours.
# Interquartile Range (IQR):The interquartile range (IQR) for females is 1.6 hours, which is higher than the IQR for males at 1.4 hours. This indicates that there is more variability in the middle 50% of sleep durations for females compared to males.
# The first quartile (lower edge of the box) for females is at around 7 hours, while for males, it is slightly below 7 hours. The third quartile (upper edge of the box) is higher for females than for males, reinforcing that females generally report longer sleep durations in the middle 50% of the data.

########################### Box plot of sleep duration by gender ##########################################################


######################################## Plot-3 Age Group Wise Sleep Duration #############################################################
# Categorical Conversion : Create age categories every 10 years
data$age_group <- cut(data$age, 
                      breaks = seq(20, 60, by = 10), 
                      labels = c( "20-29", "30-39", "40-49", "50-59"), 
                      right = FALSE)

# Calculate the average sleep duration for each age group
avg_sleep_duration <- data %>%
  group_by(age_group) %>%
  summarise(avg_sleep = mean(quality_of_sleep, na.rm = TRUE))

# Bar Chart of Average Sleep Duration by Age Group
age_plot<-ggplot(avg_sleep_duration, aes(x = age_group, y = avg_sleep, fill = age_group)) +
  geom_bar(stat = "identity") + geom_text(label=paste0(round(avg_sleep_duration$avg_sleep,2),"Hrs"), vjust = -1,size=5,color="blue")+
  labs(title = "Average Sleep Duration by Age Group", x = "Age Group", y = "Average Sleep Duration (hours)") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  ) + scale_fill_brewer(palette = "Set1") #+ sort(avg_sleep_duration$avg_sleep,decreasing = FALSE)

#rint(age_plot)
## Inference : With age the quality of sleep increases.
#########################################    Plot-3 ENDS    ####################################################################################

######################################## Plot-4 Physical Activity Vs Sleep Quality #############################################################
stats_phy <- data %>%
  summarise(
    min_sleep_duration = min(physical_activity_level, na.rm = TRUE),
    max_sleep_duration = max(physical_activity_level, na.rm = TRUE),
    avg_sleep_duration = mean(physical_activity_level, na.rm = TRUE),
    iqr_sleep_duration= IQR(physical_activity_level, na.rm = TRUE),
    .groups = 'drop'
  )

physical_activity_dist <- ggplot(data, aes(x = 1, y = physical_activity_level)) +
  geom_boxplot() +
  geom_text(data = stats_phy, aes(x = 1, y = min_sleep_duration, label = paste0("Min: ", round(min_sleep_duration, 1), " minutes")), vjust = -1, color = "red",size = 5) +
  geom_text(data = stats_phy, aes(x = 1, y = max_sleep_duration, label = paste0("Max: ", round(max_sleep_duration, 1)  ," minutes")), vjust = -1, color = "green",size = 5) +
  geom_text(data = stats_phy, aes(x = 1, y = avg_sleep_duration, label = paste0("Avg: ", round(avg_sleep_duration, 1) ," minutes")), vjust = -1, color = "blue",size = 5) +
  geom_text(data = stats_phy, aes(x = 1, y = avg_sleep_duration - iqr_sleep_duration / 2, label = paste0("IQR: ", round(iqr_sleep_duration, 1))), vjust = -0.5, color = "purple",size = 5) +
  labs(title = "Distribution of Physical Activity") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )

# Display the plot
#print(physical_activity_dist)

### Categorical Conversion : Create activity categories into 3 by breaking the data for every 30 minutes 
### based on box plot min and max)
data <- data %>%
  mutate(
    physical_activity_category = cut(
      physical_activity_level,
      breaks = c(0, 30, 60, 90, Inf),
      labels = c("Very Low", "Low", "Moderate", "High"),
      right = FALSE
    )
  )

# Calculate average sleep duration by physical activity category
avg_sleep_physical_activity <- data %>%
  group_by(physical_activity_category) %>%
  summarise(avg_sleep_duration = mean(quality_of_sleep, na.rm = TRUE), .groups = 'drop') #%>%
  #mutate(physical_activity_category = factor(physical_activity_category, levels = physical_activity_category[order(avg_sleep_duration)]))

# Create a bar plot of average sleep duration by physical activity category
avg_sleep_physical_activity_plot <- ggplot(avg_sleep_physical_activity, aes(x = physical_activity_category, y = avg_sleep_duration, fill = physical_activity_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(avg_sleep_duration, 2), " Hours")), vjust = -0.5, color = "black", size = 5) +
  labs(title = "Average Sleep Duration by Physical Activity Category", x = "Physical Activity Category", y = "Average Sleep Duration (hours)") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )+ scale_fill_brewer(palette = "Set1")

# Display the plot
#print(avg_sleep_physical_activity_plot)

#Inference: 
#No relationship on physical activity we can see, people with physical activity 30 mins have good sleep quality, but people who have physical activity 75 also have good sleep quality

######################################## Plot-4 Physical Activity Vs Sleep Quality ENDS #############################################################

######################################## Plot-5 Stress Vs Sleep Quality  #############################################################
stress_dist <- ggplot(data, aes(x = stress_level)) +
  geom_histogram( fill = "skyblue", color = "black") +
  labs(title = "Distribution of Stress Levels", x = "Stress Level", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )

# Display the plot
#print(stress_dist)

# Categorical Conversion : Create stress categories 
# Categorize stress_level into different levels (if not already done)
data <- data %>%
  mutate(stress_level_category = case_when(
    stress_level >= 5 & stress_level < 7 ~ "Moderate",
    stress_level >= 7 ~ "High",
    TRUE ~ "Low"
  ))

# Plot histogram of stress_level_category
stress_level_category_dist <- ggplot(data, aes(x = stress_level_category, fill = stress_level_category)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  labs(title = "Distribution of Stress Level Categories", x = "Stress Level Category", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(color = "blue") # Changing axis text color to blue
  )

# Display the plot
#print(stress_level_category_dist)

# Calculate average sleep duration for each stress level category
avg_stress_sleep_duration <- data %>%
  group_by(stress_level_category) %>%
  summarise(avg_sleep = mean(quality_of_sleep, na.rm = TRUE))

# Plot average sleep duration for each stress level category
avg_sleep_stress_plot <- ggplot(avg_stress_sleep_duration, aes(x = reorder(stress_level_category, -avg_sleep), y = avg_sleep, fill = stress_level_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avg_sleep, 1)), vjust = -0.5, size = 4) + # Add average sleep duration labels
  labs(title = "Average Sleep Duration by Stress Level Category", x = "Stress Level Category", y = "Average Sleep Duration (hours)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(color = "blue") # Changing axis text color to blue
  )

# Display the plot
#print(avg_sleep_stress_plot)


######################################## Plot-5 Stress Vs Sleep Quality ENDS #############################################################

#######################################. Plot-6 BMI Vs Sleep Quality ############################################################
unique(data$bmi_category)

avg_sleep_bmi <- data %>% mutate(bmi_category = case_when(
  bmi_category == "Normal" ~ "Normal Weight",
  .default = bmi_category
)) %>% 
  group_by(bmi_category) %>%
  summarise(avg_sleep = mean(quality_of_sleep, na.rm = TRUE)) 

#avg_sleep_bmi<- na.omit(avg_sleep_bmi)
#filter(avg_sleep_bmi$bmi_category)

avg_sleep_bmi_plot <- ggplot(avg_sleep_bmi, aes(x = reorder(bmi_category, avg_sleep), y = avg_sleep, fill = bmi_category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avg_sleep, 1)), vjust = -0.5, size = 4) + # Add average sleep duration labels
  labs(title = "Average Sleep Duration by BMI Category", x = "BMI Category", y = "Average Sleep Duration (hours)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, color = "blue")
  )

#print(avg_sleep_bmi_plot)
#######################################. Plot-6 BMI Vs Sleep Quality ENDS ############################################################

############################### Plot-7 OCCUPATION PLOT############################################################ ############################## 
# Calculate average sleep duration for reordering
# Group and summarize the data by occupation
avg_metrics_occupation <- data %>%
  group_by(occupation) %>%
  summarise(
    avg_sleep_occupation = mean(quality_of_sleep, na.rm = TRUE),
    mean_stress_level = mean(stress_level, na.rm = TRUE)
  )

avg_metrics_occupation <- avg_metrics_occupation %>%
  mutate(occupation = factor(occupation, 
                             levels = avg_metrics_occupation %>%
                               arrange(desc(avg_sleep_occupation)) %>%
                               pull(occupation)))

# Reshape the data for plotting, excluding physical activity
avg_metrics_long <- avg_metrics_occupation %>%
  pivot_longer(cols = c(avg_sleep_occupation, mean_stress_level), 
               names_to = "Metric", 
               values_to = "Value")

# Define a custom color palette
custom_colors <- c(
  avg_sleep_occupation = "darkgreen",
  mean_stress_level = "red"
)

# Create the combined plot with facets
occupational_stress_plot <- ggplot(avg_metrics_long, 
                        aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Average Sleep Duration and Stress Level by Occupation",
       x = "Metric", 
       y = "Average Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14,face="bold",color="purple"),
        axis.title.x = element_text(size = 14,face="bold",color="purple"),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = 0.5,face="bold",color="purple")) +
  facet_wrap(~ occupation, scales = "free_y") +
  theme(legend.position = "none")

# Display the combined plot
#print(occupational_stress_plot)
############################## Plot-7 OCCUPATION PLOT ENDS ##################################################################### 

############################## Plot-8 Heart Rate Vs Sleep Quality ##############################################################
heart_rate_sleep_plot<-ggplot(data, aes(x=heart_rate, y=quality_of_sleep,fill=heart_rate)) +
  stat_summary(fun=mean, geom="bar") +
  labs(title="Average Sleep Duration by Heart Rate",
       x="Heart Rate (bpm)",
       y="Average Sleep Duration (hours)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14, color = "blue")
        )
############################## Plot-8 Heart Rate Vs Sleep Quality ENDS ##############################################################

# Display the plots
print(box_plot_with_stats) # Gender plot
print(age_plot)
print(avg_sleep_physical_activity_plot)
print(heart_rate_sleep_plot)
print(avg_sleep_bmi_plot)
print(avg_sleep_stress_plot)
print(occupational_stress_plot)
