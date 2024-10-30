
setwd("C:/FLYTILIZER_21_plts/data")

df <- read.delim("Germination_Flytilizer_Final.txt", header = TRUE)

# Alternatively, replace blank spaces in a specific column (e.g., germ_time)
df$germ_time[df$germ_time == ""] <- NA


# Load necessary library
library(ggplot2)

#Convert germ_time to numeric, handle "NA" values appropriately
df$germ_time <- as.numeric(as.character(df$germ_time))

# Visualize germination time using boxplots
# Creating side-by-side boxplots for germination time by plant species and amendment

ggplot(df, aes(x = plant_species, y = germ_time, fill = amendment)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(title = "Germination Time Distribution by Plant Species and Amendment",
       x = "Plant Species",
       y = "Germination Time (days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("_Control" = "lightblue", "BSFFF" = "lightgreen")) +
  ylim(0, NA)



################### germination success

# Load necessary library
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)

# Step 1: Calculate the proportion of germination success for each plant species and amendment
germination_summary <- df %>%
  group_by(plant_species, amendment) %>%
  summarize(success_rate = mean(germ_success)) %>%
  ungroup()

# Step 2: Create a bar plot to visualize germination success rates
ggplot(germination_summary, aes(x = plant_species, y = success_rate, fill = amendment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Effect of Amendment on Germination Success",
       x = "Plant Species",
       y = "Germination Success Rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("_Control" = "orange", "BSFFF" = "blue"))


ggplot(germination_summary, aes(x = plant_species, y = success_rate, fill = amendment)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(title = "Effect of Amendment on Germination Success",
       x = "Plant Species",
       y = "Germination Success Rate") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("_Control" = "orange", "BSFFF" = "blue"))



##############################dryweight

# Load necessary libraries
library(ggplot2)
library(dplyr)


# Read in your dataset
df_wgt <- read.table("Dryweight_Flytilizer_Final.txt", header = TRUE)

# Check the structure of the data
str(df_wgt)


# Convert amendment, plant_species, and root_shoot to factors
df_wgt $amendment <- as.factor(df_wgt$amendment)
df_wgt $plant_species <- as.factor(df_wgt$plant_species)
df_wgt $tissue <- as.factor(df_wgt$tissue)


# Verify the conversion by checking the levels
levels(df_wgt$tissue)

# Load ggplot2 library
library(ggplot2)

# Create a plot with ggplot2
ggplot(df_wgt, aes(x = plant_species, y = dryweight, fill = amendment)) +
  geom_boxplot(position = position_dodge(0.8), alpha = 0.7) +  # Boxplot with slight dodge for clarity
  facet_wrap(~ tissue, scales = "free_y") +  # Separate panels for roots and shoots
  labs(title = "Dry Weight of Roots and Shoots Across Plant Species",
       x = "Plant Species",
       y = "Dry Weight (g)",
       fill = "Amendment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("_Control" = "orange", "BSFFF" = "green"))



########################## root, shoot length
df_length <- read.delim("Root_Shoot Length_Flytilizer_Final.txt", header = TRUE)

# Load ggplot2 library
library(ggplot2)

install.packages("reshape2")
library(reshape2)

install.packages("tidyr")
library(tidyr)


# Assuming df_length is your data frame
# Reshape the data to long format for easier plotting
df_long <- melt(df_length, id.vars = c("amendment", "plant_code", "plant_species"), 
                measure.vars = c("rootlength", "shootlength"))

# Plot root and shoot length
ggplot(df_long, aes(x = plant_species, y = value, fill = amendment)) +
  geom_boxplot(position = position_dodge(0.8), alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_y", labeller = labeller(variable = c(rootlength = "Root Length", shootlength = "Shoot Length"))) +
  labs(title = "Root and Shoot Length Across Plant Species",
       x = "Plant Species",
       y = "Length (cm)",
       fill = "Amendment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("_Control" = "blue", "BSFFF" = "orange"))



# Plot Root:Shoot Ratio
ggplot(df_length, aes(x = plant_species, y = root_shoot_ratio, fill = amendment)) +
  geom_boxplot(position = position_dodge(0.8), alpha = 0.7) +
  labs(title = "Root:Shoot Ratio Across Plant Species",
       x = "Plant Species",
       y = "Root:Shoot Ratio",
       fill = "Amendment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("_Control" = "purple", "BSFFF" = "green"))

