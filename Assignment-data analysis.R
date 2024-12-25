install.packages("ggplot2")
library(ggplot2)
#bar chart for parents age 
ggplot(df, aes(x = 'Parents age (years)')) +
  geom_bar(binwidth = 10, fill = "skyblue",
           color = "black") +
  labs(title = "Distribution of Parent's Age(years)",
          x = "Parent's Age",
          y = "Count") +
  theme_minimal() 

# grouped bar chart for parents education level by parents sex
ggplot(df, aes(x =
                 `Parent’s education level`, fill =
                 `Parent’s sex`)) +
  geom_bar(position = "dodge") +
  labs(title = "parents education level by parents sex",
       x = "education level",
       y = "count",
       fill = "paents sex") +
  theme_minimal()
# Scatter plot for knowledge of antibiotics vs household income
ggplot(df, aes(x =
                 Knowledge_of_antibiotics, y = 
                 household_income_per_month_BDT)) 
+
  geom_point(color = "blue", size
             = 2, alpha = 0.7) +
  labs(title = "knowledge of antibiotics vs household income",
       x = "knowledge of antibiotics",
       y = "household income(BDT)") +
  theme_minimal()
  

# Create the data
data <- data.frame(
  Statement = c(
    "Misuse of antibiotics can lead to antibiotic resistant bacteria",
    "Infectious diseases are becoming difficult to treat with antibiotics",
    "Antibiotic resistant bacteria are difficult to treat",
    "Antibiotics can cause allergic reaction",
    "Antibiotics can cause secondary infection",
    "Antibiotics can kill normal flora",
    "Antibiotics are used to treat diarrhea",
    "Antibiotics are useful for flu and cough",
    "Antibiotics kill bacteria",
    "Magnesium is an antibiotic",
    "Paracetamol is an antibiotic",
    "Amoxicillin is an antibiotic"
  ),
  Correct = c(0.1, 0.2, 0.2, 0.1, 0.4, 0.4, 0.2, 0.3, 0.7, 0.3, 0.3, 0.7),
  Uncertain = c(0.2, 0.3, 0.2, 0.3, 0.3, 0.3, 0.3, 0.3, 0.2, 0.3, 0.3, 0.2),
  Not_correct = c(0.7, 0.4, 0.6, 0.6, 0.2, 0.2, 0.5, 0.5, 0.1, 0.4, 0.4, 0.1)
)

# Reshape the data for ggplot2
library(tidyr)
data_long <- pivot_longer(data, cols = c(Correct, Uncertain, Not_correct), 
                          names_to = "Response", values_to = "Proportion")

# Create the bar plot
ggplot(data_long, aes(x = reorder(Statement, -Proportion), y = Proportion, fill = Response)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() + # Flip coordinates for better readability
  scale_fill_manual(values = c("skyblue", "green", "yellow")) +
  labs(
    title = "Perception of Antibiotics",
    x = "Statements",
    y = "Proportion",
    fill = "Response"
  ) +
  theme_minimal()

   
# Create the data
data <- data.frame(
  Question = c(
    "I give my children antibiotics as prophylaxis",
    "I check expiring date of antibiotic before giving to children",
    "I seek medical advice before giving antibiotic to my children",
    "I give my children antibiotics when they get cough",
    "I like to take antibiotic from pharmacy instead of taking it from doctor",
    "My child should complete a given dose, even if he improves after 2 doses"
  ),
  Yes = c(0.2, 0.6, 0.6, 0.7, 0.7, 0.7),
  No = c(0.8, 0.4, 0.4, 0.3, 0.3, 0.3)
)

# Reshape the data for ggplot
library(reshape2)
data_melted <- melt(data, id.vars = "Question", variable.name = "Response", value.name = "Proportion")

# Create the bar plot
ggplot(data_melted, aes(x = Question, y = Proportion, fill = Response)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +  # Flip coordinates for horizontal bars
  scale_fill_manual(values = c("steelblue", "lightpink")) +
  labs(
    x = "",
    y = "",
    fill = "Response",
    title = ""
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))        

# Create the dataset
data <- data.frame(
  Source = c("Others", "Social media", "Internet", "Information from University courses", 
             "Information given by a colleague", "Information from nurses", 
             "Information from dispensers", "Information from prescribers", 
             "Information provided by pharmaceutical companies leaflet"),
  Percentage = c(7, 26, 29.8, 15.6, 25.5, 76.1, 85.6, 77.1, 45.7)
)

# Plot the data
ggplot(data, aes(x = reorder(Source, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Sources of Information",
       x = "Source",
       y = "Percentage (%)") +
  theme_minimal()

library(dplyr)
# Create the data frame
data <- data.frame(
  Statement = c(
    "I will see another doctor if the first one has not been prescribed antibiotics",
    "I am not satisfied if the doctor does not prescribe an antibiotic to me",
    "Antibiotics are safe and hence can be used commonly",
    "Sick child is given antibiotics, even there is no indication",
    "Antibiotics can improve fever in children",
    "A child with cold is given antibiotics",
    "I stop antibiotics when my child condition improves",
    "I reusing the same antibiotics for similar symptoms",
    "Leftover antibiotics are good to keep at home in case I might need them for my child later on",
    "Doctors often take time to inform parents how antibiotics should be used for their children"
  ),
  Agree = c(16, 16, 28, 20, 64, 62, 26, 27, 15, 52),
  Disagree = c(81, 80, 64, 75, 31, 33, 74, 71, 84, 42),
  Neutral = c(3, 4, 9, 6, 6, 5, 0, 1, 1, 5)
)

# Transform data to long format for ggplot
data_long <- pivot_longer(data, cols = c("Agree", "Disagree", "Neutral"), 
                          names_to = "Response", values_to = "Percentage")

# Plot the data
ggplot(data_long, aes(x = Percentage, y = reorder(Statement, -Disagree), fill = Response)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("goldenrod2", "grey70", "mediumturquoise")) +
  labs(
    title = "Survey Responses on Antibiotic Usage",
    x = "Percentage",
    y = "Statement",
    fill = "Response"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10, hjust = 1),
    legend.position = "top"
  )
# Create the data frame
data <- data.frame(
  Statement = c(
    "I will see another doctor if the first one has not been prescribed antibiotics",
    "I am not satisfied if the doctor does not prescribe an antibiotic to me",
    "Antibiotics are safe and hence can be used commonly",
    "Sick child is given antibiotics, even there is no indication",
    "Antibiotics can improve fever in children",
    "A child with cold is given antibiotics",
    "I stop antibiotics when my child condition improves",
    "I reusing the same antibiotics for similar symptoms",
    "Leftover antibiotics are good to keep at home in case I might need them for my child later on",
    "Doctors often take time to inform parents how antibiotics should be used for their children"
  ),
  Agree = c(16, 16, 28, 20, 64, 62, 26, 27, 15, 52),
  Disagree = c(81, 80, 64, 75, 31, 33, 74, 71, 84, 42),
  Neutral = c(3, 4, 9, 6, 6, 5, 0, 1, 1, 5)
)

# Reshape data for plotting using `reshape2`
library(reshape2)
data_long <- melt(data, id.vars = "Statement",
                  variable.name = "Response",
                  value.name = "Percentage")

# Plot the data
ggplot(data_long, aes(x = Percentage, y = reorder(Statement, -Percentage), fill = Response)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("goldenrod2", "grey70", "mediumturquoise")) +
  labs(
    title = "Parent Perceptions on Antibiotic Use",
    x = "Percentage",
    y = NULL,
    fill = "Response"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9, hjust = 1),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# Data
statements <- c(
  "I will see another doctor if the first one has not been prescribed antibiotics",
  "I am not satisfied if the doctor does not prescribe an antibiotic to me",
  "Antibiotics are safe and hence can be used commonly",
  "Sick child is given antibiotics, even there is no indication",
  "Antibiotics can improve fever in children",
  "A child with cold is given antibiotics",
  "I stop antibiotics when my child's condition improves",
  "I reuse the same antibiotics for similar symptoms",
  "Leftover antibiotics are good to keep at home in case I might need them for my child later on",
  "Doctors often take time to inform parents how antibiotics should be used for their children"
)

agree <- c(16, 16, 28, 20, 64, 62, 26, 27, 15, 52)
disagree <- c(81, 80, 64, 75, 31, 33, 74, 71, 84, 42)
neutral <- c(3, 4, 9, 6, 6, 5, 0, 1, 1, 5)

data <- data.frame(
  Statement = factor(statements, levels = rev(statements)),
  Agree = agree,
  Disagree = disagree,
  Neutral = neutral
) %>%
  tidyr::pivot_longer(cols = Agree:Neutral, names_to = "Response", values_to = "Percentage")

# Plot

ggplot(data, aes(x = Percentage, y = Statement, fill = Response)) +
  geom_bar(stat = "identity", position = "stack", color = "blue") +
  scale_fill_manual(values = c("Agree" = "#D2B48C", "Disagree" = "#D3D3D3", "Neutral" = "#90EE90")) +
  labs(x = "Percentage", y = "Statement", fill = "Response") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


install.packages("tidyverse")
install.packages("ISLR")
install.packages("sjPlot")
install.packages("likert")
library(tidyverse)
library(ISLR)
library(sjPlot)
install.packages("sjPlot")
library(sjPlot)
library(likert)

glimpse(data)
install.packages("ggrepel")
library(ggrepel)
col(data)
row.names(data)
view_df(data, show.frq = T, show.prc = T, show.na = T)

data |>
  plot_frq(`Parent’s education level`)


data |>
  group_by(`Parent’s education level`) |>
  plot_frq(`Your average household income per month (BDT)`) |>
  plot_grid()


# distribution - boxplot
data |>
  group_by(`Your average household income per month (BDT)`) |>
  plot_frq(`Parent’s age (years)`,
           type = "box"
  ) |>
  plot_grid()


data("pisaitems")
items1 <- pisaitems
items <- items1 |>
  select(starts_with("ST24Q"))
basic<- plot_likert(items)

# Load necessary library
library(ggplot2)

# Create the data
data <- data.frame(
  Question = c(
    "I give my children antibiotics as prophylaxis",
    "I check expiring date of antibiotic before giving to children",
    "I seek medical advice before giving antibiotic to my children",
    "I give my children antibiotics when they get cough",
    "I like to take antibiotic from pharmacy instead of taking it from doctor",
    "My child should complete a given dose, even if he improves after 2 doses"
  ),
  Yes = c(0.2, 0.6, 0.6, 0.7, 0.7, 0.7),
  No = c(0.8, 0.4, 0.4, 0.3, 0.3, 0.3)
)

# Reshape the data for ggplot
library(reshape2)
data_melted <- melt(data, id.vars = "Question", variable.name = "Response", value.name = "Proportion")

# Create the grouped bar plot
ggplot(data_melted, aes(x = Question, y = Proportion, fill = Response)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) + # Grouped bars
  coord_flip() +  # Flip coordinates for horizontal bars
  scale_fill_manual(values = c("blue", "yellow")) +
  labs(
    x = "",
    y = "Proportion",
    fill = "Response",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    legend.position = "top"
  )


