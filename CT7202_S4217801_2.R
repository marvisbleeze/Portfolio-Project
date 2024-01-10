library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(readr)
library(ROCR)
library(purrr)
library(devtools)
library(visdat)
library(ggpubr)
library(Metrics)
library(DataExplorer)
library(inspectdf)
library(caTools)



# Set the path to folder
folder <- "/Users/mavisbleeze/Desktop/Data_Science/R assesment/data"

# List CSV files and create paths
data_paths <- list.files(path = folder, pattern = "*.csv", full.names = TRUE)

# Merge CSV files
crime_data <- data_paths %>% 
  map_df(~read_csv(.))

# Explore the merged data
glimpse(crime_data)
view(crime_data)


#Missing data
vis_miss(crime_data)
vis_dat(crime_data)


# rename columns

crime_data <- crime_data %>% 
  rename(area = '...1')
crime_data <- crime_data %>% 
  rename(homicide = 'Number of Homicide Convictions')
crime_data <- crime_data %>% 
  rename(person_offence = 'Number of Offences Against The Person Convictions')
crime_data <- crime_data %>% 
  rename(sexual_offence = 'Number of Sexual Offences Convictions')
crime_data <- crime_data %>% 
  rename(burglary = 'Number of Burglary Convictions')
crime_data <- crime_data %>% 
  rename(robbery = 'Number of Robbery Convictions')
crime_data <- crime_data %>% 
  rename(theft = 'Number of Theft And Handling Convictions')
crime_data <- crime_data %>% 
  rename(fraud_forgery = 'Number of Fraud And Forgery Convictions')
crime_data <- crime_data %>% 
  rename(Criminal_damage = 'Number of Criminal Damage Convictions')
crime_data <- crime_data %>% 
  rename(drugs_offence = 'Number of Drugs Offences Convictions')
crime_data <- crime_data %>% 
  rename(public_order = 'Number of Public Order Offences Convictions')
crime_data <- crime_data %>% 
  rename(other_offences = 'Number of All Other Offences (excluding Motoring) Convictions')
crime_data <- crime_data %>% 
  rename(motoring_offence = 'Number of Motoring Offences Convictions')
crime_data <- crime_data %>% 
  rename(admin_unsuccess = 'Number of Admin Finalised Unsuccessful')

# Convert percent columns to numeric
crime_data <- crime_data %>% 
  mutate(across(where(is.character), ~gsub("-", "", .)))
crime_data <- crime_data %>% 
  mutate(across(where(is.character), ~gsub("%", "", .)))

#Replace empty columns with NA
crime_data[crime_data == ""] <- NA

#Sort data
crime_data_new <- crime_data %>% filter(area != "National", area != "Metropolitan and City")

crime_data_new$area <- factor(crime_data_new$area, levels = crime_data_new %>% 
                                       group_by(area) %>% 
                                       summarise(Total = sum(homicide, na.rm = TRUE)) %>% 
                                       arrange(desc(Total)) %>% 
                                       pull(area))

# Descriptive Statistics: homicide

summary(crime_data_new$homicide)

# homicide data frame 
homicide_table <- as.data.frame(table(crime_data_new$homicide))

# create bar chart
ggplot(homicide_table, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "orange") +
  xlab("Homicide Convictions") + 
  ylab("Frequency") +
  ggtitle("Distribution of Homicide Convictions") +
  theme_minimal()

# Plot comparison of homicides in areas
ggplot(crime_data_new, aes(x = area, y = homicide, fill = area)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Area", y = "Homicide counts", title = "Homicides in area") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")


# Descriptive Statistics: offences against person

summary(crime_data_new$person_offence)

# data frame for person offences
person_table <- as.data.frame(table(crime_data_new$person_offence))

#Person offences distribution
ggplot(person_table, aes(x = Var1, y = Freq)) +
  geom_point(color = "orange") +
  xlab("Offences against person convictions") + 
  ylab("Frequency") +
  ggtitle("Distribution of offences against person") +
  theme_minimal()

# Comparison of person offences
ggplot(crime_data_new, aes(x = area, y = person_offence, fill = area)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "area", y = "Offences against person Convictions", title = "Offences against persons convictions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# Descriptive Statistics: sexual offences

summary(crime_data_new$sexual_offence)

# data frame for person offences
sexual_table <- as.data.frame(table(crime_data_new$sexual_offence))

#Sexual offences distribution
ggplot(crime_data_new, aes(x = sexual_offence)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  xlab("Sexual Offences COnvictions") +
  ylab("Frequency") +
  ggtitle("Distribution of Sexual Offences Convictions") +
  theme_minimal()

# Sexual offence across areas
ggplot(crime_data_new, aes(x = area, y = person_offence, fill = area)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "area", y = "Sexual Offences conviction", title = "Sexual Offences convictions across areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# Descriptive Statistics: burglary convictions

summary(crime_data_new$burglary)

# data frame for burglary convictions
burglary_table <- as.data.frame(table(crime_data_new$burglary))

#Burglary conviction distribution
ggplot(crime_data_new, aes(x = burglary)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  xlab("Burglary Convictions") +
  ylab("Frequency") +
  ggtitle("Distribution of burglary Convictions") +
  theme_minimal()

# Burlgary Conviction across areas
ggplot(crime_data_new, aes(x = area, y = burglary, fill = area)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "area", y = "Burglary conviction", title = "Burglary convictions across areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# Descriptive Statistics: drugs offence

summary(crime_data_new$drugs_offence)

# data frame for drugs offence convictions
drugs_table <- as.data.frame(table(crime_data_new$drugs_offence))

#Drugs offence conviction distribution
ggplot(crime_data_new, aes(x = drugs_offence)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  xlab("Drugs offence Convictions") +
  ylab("Frequency") +
  ggtitle("Distribution of drugs offence Convictions") +
  theme_minimal()

# Drugs offence conviction across areas
ggplot(crime_data_new, aes(x = area, y = drugs_offence, fill = area)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "area", y = "Drugs offence conviction", title = "Drugs offence convictions across areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

#correlation analysis
ggscatter(crime_data, x = "homicide", y = "burglary", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Homicide Convictions", ylab = "Burglary Convictions",
          color = "orange")

res <- cor.test(crime_data$homicide, crime_data$burglary,
                method = 'pearson')

res

#Regression Model
l_model <- lm(homicide ~ burglary + drugs_offence, 
              data = crime_data)

summary(l_model)

#cluster model

numbers <- select(crime_data, homicide, burglary, drugs_offence)

# K-means clustering
set.seed(35)
cluster.conviction <- kmeans(numbers[, 1:2], 2, nstart = 20)

# clustering result
print(cluster.conviction)

# Creating a table
table(cluster.conviction$cluster, numbers$homicide)

# Clustering plot
cluster.conviction$cluster <- as.factor(cluster.conviction$cluster)
ggplot(numbers, aes(homicide, burglary, color=cluster.conviction$cluster)) +
  geom_point() +
  ggtitle("Burglary and Homicide Cluster Plot")

#classification
# Add clusters to the data
numbers$clusters <- cluster.conviction$cluster

# Splitting the data
numbers_split <- sample.split(numbers$clusters, SplitRatio = 0.7)
train <- subset(numbers, numbers_split == TRUE)
test <- subset(numbers, numbers_split == FALSE)

# Fit logistic regression model
log_model <- glm(clusters ~ homicide + burglary + drugs_offence, 
                 data = train, family = "binomial") 

# Summary of the model
summary(log_model)

#check accuracy and plot ROC-AUC curve
predictions <- predict(log_model, 
                       test, type = "response")

predictions <- ifelse(predictions > 0.5, 1, 0)

measure <- mean(predictions != test$clusters)
print(paste('Accuracy =', 1 - measure))


# ROC-AUC Curve
ROC <- prediction(predictions, test$clusters)
ROCPer <- performance(ROC, measure = "tpr",
                      x.measure = "fpr")

auc <- performance(ROC, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1),
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)