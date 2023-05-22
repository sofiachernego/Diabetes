# DATA PREP ====

## load necessary libraries  ====
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)

## download the data set  ====
df <- read_csv("/Users/macbookair/Desktop/Diabetes project/diabetes.csv")

## get a summary of the df to see the data types you are working with 
glimpse(df)

## get summary stats - see if there are any obvious outliers or NAs 
summary(df)

## max insulin of 846 -  seems high
## max BMI - seems high

### zoom into the insulin outlier  
insulin_outlier <- df[df$Insulin == 846, ]
print(insulin_outlier)

### zoom into the BMI outlier 
BMI_outlier <- df[df$BMI == 67.10, ]
print(BMI_outlier)

### delete the entire patient case associated with outliers 
df <- df[!(df$Insulin %in% insulin_outlier | df$BMI %in% BMI_outlier), ]
summary(df)


# EXPLORAOTORY ANALYSIS  ====
df <- as.data.frame(df)

## Data visualization  ====
### Glucose histogram  ====
ggplot(df, aes(x = Glucose)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(x = "Glucose", y = "Frequency", title = "Distribution of Glucose")

### Blood pressure histogram  ====
ggplot(df, aes(x = BloodPressure)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(x = "Blood Pressure", y = "Frequency", title = "Distribution of BP")

### Insulin histogram  ==== 
ggplot(df, aes(x = Insulin)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(x = "Insulin", y = "Frequency", title = "Distribution of Insulin")

### BMI histogram  ====
ggplot(df, aes(x = BMI)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(x = "BMI", y = "Frequency", title = "Distribution of BMI")

### Age hist
ggplot(df, aes(x = Age)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(x = "Age", y = "Frequency", title = "Distribution of Age")


## Correlations between each pair  ====
### Compute correlation matrix 
cor_mat <- cor(df[!names(df) %in% "Outcome"])
round(cor_mat, 2)

### Visualize the matrix using a heatmap  ====
heatmap(cor_mat, Rowv = NA, Colv = NA,                           # without dendrogram 
        col = colorRampPalette(c("white", "blue"))(100),
        main = "Correlation Heatmap")


### Visualize how many people are/not diabetic - 0 non-diabetic, 1 diabetic  ====
ggplot(df, aes(Outcome) +
  geom_bar())

### Visualize how many people are diabetic relative to age  ====
ggplot(df, aes(factor(Outcome), Age))+ # come back to this because the y axis shows wrong values 
  geom_col() +
  xlab("Outcome") +
  ylab("Age")

### Matrix with scatterplots, density plots, etc. for more exploration
ggpairs(df)





  