# here i try to remove all patient cases where insulin = 0 and work with it 

df1 <- read_csv("/Users/macbookair/Desktop/Diabetes project/diabetes.csv") 
df1_filtered <- df[df$Insulin != 0, ]

# The expression df$Insulin != 0 creates a logical vector that identifies the rows 
# where the Insulin variable is not equal to 0. By using this logical vector as an index, 
# I subset the data frame df to exclude those cases.


# Exploratory analysis =====
ggpairs(df1_filtered)

## Correlations between each pair  ====
### Compute correlation matrix 
cor_mat1 <- cor(df1_filtered[!names(df1_filtered) %in% "Outcome"])
round(cor_mat1, 2)

### Visualize the matrix using a heatmap  ====
heatmap(cor_mat1, Rowv = NA, Colv = NA                           # without dendrogram
        )                          

# Logistic regression ====
# Logistic regression is used for predicting binary outcomes.
# glucose and outcome variables have the highest correltion coeff (0.512) among other pairs

library(caret)

## Split the data into training and test set ====
set.seed(123)                                    # ensure reproducibility 
training.samples <- df1_filtered$Outcome %>% 
  createDataPartition(p = 0.8, list = FALSE)     # 80% of data goes to the training set
train.data  <- df1_filtered[training.samples, ]
test.data <- df1_filtered[-training.samples, ]


## Compute logistic regression ====
### Specify the formula ====
formula <- as.formula("Outcome ~ Glucose")


### Fit the logistic regression model using the training data ====
model <- glm(formula, data = train.data, family = binomial)


### Predict the outcomes using the testing data ====
predictions <- predict(model, newdata = test.data, type = "response")


### Compare the predicted outcomes with the actual outcomes in the testing data ====
accuracy <- mean(round(predictions) == test.data$Outcome)


## Visualize the model ====
### Create a data frame with the test data and predicted probabilities ====
prediction_data <- data.frame(Glucose = test.data$Glucose, Predicted_Probability = predictions, Outcome = test.data$Outcome)

### Create the scatter plot with a smoothed line ====
ggplot(prediction_data, aes(x = Insulin, y = Predicted_Probability, color = Outcome)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
  labs(x = "Insulin", y = "Predicted Probability", color = "Outcome") +
  theme_minimal()


ggplot(prediction_data, aes(Glucose, Predicted_Probability)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabetic"
  )


# Model evaluation ====
## Identify coefficients ====
coefficients <- coef(model)

## Exponentiate the coefficients ====
odds_ratios <- exp(coefficients)

## Interpret the Coefficients =====
