# Week 1: Data Cleaning and Preliminary Analysis with R
# Dataset: Titanic passenger survival data (891 rows, 12 columns)
# Source: Kaggle Titanic competition / public mirror
# https://www.kaggle.com/c/titanic/data

# Packages
library(ggplot2)
library(dplyr)

# 1. Load data
titanic <- read.csv("titanic_raw.csv", stringsAsFactors = FALSE)

# 2. Initial inspection
dim(titanic)
str(titanic)
head(titanic)
summary(titanic)

# 3. Missing-value assessment
missing_count <- colSums(is.na(titanic))
missing_percent <- round(missing_count / nrow(titanic) * 100, 2)
data.frame(Column = names(missing_count),
           Missing = as.numeric(missing_count),
           Percent = as.numeric(missing_percent))

# 4. Cleaning
# Cabin has approximately 77% missing values, so it is excluded from
# the preliminary analysis rather than using weak imputation.
titanic_clean <- titanic %>%
  mutate(
    Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age),
    Embarked = ifelse(is.na(Embarked),
                      names(sort(table(Embarked), decreasing = TRUE))[1],
                      Embarked)
  )

# 5. Outlier detection using the IQR rule
iqr_bounds <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  c(lower = q1 - 1.5*iqr, upper = q3 + 1.5*iqr)
}

for (v in c("Age", "Fare", "SibSp", "Parch")) {
  b <- iqr_bounds(titanic_clean[[v]])
  cat(v, "lower =", b["lower"], "upper =", b["upper"], "\n")
}

# IQR capping preserves observations while reducing the effect of extremes.
cap_iqr <- function(x) {
  b <- iqr_bounds(x)
  pmin(pmax(x, b["lower"]), b["upper"])
}

titanic_clean <- titanic_clean %>%
  mutate(
    Age = cap_iqr(Age),
    Fare = cap_iqr(Fare),
    SibSp = cap_iqr(SibSp),
    Parch = cap_iqr(Parch)
  )

# 6. Remove fields not needed for preliminary numerical analysis
titanic_clean <- titanic_clean %>%
  select(-Name, -Ticket, -Cabin)

# 7. Encode categorical variables
titanic_clean$Sex_Encoded <- ifelse(titanic_clean$Sex == "female", 0, 1)
titanic_clean$Sex <- NULL

titanic_clean$Embarked_Q <- ifelse(titanic_clean$Embarked == "Q", 1, 0)
titanic_clean$Embarked_S <- ifelse(titanic_clean$Embarked == "S", 1, 0)
titanic_clean$Embarked <- NULL

# 8. Standardize numerical variables (z-score)
z_score <- function(x) as.numeric(scale(x))
titanic_clean$Age_Z <- z_score(titanic_clean$Age)
titanic_clean$Fare_Z <- z_score(titanic_clean$Fare)
titanic_clean$SibSp_Z <- z_score(titanic_clean$SibSp)
titanic_clean$Parch_Z <- z_score(titanic_clean$Parch)

# 9. Exploratory analysis
summary(titanic_clean)

survival_rate <- mean(titanic$Survived) * 100
survival_by_sex <- titanic %>%
  group_by(Sex) %>%
  summarise(Survival_Rate = mean(Survived) * 100)

survival_by_class <- titanic %>%
  group_by(Pclass) %>%
  summarise(Survival_Rate = mean(Survived) * 100)

cor_matrix <- cor(titanic[c("Survived","Pclass","Age","SibSp","Parch","Fare")],
                  use = "complete.obs")

print(survival_rate)
print(survival_by_sex)
print(survival_by_class)
print(round(cor_matrix, 3))

# 10. Visualizations
ggplot(titanic, aes(x = Age)) +
  geom_histogram(bins = 25, color = "black") +
  labs(title = "Distribution of Passenger Age", x = "Age", y = "Frequency")

ggplot(titanic, aes(x = Fare)) +
  geom_histogram(bins = 30, color = "black") +
  labs(title = "Distribution of Passenger Fare", x = "Fare", y = "Frequency")

ggplot(survival_by_class, aes(x = factor(Pclass), y = Survival_Rate)) +
  geom_col() +
  labs(title = "Survival Rate by Passenger Class",
       x = "Passenger Class", y = "Survival Rate (%)")

ggplot(survival_by_sex, aes(x = Sex, y = Survival_Rate)) +
  geom_col() +
  labs(title = "Survival Rate by Sex",
       x = "Sex", y = "Survival Rate (%)")

# 11. Save cleaned data
write.csv(titanic_clean, "titanic_cleaned.csv", row.names = FALSE)
