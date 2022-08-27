library(readr)
library(GGally)
library(tidyverse)
theme_set(theme_classic())  # this is to set the theme for ggplot

# Load data set ----------------------------------------------------------------
titanic <- read_csv("titanic.csv")
dim(titanic)
summary(titanic$Age)
titanic <- titanic %>%
  drop_na()
dim(titanic)

# How many passengers survived? ------------------------------------------------
table(titanic$Survived)  # not sure what's 0 and 1?

# Convert to factor (i.e. categorical variables)
titanic$Survived <- factor(titanic$Survived)
levels(titanic$Survived) <- c("No", "Yes")
table(titanic$Survived)

# Bar plot
ggplot(titanic, aes(x = Survived)) +
  geom_bar()

# Proportion
ggplot(titanic, aes(x = Survived, fill = Survived,
                    y = ..count.. / sum(..count..))) +
  geom_bar() +
  geom_text(aes(label = scales::percent(..count.. / sum(..count..))),
            stat = "count", vjust = -0.5) +
  scale_y_continuous(labels = scales::percent, name = "Proportion",
                     limits = c(0, 0.7)) +
  theme_bw() +
  theme(legend.position = "none")

# What is the distribution of passenger class? ---------------------------------
titanic$Pclass <- factor(titanic$Pclass, labels = c("First", "Second", "Third"))
table(titanic$Pclass)
proportions(table(titanic$Pclass))

ggplot(titanic, aes(x = Pclass)) +
  geom_bar()

# Any relationship between the two variables? ----------------------------------
(tab <- table(titanic$Survived, titanic$Pclass))
round(proportions(tab), 2)

# visualise
ggplot(titanic, aes(x = Pclass, y = 1, fill = Survived)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion survived by passenger class",
       x = "Passenger class", y = "Proportion")

# test for contingency table: survivability independent of passenger class? No.
chisq.test(tab)

# Distribution of age of passengers? -------------------------------------------
summary(titanic$Age)
qplot(titanic$Age, geom = "histogram")

ggplot(titanic, aes(x = Age, y = stat(density))) +
  geom_histogram() +
  geom_line(stat = "density", col = "blue", size = 1)

ggplot(titanic, aes(x = Age, group = Survived,
                    fill = Survived)) +
  geom_histogram() +
  labs(y = "Frequency", title = "Histogram of age of passengers")

ggplot(titanic, aes(x = Age, y = stat(density), group = Survived,
                    fill = Survived)) +
  geom_histogram() +
  geom_density(aes(fill = NULL), size = 1) +
  facet_grid(. ~ Survived) +
  theme(legend.position = "none") +
  labs(y = "Density", title = "Histogram of age of passengers")

ggpairs(titanic, columns = c("Age", "Pclass"), mapping = aes(col = Survived)) +
  theme(legend.position = "bottom")

# Comparison mean age of survival
x <- titanic$Age[titanic$Survived == "Yes"]  # age of survivors
y <- titanic$Age[titanic$Survived == "No"]  # age of not survived
mean(x)
mean(y)

hist(x)
hist(y)
# Shapiro test H0: normal, H1: not normal
shapiro.test(x)  # test rejected therefore non-normal
shapiro.test(y)  # test rejected therefore non-normal
# cannot use t-test. proceed with non-parametric test
wilcox.test(x, y)

agegroup <- titanic$Age
agegroup <- ifelse(titanic$Age <= 12, "kids", "adults")
titanic$agegroup <- agegroup
table(titanic$agegroup, titanic$Survived) -> tab
chisq.test(tab)
# worry is that no of adults is much higher than kids, so the difference is due
# to this instead of survivability. better to do the analysis separately

x <- titanic$Age[titanic$Survived == "Yes" & titanic$agegroup == "adults"]  # adults age of survivors
y <- titanic$Age[titanic$Survived == "No" & titanic$agegroup == "adults"]  # adults age of not survived
hist(x)
hist(y)
# Shapiro test H0: normal, H1: not normal
shapiro.test(x)  # test rejected therefore non-normal
shapiro.test(y)  # test rejected therefore non-normal
# cannot use t-test. proceed with non-parametric test
wilcox.test(x, y)

table(titanic$Sex, titanic$Survived) -> tab
chisq.test(tab)

# Save data set ----------------------------------------------------------------
save(titanic, file = "titanic.RData")  # this saves the clean data set

# Logistic regression ----------------------------------------------------------
# load("titanic.RData")
mod <-  glm(Survived ~ Age + Sex + Pclass, titanic, family = "binomial")
summary(mod)
fitted(mod)  # these are probabilities!

# Compare male vs female
tibble(
  Sex = "male", Pclass = "First",
  Age = seq(0, 80, by = 1)
) -> predict.df1
predict.df1$fitted <- predict(mod, newdata = predict.df1, type = "response")
tibble(
  Sex = "female", Pclass = "First",
  Age = seq(0, 80, by = 1)
) -> predict.df2
predict.df2$fitted <- predict(mod, newdata = predict.df2, type = "response")
male.female <- rbind(predict.df1, predict.df2)

ggplot(titanic, aes(Age, Survived)) +
  geom_point(aes(shape = Pclass)) +
  geom_line(data = male.female, aes(Age, fitted + 1, group = Sex, col = Sex)) +
  labs(title = "Predicted probability of survival by gender and age in First Class")

# Compare survival by class for males
predict.df1 <- tibble(
  Age = rep(seq(0, 80, by = 1), 3),
  Pclass = rep(c("First", "Second", "Third"), each = 81),
  Sex = "male"
)
predict.df2 <- tibble(
  Age = rep(seq(0, 80, by = 1), 3),
  Pclass = rep(c("First", "Second", "Third"), each = 81),
  Sex = "female"
)
predict.df <- rbind(predict.df1, predict.df2)
predict.df$fitted <- predict(mod, newdata = predict.df, type = "response")
predict.df$Pclass

ggplot(titanic, aes(Age, Survived)) +
  geom_point(aes(shape = Sex)) +
  geom_line(data = predict.df, aes(Age, fitted + 1, group = factor(Pclass):factor(Sex), col = Pclass, linetype = Sex)) +
  labs(title = "Predicted probability of survival of male passengers by passenger class and age",
       col = "Passenger class")
