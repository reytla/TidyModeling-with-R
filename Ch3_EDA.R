if(!require(tidymodels)) install.packages("tidymodels")
library(tidymodels)

tidymodels_prefer()

ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = "white")

ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = "white") +
  scale_x_log10()


data(ames)

ames <- ames %>%
  mutate(Sale_Price = log10(Sale_Price))


set.seed(501)


ames_split <- initial_split(ames, prop = 0.8)

ames_train <- training(ames_split)
ames_test <- testing(ames_split)


dim(ames_train)

ggplot(ames_train, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = "white")

# Stratified random sampling: breaks distribution into quartiles and does a
# 80/20 sampling from each quartile

set.seed(502)

ames_split <- initial_split(ames, prop = 0.8, strata = Sale_Price)

ames_train <- training(ames_split)
ames_test <- testing(ames_split)

ggplot(ames_train, aes(x = Sale_Price)) +
  geom_histogram(bins = 50, col = "white")
