if(!require(tidymodels)) install.packages("tidymodels")
library(tidymodels)
tidymodels_prefer() # Package conflict resolution

data(ames)

ames <- ames %>%
  mutate(Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.8, strata = Sale_Price) #Stratified random sampling
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

lm_model <-
  linear_reg() %>%
  set_engine("lm")

lm_form_fit <-
  lm_model %>%
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)


lm_xy_fit <-
  lm_model %>%
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% select(Sale_Price)
  )


lm_form_fit

lm_xy_fit

lm_form_fit %>% extract_fit_engine() 

lm_form_fit %>% extract_fit_engine() %>% vcov()

model_res <-
  lm_form_fit %>%
  extract_fit_engine() %>%
  summary()

param_est <- coef(model_res)
class(param_est)
param_est


tidy(lm_form_fit)

ames_test_small <- ames_test %>% slice(1:5)
predict(lm_form_fit, new_data = ames_test_small)


ames_test_small %>%
  select(Sale_Price) %>%
  bind_cols(predict(lm_form_fit, ames_test_small)) %>%
  # Add 95% prediction intervals to the results
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int"))

tree_model <-
  decision_tree(min_n = 2) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_fit <-
  tree_model %>%
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)


ames_test_small %>%
  select(Sale_Price) %>%
  bind_cols(predict(tree_fit, ames_test_small))

parsnip_addin()

