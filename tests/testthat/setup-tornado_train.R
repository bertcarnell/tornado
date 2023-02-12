# Copyright 2022 Robert Carnell

rf_model_numeric <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")

if (!exists("my_mtcars"))
{
  source("setup-create_endpoints.R")
}

rf_model_factor <- caret::train(x = subset(my_mtcars, select = -mpg), y = my_mtcars$mpg, method = "rf")

rf_model_class <- caret::train(x = subset(my_mtcars, select = -vs), y = my_mtcars$vs, method = "rf")

stepAIC_model_numeric <- caret::train(x = subset(mtcars, select = -mpg),
                                      y = mtcars$mpg, method = "glmStepAIC", trace = 0)
