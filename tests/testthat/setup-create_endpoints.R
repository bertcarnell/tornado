# Copyright 2022 Robert Carnell

my_mtcars <- mtcars
my_mtcars$am <- factor(my_mtcars$am, labels = c("automatic", "manual"))
my_mtcars$vs <- factor(my_mtcars$vs, labels = c("v", "s"))
my_mtcars$cyl <- factor(my_mtcars$cyl)
my_mtcars$gear <- factor(my_mtcars$gear)
my_mtcars$carb <- factor(my_mtcars$carb)

model_nofactors <- lm(mpg ~ cyl*wt*hp, data = mtcars)

model_onefactor <- lm(mpg ~ cyl*wt*hp, data = my_mtcars)

model_twofactors <- lm(mpg ~ cyl*wt*hp + vs, data = my_mtcars)

model_allfactors <- lm(mpg ~ cyl*vs, data = my_mtcars)
