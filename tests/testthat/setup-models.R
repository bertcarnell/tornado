# Common models between importance and tornado tests

survreg_model <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps*rx + age,
                                 survival::ovarian,
                                 dist = "weibull")

set.seed(1923)
w <- sample(1:7, size = nrow(survival::ovarian), replace = TRUE)
survreg_model_weighted <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps*rx + age,
                                 survival::ovarian,
                                 dist = "weibull", weights = w)

base_glm_model <- glm(mpg ~ cyl*wt*hp + gear + carb, data = mtcars, family = gaussian)

base_glm_binomial_model <- glm(vs ~ wt + disp + gear, data = mtcars, family = binomial(link = "logit"))

weigthed_glm_model <- glm(mpg ~ cyl*wt*hp, data = mtcars, family = gaussian,
                          weights = rep(1:2, nrow(mtcars) / 2))

weighted_glm_binomial_model <- glm(vs ~ wt + disp + cyl, data = mtcars,
                                   family = binomial(link = "logit"),
                                   weights = rep(1:2, nrow(mtcars) / 2))

if (requireNamespace("glmnet")) {
  glmnet_form <- formula(mpg ~ cyl*wt*hp)
  glmnet_mf <- model.frame(glmnet_form, data = mtcars)
  glmnet_mm <- model.matrix(glmnet_mf, glmnet_mf)

  glmnet_model <- glmnet::cv.glmnet(x = glmnet_mm, y = mtcars$mpg, family = "gaussian")

  glmnet_model_weighted <- glmnet::cv.glmnet(x = glmnet_mm, y = mtcars$mpg,
                                             family = "gaussian", weights = rep(1:2, nrow(mtcars) / 2))
}
