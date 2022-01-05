<div>
<table>
<tr>
<td>
<img align="left" width="200" height="200" src="logo.png"/>
</td>
</tr>
</table>
</div>

|                                                                               <sub>Actions</sub>                                                                               |                                                                  <sub>Code Coverage</sub>                                                                  |                                          <sub>Website</sub>                                          |
|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:----------------------------------------------------------------------------------------------------------------------------------------------------------:|:----------------------------------------------------------------------------------------------------:|
| [![R-CMD-CHECK](https://github.com/bertcarnell/tornado/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/bertcarnell/tornado/actions/workflows/r-cmd-check.yml) | [![Coverage status](https://codecov.io/gh/bertcarnell/tornado/branch/master/graph/badge.svg)](https://codecov.io/github/bertcarnell/tornado?branch=master) | [![](https://img.shields.io/badge/pkgdown-tornado-blue.svg)](https://bertcarnell.github.io/tornado/) |

tornado
=======

`tornado` plots for model sensitivity analysis.

`tornado` [website](https://bertcarnell.github.io/tornado/)

Installation
------------

You can also install the development version of `lhs` from github with:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("bertcarnell/tornado")
```

Quick Start - Torndao Plots
---------------------------

### Linear Models

``` r
lm1 <- lm(mpg ~ cyl*wt*hp, data = mtcars)
torn1 <- tornado(lm1, type = "PercentChange", alpha = 0.10)
plot(torn1, xlabel = "MPG")
```

![](man/figures/random-1.png)

### Generalized Linear Models

Plots can also be created for the range of the input data

``` r
glm2 <- glm(vs ~ wt + disp + cyl, data = mtcars, family = binomial(link = "logit"))
torn2 <- tornado(glm2, type = "ranges", alpha = 0.10)
plot(torn2, xlabel = "V or Straight Engine", geom_bar_control = list(width = 0.5))
```

![](man/figures/glm_section-1.png)

Plots can include factors and percentiles of the data as well.

``` r
mydata <- mtcars
mydata$am <- factor(mtcars$am)
mydata$vs <- factor(mtcars$vs)
glm2 <- glm(mpg ~ wt + cyl + am + vs, data = mydata, family = gaussian)
torn2 <- tornado(glm2, type = "percentiles", alpha = 0.10)
plot(torn2, xlabel = "MPG", 
     geom_bar_control = list(width = 0.5),
     sensitivity_colors = c("#E41A1C", "#377EB8"),
     geom_point_control = list(fill = "red", col = "red", size = 3))
```

![](man/figures/glm_factor_section-1.png)

### Censored Data

``` r
survreg3 <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx + age + resid.ds, 
                              survival::ovarian, dist = 'weibull', scale = 1)
torn3 <- tornado(survreg3, modeldata = survival::ovarian, type = "PercentChange", alpha = 0.10)
plot(torn3, xlabel = "Survival Time", geom_bar_control = list(width = 0.5))
```

![](man/figures/censored_section-1.png)

``` r
coxph4 <- survival::coxph(survival::Surv(stop, event) ~ rx + size + number,
                          survival::bladder)
torn4 <- tornado(coxph4, modeldata = survival::bladder, type = "PercentChange",
                 alpha = 0.10)
plot(torn4, xlabel = "Risk", geom_bar_control = list(width = 0.5))
```

![](man/figures/censored_section-2.png)

### Machine Learning Models from caret::train

The `plot` method can also return a ggplot object

``` r
  mydat <- mtcars
  mydat$am <- factor(mydat$am)
  mydat$vs <- factor(mydat$vs)
  mydat$cyl <- factor(mydat$cyl)
  mydat$gear <- factor(mydat$gear)
  mydat$carb <- factor(mydat$carb)

  gtest <- caret::train(x = subset(mydat, select = -vs), y = mydat$vs, method = "rf")
  torn <- tornado(gtest, type = "percentiles", alpha = 0.10, class_number = 1)
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class V-shaped Engine")
  g <- g + ggtitle("Classifier caret::train randomforest, 90-110% of the mean in each variable")
  plot(g)
```

![](man/figures/train_section-1.png)
