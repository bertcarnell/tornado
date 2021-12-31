# Copyright 2019 Robert Carnell

#' Create Tab Summary Data
#'
#' @param model_final a glm or lm object
#' @param model_null a glm or lm object for the null model
#' @param dict a dictionary to translate the model variables to plotting vriables
#' @param isDeviance TRUE for glms and other deviance residual based models
#'
#' @importFrom stats add1 anova formula
#' @importFrom assertthat assert_that
#'
#' @return a data.frame
#' @noRd
.create_tab_summary <- function(model_final, model_null, dict=NA, isDeviance=FALSE)
{
  # need a consistent way to get the variable names out of the model object
  # for continuous variables, names(coef()) works fine
  #var_final <- names(coef(model_final))[-1]
  var_final <- names(model_final$model)[-1]

  dict <- .create_dict(dict, var_final)

  tab_summary <- data.frame(vars = var_final,
                            desc = dict$new[match(var_final, dict$old)],
                            stringsAsFactors = FALSE)

  if (isDeviance)
  {
    model_anova <- stats::anova(model_final)[-1, ]
  } else
  {
    model_anova <- stats::anova(model_final)
    model_anova <- model_anova[-nrow(model_anova),]
  }
  tab_contr <- data.frame(vars = rownames(model_anova), stringsAsFactors = FALSE)
  if (isDeviance)
  {
    tab_contr$contr <- model_anova$Deviance / sum(model_anova$Deviance)
  } else
  {
    tab_contr$contr <- model_anova$`Sum Sq` / sum(model_anova$`Sum Sq`)
  }
  ord <- order(tab_contr$contr, decreasing = TRUE)
  tab_contr <- tab_contr[ord,]
  tab_contr$cum_contr <- cumsum(tab_contr$contr)
  tab_contr$cum_contr_from <- with(tab_contr, cum_contr - contr)

  model_add1 <- stats::add1(model_null, stats::formula(model_final))
  if (isDeviance)
  {
    add1_contr <- (model_add1$Deviance[1] - model_add1$Deviance[-1])/sum(model_anova$Deviance)
  } else
  {
    add1_contr <- model_add1$`Sum of Sq`[-1] / sum(model_anova$`Sum Sq`)
  }

  assertthat::assert_that(all(var_final == rownames(model_add1)[-1]),
                          msg = "Unexpected Internal error")
  dev_add1 <- data.frame(vars = var_final, add1 = add1_contr,
                         stringsAsFactors = FALSE)

  tab_summary <- merge(tab_contr, tab_summary, all.x = TRUE)
  tab_summary <- merge(tab_summary, dev_add1, all.x = TRUE)
  tab_summary <- tab_summary[order(tab_summary$contr, decreasing = TRUE), ]
  tab_summary <- within(tab_summary, desc <- ifelse(is.na(desc), vars, desc))
  tab_summary$add1 <- with(tab_summary, ifelse(is.na(add1), 0, add1))
  return(tab_summary)
}

#' Common Code for importance plots
#'
#' @param tab_summary the plotting data.frame
#'
#' @return the data for plotting
#' @noRd
.create_common_importance_data <- function(tab_summary)
{
  lvar <- nrow(tab_summary)

  importance_alone_length <- with(tab_summary, c(0, add1[-1]))
  space_between_length <- with(tab_summary, c(0, cum_contr_from[-1] - add1[-1]))
  cum_importance_length <- with(tab_summary, cum_contr - space_between_length -
                                  importance_alone_length)
  ind <- which(space_between_length < 0)
  if (length(ind) > 0)
  {
    importance_alone_length[ind] <- importance_alone_length[ind] + space_between_length[ind]
    space_between_length[ind] <- 0
  }
  dat2 <- data.frame(variable = rep(tab_summary$desc, times = 3),
                     posit = rep(c("Alone","Space","Cum"), each = lvar),
                     value = c(importance_alone_length,
                               space_between_length,
                               cum_importance_length))
  dat2$posit <- factor(dat2$posit, levels = c("Cum", "Space", "Alone"))
  dat2$variable <- factor(dat2$variable, levels = rev(tab_summary$desc))

  return(dat2)
}
