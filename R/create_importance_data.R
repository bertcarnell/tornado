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

  if (length(dict) == 1 && is.na(dict))
  {
    dict <- data.frame(Orig.Node.Name = var_final,
                       Description.for.Presentation = var_final,
                       stringsAsFactors = FALSE)
    if (any(grepl("x_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_", dict$Orig.Node.Name)))
    {
      dict$Description.for.Presentation <- gsub("[xX]_[0-9][0-9]_[0-9][0-9]_[0-9][0-9]_", "", dict$Description.for.Presentation)
    }
  } else
  {
    assertthat::assert_that(all(names(dict) == c("Orig.Node.Name",
                                                 "Description.for.Presentation")))
    assertthat::assert_that(all(var_final %in% dict$Orig.Node.Name))
  }

  tab_summary <- data.frame(vars = var_final,
                            desc = dict$Description.for.Presentation[match(var_final, dict$Orig.Node.Name)],
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
#' @param isDeviance is the model deviance residual based
#' @param col_imp_alone the color used for the variance explained by each variable
#' alone
#' @param col_imp_cumulative the color used for the cumulative variance explained
#'
#' @import ggplot2
#' @importFrom grid rectGrob grid.newpage grid.draw gpar unit.c unit
#' @importFrom gridExtra arrangeGrob
#' @importFrom scales percent
#' @importFrom grDevices dev.cur dev.off
#'
#' @return a plotting object
#' @noRd
.create_common_importance_plot <- function(tab_summary, isDeviance=FALSE,
                                           col_imp_alone = "#69BE28",
                                           col_imp_cumulative = "#427730")
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
  #69BE28 = light green
  #427730 = dark green

  ggp <- ggplot(dat2, aes_string(x = "variable", y = "value", fill = "posit")) + #,
                                 #alpha = "posit")) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_bw() +
    # create legend and turn it off
    scale_fill_manual(values = list("Alone" = col_imp_alone,
                                    "Space" = "white",
                                    "Cum" = col_imp_cumulative),
                      guide = "none") +
    # make the middle bar transparent
    #scale_alpha_manual(values = list("Alone" = 1, "Space" = 0, "Cum" = 1),
    #                   guide = "none") +
    xlab("") +
    scale_y_continuous(labels = scales::percent)
  if (isDeviance)
  {
    ggp <- ggp + ylab("Percent of Model Deviance Explained (Importance)")
    dat3 <- data.frame(
      cats = factor(c("Marginal Deviance Explained", "Cumulative Deviance Explained")),
      heights = c(1, 2),
      cols = factor(c("A", "B")),
      stringsAsFactors = FALSE)
  } else
  {
    ggp <- ggp + ylab("Percent of Model Variance Explained (Importance)")
    dat3 <- data.frame(
      cats = factor(c("Marginal Variance Explained", "Cumulative Variance Explained")),
      heights = c(1, 2),
      cols = factor(c("A", "B")),
      stringsAsFactors = FALSE)
  }

  # create a dummy legend
  ggp2 <- ggplot2::ggplot(dat3,
                          aes_string(x = "cats", y = "heights", fill = "cats")) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c(col_imp_cumulative, col_imp_alone)) +
    theme(legend.title = element_blank(), legend.position = "bottom")

  # ggplotGrob has a side effect of opening a device if one is not open
  close_after <- (grDevices::dev.cur() == 1 & names(grDevices::dev.cur()) == "null device")
  gt <- ggplot2::ggplotGrob(ggp)
  ggp2_grob <- ggplot2::ggplotGrob(ggp2)
  if (close_after)
    grDevices::dev.off()

  # extract the legend
  legend_grob <- ggp2_grob$grobs[[which(ggp2_grob$layout$name == "guide-box")]]
  # blank grob
  r <- grid::rectGrob(gp = grid::gpar(fill = NA, col = NA))
  # height of legend
  legend_height <- sum(legend_grob$height)
  # width of xlabs moved to ylabs
  vert_axis_label <- gt$grobs[[which(ggp2_grob$layout$name == "ylab-l")]]
  vert_axis_label_width <- sum(vert_axis_label$width)
  # add the plot and legend together
  combined <- gridExtra::arrangeGrob(
    gt, r, legend_grob, layout_matrix = matrix(c(1,1,2,3), nrow = 2, byrow = TRUE),
    heights = grid::unit.c(grid::unit(1, "npc") - legend_height,
                           legend_height),
    widths = grid::unit.c(vert_axis_label_width,
                          grid::unit(1, "npc") - vert_axis_label_width))
  return(combined)
}
