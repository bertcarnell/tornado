# Copyright 2019 Robert Carnell

#' Plot an Importance Plot object
#'
#' @param x a \code{importance_plot} object
#' @param plot boolean to determine if the plot is displayed, or just returned
#' @param nvar the number of variables to plot in order of importance
#' @param col_imp_alone the color used for the variance explained by each variable
#' alone
#' @param col_imp_cumulative the color used for the cumulative variance explained
#' @param geom_bar_control list of arguments to control the plotting of \code{ggplot2::geom_bar}
#' @param ... future arguments
#'
#' @return the plot
#' @export
#'
#' @method plot importance_plot
#' @import ggplot2
#' @importFrom grid rectGrob grid.newpage grid.draw gpar unit.c unit
#' @importFrom gridExtra arrangeGrob
#' @importFrom scales percent
#' @importFrom grDevices dev.cur dev.off
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#'
#' @examples
#' gtest <- lm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars)
#' gtestreduced <- lm(mpg ~ 1, data = mtcars)
#' imp <- importance(gtest, gtestreduced)
#' plot(imp)
#'
#' gtest <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps*rx + age,
#'                            data = survival::ovarian,
#'                            dist = "weibull")
#' imp <- importance(gtest, survival::ovarian, nperm = 50)
#' plot(imp)
plot.importance_plot <- function(x, plot = TRUE, nvar = NA,
                                 col_imp_alone = "#69BE28",
                                 col_imp_cumulative = "#427730",
                                 geom_bar_control = list(fill = "#69BE28"),
                                 ...)
{
  assertthat::assert_that(length(nvar) == 1,
                         msg = "nvar must be a length 1 integer or NA")

  if (x$type %in% c("lm", "glm"))
  {
    if (all(!is.na(nvar)) & nvar <= nrow(x$data)/3)
    {
      temp <- x$data[x$data$posit == "Cum",]
      ind <- which(x$data$variable %in% temp$variable[1:nvar])
      x$data <- x$data[ind,]
    }
  } else
  {
    if (all(!is.na(nvar)) & nvar <= nrow(x$data))
      x$data <- x$data[1:nvar,]
  }

  if (x$type %in% c("lm", "glm"))
  {
    # if geom_bar_control contains fill, then delete it
    ind <- which(names(geom_bar_control) == "fill")
    original_geom_length <- length(geom_bar_control)
    original_fill <- geom_bar_control$fill
    if (length(ind) >= 1)
    {
      geom_bar_control <- geom_bar_control[-ind]
      # if fill was the default and the only entry skip, otherwise, warn
      if (length(ind) == 1 && original_geom_length == 1 && original_fill == "#69BE28")
      {
        # do nothing
      } else {
        warning("geom_bar_control fill argument is not used with importance_plot with type lm or glm")
      }
    }

    ggp <- ggplot(x$data, aes(x = .data$variable, y = .data$value, fill = .data$posit)) + #,
      #alpha = "posit")) +
      do.call(geom_bar, args = c(list(stat = "identity"), geom_bar_control)) +
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
    if (x$type == "glm")
    {
      ggp <- ggp + ylab("Percent of Model Deviance Explained (Importance)")
      cats_for_dat3 <- factor(c("Marginal Deviance Explained", "Cumulative Deviance Explained"))
    } else
    {
      ggp <- ggp + ylab("Percent of Model Variance Explained (Importance)")
      cats_for_dat3 <- factor(c("Marginal Variance Explained", "Cumulative Variance Explained"))
    }
    dat3 <- data.frame(
      cats = cats_for_dat3,
      heights = c(1, 2),
      cols = factor(c("A", "B")),
      stringsAsFactors = FALSE)

    # create a dummy legend
    ggp2 <- ggplot2::ggplot(dat3,
                            aes(x = .data$cats, y = .data$heights, fill = .data$cats)) +
      do.call(geom_bar, args = c(list(stat = "identity"), geom_bar_control)) +
      scale_fill_manual(values = c(col_imp_cumulative, col_imp_alone)) +
      theme(legend.title = element_blank(), legend.position = "bottom")

    # ggplotGrob has a side effect of opening a device if one is not open
    close_after <- (grDevices::dev.cur() == 1 & names(grDevices::dev.cur()) == "null device")
    gt <- ggplot2::ggplotGrob(ggp)
    ggp2_grob <- ggplot2::ggplotGrob(ggp2)
    if (close_after)
      grDevices::dev.off()

    # extract the legend
    legend_grob <- ggp2_grob$grobs[grepl("guide-box", ggp2_grob$layout$name)]
    select <- which(!vapply(legend_grob, inherits, logical(1), "zeroGrob"))[1]
    legend_grob <- legend_grob[[select]]

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
    if (plot)
    {
      grid::grid.newpage()
      grid::grid.draw(combined)
    } else {
      return(combined)
    }

  } else if (x$type == "cv.glmnet")
  {
    ggp <- ggplot(x$data, aes(x = .data$variable, y = .data$value)) +
      do.call(geom_bar, args = c(list(stat = "identity"), geom_bar_control)) +
      coord_flip() +
      theme_bw() +
      xlab("") +
      scale_y_continuous(labels = scales::percent) +
      ylab("Percent Increase in MSE when Variable is permuted (Importance)")

    if (plot) {plot(ggp)} else {return(ggp)}

  } else if (x$type == "survreg")
  {
    ggp <- ggplot(x$data, aes(x = .data$variable, y = .data$value)) +
      do.call(geom_bar, args = c(list(stat = "identity"), geom_bar_control)) +
      coord_flip() +
      theme_bw() +
      xlab("") +
      scale_y_continuous(labels = scales::percent) +
      ylab("Change in Likelihood Ratio when Variable is permuted (Importance)")

    if (plot) {plot(ggp)} else {return(ggp)}

  } else if (x$type == "train")
  {
    ggp <- ggplot(x$data, aes(x = .data$names, y = .data$Overall)) +
      do.call(geom_bar, args = c(list(stat = "identity"), geom_bar_control)) +
      coord_flip() +
      xlab("") +
      ylab("Importance from caret::varImp") +
      theme_bw()

    if (plot) {plot(ggp)} else {return(ggp)}

  } else
  {
    stop("unrecognized importance_plot type in plot.importance_plot")
  }
}
