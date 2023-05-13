#' Create a coefficient plot manually
#'
#' Takes a data frame with one or more models row-binded together and creates
#' a coefficient plot from it. Based on jtools but takes data frame input to
#' be flexible to different model inputs
#'
#' @param df a data frame of model output(s) with the following columns: Estimate, term, model
#' @param model_names a vector of names matching the names in the 'model' column of the df
#' @param color_vals a vector of color values that is the same length as the model names
#' @param shape_vals a vector of shape values that is the same length as the model names
#' @param legend_nrow a numeric value for the number of rows the legend should span
#'
#' @return A coefficient plot
#'
#' @examples patents <- read_patents(file_dir = "patent_files/")
#'
#' @import ggplot2
#' @import ggstance
#' @import magrittr
#' @import jtools
#'
#'
#' @export


manual_coef_plot <- function(df, model_names, color_vals, shape_vals, legend_nrow){
  p <- df %>%
    ggplot(aes(x = Estimate,
               y = factor(term, rev(unique(.$term))))) +
    ggstance::geom_pointrangeh(aes(xmin = LCI,
                                   xmax = UCI,
                                   color = factor(model, rev(model_names)),
                                   shape = factor(model, rev(model_names))),
                               position = ggstance::position_dodgev(height = .8),
                               fill = "white", fatten = 2, size = 0.5) +
    #geom_errorbar(aes(xmin=`conf.high`,xmax=`conf.low`), width=0.1, position = pd) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
    scale_colour_manual(values = color_vals,
                        limits = model_names) +
    scale_shape_manual(values = shape_vals,
                       limits = model_names) +
    jtools::theme_nice(base_family = "Times") +
    jtools::drop_y_gridlines() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_text(size = 8),
          text = element_text(size = 10),
          legend.text = element_text(size = 6),
          legend.position = "bottom",
          legend.justification = 1,
          panel.grid.major.x = element_line(linetype = "solid")) +
    labs(x = "Coefficient Estimate", shape = "", color = "") +
    guides(color=guide_legend(nrow=legend_nrow,byrow=TRUE),
           shape=guide_legend(nrow=legend_nrow,byrow=TRUE))
  return(p)
}
