# Functions for visualisation of the data and the results


#' @title Scatterplot for a species
#' @description Plot scatter diagrams of the number of shoots in the years of observation.
#' @param csv_high Name of .csv file with data. No defaults.
#' @param csv_low Name of second .csv file. Default to NULL
#' @param state Character. "g" - generative, "v" - vegetative, "v+j" - vegetative and juvenile.
#'  Which states should be selected. If NULL, all shoots will be selected.
#'  Dafault to NULL.
#' @param tr_line What trend line method should be applied to the charts. Default to lm.
#' @param plot_title The main title of the plot. No default.
#' @param mute_ax_lab Boolean. Whether we should mute axis names to each indivdual scatterplot.
#' Default to TRUE.
#' @return Scatterplot with the trend line
#' @export


shoots_scatter_plot <- function(
  csv_high,
  csv_low = NULL,
  sp_name,
  state = NULL,
  tr_line = lm,
  plot_title = sp_name,
  mute_ax_lab = TRUE,
  scaled = FALSE,
  ...){
  # Takes dataframe
  # chooses columns "year" and species name
  # returns the scatterplot with a trend line
  shoots <- get_tidy_data(csv_high, csv_low, state = state)

  # choose data about a certain species
  shoots <- shoots %>% select(year, sp_name)

  # make plots for every state
  sh_scat_plot <- ggplot(shoots, aes(x = year, y = ifelse(scaled, scale(shoots[, 2]), shoots[, 2]))) +
    geom_point() +
    geom_smooth(method = tr_line) +
    labs(title = plot_title, x = "Year", y = "Number of shoots")
  if(mute_ax_lab == TRUE){
    sh_scat_plot <- sh_scat_plot +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  }

  return(sh_scat_plot)
}

# Put several plots into the object list
list_spec_plots <- function(sp_name_list,
                            csv_high,
                            csv_low = NULL,
                            state = NULL,
                            tr_line = lm,
                            plot_title = NULL,
                            ...){
  sp_pl_list <- lapply(sp_name_list,
                       shoots_scatter_plot,
                       csv_high = csv_high,
                       csv_low = csv_low,
                       state = state,
                       tr_line = tr_line,
                       ...)
  sp_pl_list <- sp_pl_list
  return(sp_pl_list)
}


# Plot several charts
arrange_plots <- function(plot_list,
                          plot_title,
                          font_face = "bold",
                          font_family = "Arial",
                          main_font_size = 16,
                          axis_font_size = 14){

  shoots_fig <- ggarrange(plotlist = plot_list)

  shoots_fig <- annotate_figure(shoots_fig,
                                top = text_grob(plot_title,
                                                face = font_face,
                                                family = font_family,
                                                size = main_font_size
                                ),
                                bottom = text_grob("Year",
                                                   face = font_face,
                                                   size = axis_font_size),
                                left = text_grob("The number of shoots",
                                                 face = font_face,
                                                 size = axis_font_size,
                                                 rot = 90)
  )
  return(shoots_fig)
}

#' @title Graph the numbers of shoots vs years
#' @description Combines scatterplots for all species in sp_list into one image
#' with common main heading and common axis names
#' @import ggpubr
#' @import cowplot
#' @param sp_name_list A character vector contanes latin names of species in question. No default.
#' @param csv_high Name of .csv file with data. No defaults.
#' @param csv_low Name of second .csv file. Default to NULL
#' @param state Character. "g" - generative, "v" - vegetative, "v+j" - vegetative and juvenile.
#'  Which states should be selected. If NULL, all shoots will be selected.
#'  Dafault to NULL.
#' @param tr_line What trend line method should be applied to the charts. Default to lm.
#' @param plot_title The main title of the plot. No default.
#' @param mute_ax_lab Boolean. Whether we should mute axis names to each indivdual scatterplot.
#' Default to TRUE.
#' @param font_face Default to "bold".
#' @param font_family Default to "Arial".
#' @param main_font_size Default to 16.
#' @param axis_font_size  Default to 14.
#' @return arranged scatterplots
#' @export

# The numbers of shoots. All in one picture
graph_all_shoot_num <- function(sp_name_list,
                               csv_high,
                               plot_title,
                               csv_low = NULL,
                               state = NULL,
                               tr_line = lm,
                               mute_ax_lab = TRUE,
                               font_face = "bold",
                               font_family = "Arial",
                               main_font_size = 16,
                               axis_font_size = 14,
                               ...){
  sp_pl_list <- list_spec_plots(sp_name_list = sp_name_list,
                                csv_high = csv_high,
                                csv_low = csv_low,
                                state = state,
                                tr_line = tr_line,
                                plot_title = plot_title)
  arrange_plots(sp_pl_list,
                plot_title = plot_title,
                font_face = font_face,
                font_family = font_family,
                main_font_size = main_font_size,
                axis_font_size = axis_font_size)

}
