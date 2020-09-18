#' @title check_where()
#' @description This function is for check on what sample certain species present and with what frequency.
#' Also, it can provide when the species was found at the certain sample
#' @import dplyr
#' @param data_file A name of .csv file with the data. No defaults.
#' @param sp_name A full name of the species as it is provided in the data file. No defaults.
#' @param sample_pattern A pattern for a certain sample name. Default to NULL.
#' @return table for species occurens at the samples or data.frame for the species occurence at a certain sample
#' @examples
#' check_where(data_file = "foo.csv", sp_name = ".Leontodon hispidus L.")
#' # returns frequency table for presence at samples
#'
#' check_where(data_file = "foo.csv", sp_name = ".Leontodon hispidus L.", sample_pattern = "^I_2_")
#' # returns data.frame with the species occurence at a certain sample
#'
#' teberda.tools::check_where()
#'
#' @export

check_where <- function(data_file, sp_name, sample_pattern = NULL){
  data <- read.csv(data_file)
  sp_selected <- data %>% filter(species == sp_name)
  if(!is.null(sample_pattern)){
    sp_non_zero_samples <- sp_selected %>% select(matches(sample_pattern))
    sp_non_zero_samples <- sp_non_zero_samples %>% select(names(which(colSums(sp_non_zero_samples) > 0)))
    return(sp_non_zero_samples)
  } else {
    sp_non_zero <- sp_selected %>% select(names(which(colSums(sp_selected[, 3:ncol(sp_selected)]) > 0)))
    return(table(substr(names(sp_non_zero), 0, nchar(names(sp_non_zero)) - 5)))
  }
}

