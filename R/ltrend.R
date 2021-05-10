########################## Linear tends ##########################

# Function get_gls_reml
# Returns list of  summaries of gls regression models estimated with REML method
get_gls_reml <- function(wide_t_df){
  require(nlme)
  year <- wide_t_df[, 1]
  model_list <- apply(wide_t_df[, 2: ncol(wide_t_df)], 2, function(x) gls(x~year, correlation = corAR1(form=~year)))
  model_sum_list <- lapply(model_list, function(x) summary(x))
  return(model_sum_list)
}

gls_regcoef_and_se <- function(model_sum_list){
  reg_coeff_and_se <- t(sapply(1:length(model_sum_list), function(x) round(model_sum_list[[x]]$tTable[2, 1:2], 2)))
  colnames(reg_coeff_and_se) <- c("B", "SE_B")
  return(reg_coeff_and_se)
}

# Function compare_gls_ml
# Compares two gls models estimated with ML methods through anova
# Returns list of the results of the comparison
compare_gls_ml <- function(wide_t_df, scaled = FALSE){
  require(nlme)
  year <- wide_t_df[, 1]

    model_ML_list <- apply(wide_t_df[, 2: ncol(wide_t_df)], 2, function(x) gls(x~year, correlation = corAR1(form=~year), method = "ML"))
    model_ML0_list <- apply(wide_t_df[, 2: ncol(wide_t_df)], 2, function(x) gls(x~1, correlation = corAR1(form=~year), method = "ML"))
    model_list_anova <- lapply(1:length(model_ML_list), function(x) anova(model_ML_list[[x]], model_ML0_list[[x]]))

    return(model_list_anova)
}

compare_gls_p_val <- function(model_list_anova){
  p_val <- sapply(1:length(model_list_anova), function(x) round(model_list_anova[[x]]$"p-value"[2], 3))
  return(p_val)
}

# Function get_gls_reml_scaled
# Returns list of  summaries of gls regression models estimated with REML method

get_gls_reml_scaled <- function(wide_t_df){
  require(nlme)
  year <- wide_t_df[, 1]
  model_list_scaled <- apply(wide_t_df[, 2: ncol(wide_t_df)], 2, function(x) gls(scale(x)~scale(year), correlation = corAR1(form=~year)))
  model_sum_list_scaled <- lapply(model_list_scaled, function(x) summary(x))
  return(model_sum_list_scaled)
}

gls_regcoef_and_se_scaled <- function(model_sum_list_scaled){
  reg_coeff_and_se <- t(sapply(1:length(model_sum_list_scaled), function(x) round(model_sum_list_scaled[[x]]$tTable[2, 1:2], 2)))
  colnames(reg_coeff_and_se) <- c("B_scaled", "SE_B_scaled")
  return(reg_coeff_and_se)
}

#' @title ltrend
#' @param data_file Name of .csv file with data. No defaults.
#' @param data_file_2 Name of second .csv file to be united with the first one.
#' It must be checked, that the data are from the SAME period.
#' Default to NULL
#' @param number_of_plots Number of 1square meter plots in sampled area. No defaults
#' @param scaled Boolean. Whether to carry out calculations based on scaled values. Default to TRUE.
#' @param need_abbr Boolean. Whether species names should be abbreviated. Default to FALSE.
#' @param state Character. "g" - generative, "v" - vegetative, "v+j" - vegetative and juvenile.
#'  Which states should be selected. If NULL, all shoots will be selected.
#'  Default to NULL.
#' @param mean_threshold Number. Mean shoot number threshold
#'  for species to be taken into consideration. Default to 3.2.
#' @param pVal Number (0.05, 0.01 or less). p-value of ANOVA comparing two gls models compared with ML methods
#'  for species to be taken into consideration. Default to 0.05.
#' @return dataframe with linear trends parameters
#' @export

ltrend <- function(data_file,
                   data_file_2 = NULL,
                   number_of_plots,
                   per_sample = c("one_sq_m", "ten_sq_m", "whole_plot"),
                   scaled = TRUE,
                   need_abbr = FALSE,
                   state = NULL,
                   threshold_mean = 10,
                   pVal = 0.05,
                   ...){
  # read data
  wide_t_df <- get_tidy_data(data_file, data_file_2, need_abbr, state, ...)

  # get mean number of shoots:
  per_sample <- match.arg(per_sample)
  switch(per_sample,
         "one_sq_m" = {
           mean_sh_num <- get_mean_shoot_number_1(wide_t_df, number_of_plots)
           sd_sh_num <- get_sd_sh_num_1(wide_t_df, number_of_plots)
           mean_sd_shoot_num <- get_mean_sd_shoot_number_1_fl(wide_t_df, number_of_plots)
         },
         "ten_sq_m" = {
           mean_sh_num <- get_mean_shoot_number_10(wide_t_df, number_of_plots)
           sd_sh_num <- get_sd_sh_num_10(wide_t_df, number_of_plots)
           mean_sd_shoot_num <- get_mean_sd_shoot_number_10_fl(wide_t_df, number_of_plots)
         },
         "whole_plot" = {
           mean_sh_num <- get_mean_shoot_number(wide_t_df, number_of_plots)
           sd_sh_num <- get_sd_sh_num(wide_t_df, number_of_plots)
           mean_sd_shoot_num <- get_mean_sd_shoot_number_fl(wide_t_df, number_of_plots)
         },
         stop("Please, choose between 'one_sq_m', 'ten_sq_m' or 'whole_plot'!")
  )
 # Whether the model should be scaled or not:
    if(scaled == F){
    # get non-scaled linear models
    reml_model_list <- get_gls_reml(wide_t_df)
    b_and_se <- gls_regcoef_and_se(reml_model_list)
  } else {
    # get scaled linear models
    reml_model_list <- get_gls_reml_scaled(wide_t_df)
    b_and_se <- gls_regcoef_and_se_scaled(reml_model_list)
  }

  # get comparison of models over years and without year predictors
  model_list_anova <- compare_gls_ml(wide_t_df)
  anova_p_val <- compare_gls_p_val(model_list_anova)

  # get pivot table about trends
  spec_names <- colnames(wide_t_df)[2:ncol(wide_t_df)]
  trends_pivot_table <- data.frame(Species = spec_names,
                                   Mean = mean_sh_num,
                                   SD = sd_sh_num,
                                   mean_sd_shoot_num,
                                   B = b_and_se[, 1],
                                   B_SE = b_and_se[, 2],
                                   Anova_p_value = anova_p_val)
  trends_pivot_table <- subset(trends_pivot_table, Mean > threshold_mean & Anova_p_value < pVal)

  return(trends_pivot_table)

}
