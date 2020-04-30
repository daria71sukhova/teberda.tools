################### Means and Coeffitiets of Variance ##############################

# Function get_mean_shoot_number_10
# Takes data frame without zeros (without_zeros_df)
#   and number of 1-square-meter plots for the phytocenosis of question
# Returns means for shoot numbers per 10 m2 for all observation period
get_mean_shoot_number_10 <- function(without_zeros_df, number_of_plots){
  mean_sh_num_10 <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(mean(x*10/number_of_plots), 2))
  return(mean_sh_num_10)
}

# function get_CV_shoot_number_10
# Takes data frame without zeros (without_zeros_df)
#   and number of 1-square-meter plots for the phytocenosis of question
# Returns variation coeffitients for shoot numbers per 10 m2
get_CV_shoot_number_10 <- function(without_zeros_df, number_of_plots){
  CV_sh_num_10 <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(10*x/number_of_plots)/mean(x*10/number_of_plots), 2))
  return(CV_sh_num_10)
}

######################### Autocorrelations ##########################

# function get_ts
# Takes without_zeros_df
# Returns sp_ts object with time series for each species
get_ts <- function(without_zeros_df){
  sp_ts <- sapply(2:ncol(without_zeros_df), function(x) ts(without_zeros_df[,x]))
  return(sp_ts)
}

# function get_max_acf
# Takes sp_ts
# Returns maximum of autocorrelation coeffitients with lag != 0
get_max_acf <- function(sp_ts){
  acf_sp_ts <- lapply(1:ncol(sp_ts), function(x) acf(sp_ts[,x], plot=F))
  max_acf_sp_ts <- sapply(1:length(acf_sp_ts), function(x) max(acf_sp_ts[[x]]$acf[acf_sp_ts[[x]]$lag!=0]))
  return(max_acf_sp_ts)
}

# Function get_autocorr_1
get_autocorr_1 <- function(sp_ts){
  acf_sp_ts <- lapply(1:ncol(sp_ts), function(x) acf(sp_ts[,x], plot=F))
  autocor_1 <- sapply(1:length(acf_sp_ts), function(x) acf_sp_ts[[x]][["acf"]][2])
  #eturn(autocor_1)
  return(autocor_1)
}


# function get_most_sign_lag
# Takes sp_ts
# Returns the most significant lags
get_most_sign_lag <- function(sp_ts, max_acf_sp_ts){
  # sp_ts <- sapply(1:ncol(without_zeros_df), function(x) ts(without_zeros_df[,x]))
  acf_sp_ts <- lapply(1:ncol(sp_ts), function(x) acf(sp_ts[,x], plot=F))
  most_sign_lag <- sapply(1:length(max_acf_sp_ts),
                          function(x) acf_sp_ts[[x]]$lag[acf_sp_ts[[x]]$acf == max_acf_sp_ts[x]])
  return(most_sign_lag)
}


# function get_Lij_Box_test
# Takes sp_ts
# Returns list of results of Ljung-Box test
get_Lju_Box_test_pval <- function(sp_ts, most_sign_lag){
  box_result <- lapply(1:ncol(sp_ts), function(x) Box.test(sp_ts[,x],lag=most_sign_lag[x],type="Ljung-Box"))
  box_result_p_val <-sapply(1:length(box_result), function(x) box_result[[x]][["p.value"]])
  return(box_result_p_val)
}

############### Summary tables about autocorrelations #####################

#' @title autocor_max_lag_table
#' @param data_file Name of .csv file with data. No defaults.
#' @param number_of_plots Number of 1square meter plots in sampled area. No defaults
#' @param need_abbr Boolean. Whether species names should be abbreviated. Default to FALSE.
#' @param state Character. "g" - generative, "v" - vegetative, "v+j" - vegetative and juvenile.
#'  Which states should be selected. If NULL, all shoots will be selected.
#'  Dafault to NULL.
#' @param mean_threshold Number. Mean shoot number threshhold
#'  for species to be taken into consideration. Default to 3.2.
#' @param pVal Number (0.05, 0.01 or less). p-value of Ljung-Box test
#'  for species to be taken into consideration. Default to 0.05.
#' @return Data frame with autocorrelation coefficients for maximum lag value,
#' where lag > 1.
#' @export

autocor_max_lag_table <- function(data_file,
                                  data_file_2 = NULL,
                                  number_of_plots,
                                  need_abbr = FALSE,
                                  state = NULL,
                                  maxLag = 1,
                                  mean_threshold = 3.2,
                                  pVal = .05){
  wide_t_df <- get_tidy_data(data_file, data_file_2, need_abbr, state)

  # Estimate variation coeffficient
  mean_sh_num_10 <- get_mean_shoot_number_10(wide_t_df, number_of_plots)
  CV_sh_num_10 <- get_CV_shoot_number_10(wide_t_df, number_of_plots)

  # Estimate autocorrelations
  sp_ts <- get_ts(wide_t_df)
  max_acf_sp_ts <- get_max_acf(sp_ts)
  most_sign_lag <- get_most_sign_lag(sp_ts, max_acf_sp_ts)
  box_result_p_val <- get_Lju_Box_test_pval(sp_ts, most_sign_lag)

  # Make pivot table
  spec_names <- colnames(wide_t_df)[2:ncol(wide_t_df)]
  pivot_table <- data.frame(Species = spec_names,
                            Mean = round(mean_sh_num_10),
                            CV = round(CV_sh_num_10, 2)*100,
                            lag = most_sign_lag,
                            R = round(max_acf_sp_ts, 3),
                            p_value = round(box_result_p_val, 3))
  pivot_table <- subset(pivot_table, Mean > mean_threshold & p_value < pVal & lag > maxLag)
  return(pivot_table)
}

#' @title autocor_1_order_table
#' @param data_file Name of .csv file with data. No defaults.
#' @param number_of_plots Number of 1square meter plots in sampled area. No defaults
#' @param need_abbr Boolean. Whether species names should be abbreviated. Default to FALSE.
#' @param state Character. "g" - generative, "v" - vegetative, "v+j" - vegetative and juvenile.
#'  Which states should be selected. If NULL, all shoots will be selected.
#'  Dafault to NULL.
#' @param mean_threshold Number. Mean shoot number threshhold
#'  for species to be taken into consideration. Default to 3.2.
#' @param pVal Number (0.05, 0.01 or less). p-value of Ljung-Box test
#'  for species to be taken into consideration. Default to 0.05.
#' @return Data frame with autocorrelation coefficients for maximum lag value.
#' @export
autocor_1_order_table  <- function(data_file,
                                   data_file_2 = NULL,
                                   number_of_plots,
                                   need_abbr = FALSE,
                                   state = NULL,
                                   mean_threshold = 3.2,
                                   pVal = .05){
  wide_t_df <- get_tidy_data(data_file, data_file_2, need_abbr, state)
  mean_sh_num_10 <- get_mean_shoot_number_10(wide_t_df, number_of_plots)
  sp_ts <- get_ts(wide_t_df)
  autocorr_lag_1 <- get_autocorr_1(sp_ts)
  box_result <- lapply(1:ncol(sp_ts), function(x) Box.test(sp_ts[,x],
                                                           lag = 1,
                                                           type="Ljung-Box"))
  box_result_p_val <-sapply(1:length(box_result),
                            function(x)
                              box_result[[x]][["p.value"]])
  spec_names <- colnames(wide_t_df)[2:ncol(wide_t_df)]
  pivot_table <- data.frame(Species = spec_names,
                            Mean = round(mean_sh_num_10),
                            lag = 1,
                            R = round(autocorr_lag_1, 3),
                            p_value = round(box_result_p_val, 3)
  )
  pivot_table <- subset(pivot_table, Mean > mean_threshold & p_value < pVal)
  return(pivot_table)
}

#' @title autocorrelations
#' @param data_file Name of .csv file with data. No defaults.
#' @param data_file_2 Name of second .csv file to be united with the first one.
#' It must be checked, that the data are from the SAME period.
#' Default to NULL
#' @param what_autocorr What lag we need. "max_lag" or "order_1". No default.
#' @param number_of_plots Number of 1square meter plots in sampled area. No defaults
#' @param need_abbr Boolean. Whether species names should be abbreviated. Default to FALSE.
#' @param state Character: "g" - generative, "v" - vegetative, "v+j" - vegetative and juvenile.
#'  Which states should be selected. If NULL, all shoots will be selected.
#'  Dafault to NULL.
#' @param maxLag Treshold value of maximum lag. Numeric. Default to 1.
#' @param mean_threshold Mean shoot number threshhold
#'  for species to be taken into consideration. Numeric. Default to 3.2.
#' @param pVal Number (0.05, 0.01 or less). p-value of Ljung-Box test
#'  for species to be taken into consideration. Default to 0.05.
#' @return Dependent on what_autocorr parameter, data frame with autocorrelation coefficients
#'  for maximum lag value or the first order autocorrelation coeffitients.
#' @export
autocorrelations <- function(data_file,
                             data_file_2 = NULL,
                             what_autocorr,
                             number_of_plots,
                             need_abbr = FALSE,
                             state = NULL,
                             maxLag = 1,
                             mean_threshold = 3.2,
                             pVal = .05){
  if(what_autocorr == "max_lag"){ return(autocor_max_lag_table(data_file,
                                                               data_file_2,
                                                               number_of_plots,
                                                               need_abbr,
                                                               state,
                                                               maxLag,
                                                               mean_threshold,
                                                               pVal)) }
  else {
    if(what_autocorr == "order_1"){return(autocor_1_order_table(data_file,
                                      data_file_2,
                                      number_of_plots,
                                      need_abbr,
                                      state,
                                      pVal)) }
  }
}

