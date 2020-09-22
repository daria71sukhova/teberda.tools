################### Means and Coeffitiets of Variance and Standard deviation ##############################

# Function get_mean_shoot_number_10
# Takes data frame without zeros (without_zeros_df)
#   and number of 1-square-meter plots for the phytocenosis of question
# Returns means for shoot numbers per 10 m2 for all observation period
get_mean_shoot_number_10 <- function(without_zeros_df, number_of_plots){
  mean_sh_num_10 <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(mean(x*10/number_of_plots), 2))
  return(mean_sh_num_10)
}

get_mean_sd_shoot_number_10_fl <- function(without_zeros_df, number_of_plots){
  first3 <- without_zeros_df[1:3,]
  last3 <- without_zeros_df[(nrow(without_zeros_df)-2):nrow(without_zeros_df),]
  mean_sh_num_10_first <- apply(first3[, 2:ncol(without_zeros_df)], 2, function(x) round(mean(x*10/number_of_plots), 2))
  sd_sh_num_10_first <- apply(first3[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(x*10/number_of_plots), 2))
  mean_sh_num_10_last <- apply(last3[, 2:ncol(without_zeros_df)], 2, function(x) round(mean(x*10/number_of_plots), 2))
  sd_sh_num_10_last <- apply(last3[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(x*10/number_of_plots), 2))
  f3_l3_df <- data.frame(Mean_first_3y = mean_sh_num_10_first,
                         SD_first_3y = sd_sh_num_10_first,
                         Mean_last_3y = mean_sh_num_10_last,
                         SD_last_3y = sd_sh_num_10_last)
  return(f3_l3_df)
}

# Function get_sd_sh_num_10
get_sd_sh_num_10 <- function(without_zeros_df, number_of_plots){
  sd_sh_num_10 <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(x*10/number_of_plots), 2))
  return(sd_sh_num_10)
}


# function get_CV_shoot_number_10
# Takes data frame without zeros (without_zeros_df)
#   and number of 1-square-meter plots for the phytocenosis of question
# Returns variation coeffitients for shoot numbers per 10 m2
get_CV_shoot_number_10 <- function(without_zeros_df, number_of_plots){
  CV_sh_num_10 <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(10*x/number_of_plots)/mean(x*10/number_of_plots), 2))
  return(CV_sh_num_10)
}
