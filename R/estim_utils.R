################### Means and Coeffitiets of Variance and Standard deviation ##############################

# Function get_mean_shoot_number_10
# Takes data frame without zeros (without_zeros_df)
#   and number of 1-square-meter plots for the phytocenosis of question
# Returns means for shoot numbers per 10 m2 for all observation period
get_mean_shoot_number_10 <- function(without_zeros_df, number_of_plots){
  mean_sh_num_10 <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(mean(x*10/number_of_plots), 2))
  return(mean_sh_num_10) # returns average number of shoots for 10 m2
}

get_mean_shoot_number_1 <- function(without_zeros_df, number_of_plots){
  mean_sh_num_1 <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(mean(x/number_of_plots), 2))
  return(mean_sh_num_1) # returns average number of shoots for 1 m2
}

get_mean_shoot_number <- function(without_zeros_df, number_of_plots){
  mean_sh_num <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(mean(x), 2))
  return(mean_sh_num) # returns average number of shoots for the whole plot
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
get_mean_sd_shoot_number_1_fl <- function(without_zeros_df, number_of_plots){
  first3 <- without_zeros_df[1:3,]
  last3 <- without_zeros_df[(nrow(without_zeros_df)-2):nrow(without_zeros_df),]
  mean_sh_num_1_first <- apply(first3[, 2:ncol(without_zeros_df)], 2, function(x) round(mean(x/number_of_plots), 2))
  sd_sh_num_1_first <- apply(first3[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(x/number_of_plots), 2))
  mean_sh_num_1_last <- apply(last3[, 2:ncol(without_zeros_df)], 2, function(x) round(mean(x/number_of_plots), 2))
  sd_sh_num_1_last <- apply(last3[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(x/number_of_plots), 2))
  f3_l3_df <- data.frame(Mean_first_3y = mean_sh_num_1_first,
                         SD_first_3y = sd_sh_num_1_first,
                         Mean_last_3y = mean_sh_num_1_last,
                         SD_last_3y = sd_sh_num_1_last)
  return(f3_l3_df)
}

get_mean_sd_shoot_number_fl <- function(without_zeros_df, number_of_plots){
  first3 <- without_zeros_df[1:3,]
  last3 <- without_zeros_df[(nrow(without_zeros_df)-2):nrow(without_zeros_df),]
  mean_sh_num_first <- apply(first3[, 2:ncol(without_zeros_df)], 2, function(x) round(mean(x), 2))
  sd_sh_num_first <- apply(first3[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(x), 2))
  mean_sh_num_last <- apply(last3[, 2:ncol(without_zeros_df)], 2, function(x) round(mean(x), 2))
  sd_sh_num_last <- apply(last3[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(x), 2))
  f3_l3_df <- data.frame(Mean_first_3y = mean_sh_num_first,
                         SD_first_3y = sd_sh_num_first,
                         Mean_last_3y = mean_sh_num_last,
                         SD_last_3y = sd_sh_num_last)
  return(f3_l3_df)
}

# Functions of the group get_sd_sh_num*
get_sd_sh_num_10 <- function(without_zeros_df, number_of_plots){
  sd_sh_num_10 <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(x*10/number_of_plots), 2))
  return(sd_sh_num_10) # returns standard deviation for 10 m2
}

get_sd_sh_num_1 <- function(without_zeros_df, number_of_plots){
  sd_sh_num_1 <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(x/number_of_plots), 2))
  return(sd_sh_num_1) # returns standard deviation for 1 m2
}

get_sd_sh_num <- function(without_zeros_df, number_of_plots){
  sd_sh_num <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(x), 2))
  return(sd_sh_num) # returns standard deviation for the whole plot
}


# Functions of the group get_CV_shoot_number*
# Take data frame without zeros (without_zeros_df)
#   and number of 1-square-meter plots for the phytocenosis of question
# Returns variation coefficients for shoot numbers per 10 m2, 1m2 or per whole plot
get_CV_shoot_number_10 <- function(without_zeros_df, number_of_plots){
  CV_sh_num_10 <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(10*x/number_of_plots)/mean(x*10/number_of_plots), 2))
  return(CV_sh_num_10)
}

get_CV_shoot_number_1 <- function(without_zeros_df, number_of_plots){
  CV_sh_num_1 <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(x/number_of_plots)/mean(x/number_of_plots), 2))
  return(CV_sh_num_1)
}


get_CV_shoot_number <- function(without_zeros_df, number_of_plots){
  CV_sh_num <- apply(without_zeros_df[, 2:ncol(without_zeros_df)], 2, function(x) round(sd(x)/mean(x), 2))
  return(CV_sh_num)
}

