########################################################################################
# Correlations between vegetative shoot number of the current year                     #
# and generative shoot number of the past year and vice versa                          #
########################################################################################

# Function get_shapiro_wilk_norm_test
get_shapiro_wilk_norm_test <- function(data_file, need_abbr = FALSE, state = NULL){
  df <- get_tidy_data(data_file, need_abbr, state)
  df <- df[,2:ncol(df)]
  shapiro_wilk_norm_list <- lapply(1:ncol(df), function(x) shapiro.test(df[,x]))
  shapiro_wilk_pVal <- sapply(1:length(shapiro_wilk_norm_list), function(x) shapiro_wilk_norm_list[[x]][["p.value"]])
  sp_name <- colnames(df)
  shapiro_wilk_norm_result <- data.frame(Species = sp_name, Shapiro_Wilk_pValue = shapiro_wilk_pVal)
  shapiro_wilk_norm_result <- subset(shapiro_wilk_norm_result, Shapiro_Wilk_pValue > .05)

  return(shapiro_wilk_norm_result)
}


# Function get_shapiro_wilk_norm_list
get_shapiro_wilk_norm_list <- function(data_file, state = NULL){
  df <- get_tidy_data(data_file)
  df <- df[,2:ncol(df)]
  shapiro_wilk_norm_list <- lapply(1:ncol(df), function(x) shapiro.test(df[,x]))
  return(shapiro_wilk_norm_list)
}


# Function get_corr_test()
# Takes two dataframes with rows of species vegetative and generative shoot numbers
# Returns list of values of correlation tests
get_corr_test <- function(soots_df_g_sp, shoots_df_v_sp, method = "spearman"){
  if(method != "spearman"){
    if(method == "pearson"){
      corr_test_list <- lapply(1:ncol(shoots_df_g_sp),
                               function(x) cor.test(shoots_df_g_sp[, x],
                                                    shoots_df_v_sp[, x],
                                                    method = "pearson"))

      return(corr_test_results)
    } else {
      corr_test_list <- lapply(1:ncol(shoots_df_g_sp),
                               function(x) cor.test(shoots_df_g_sp[, x],
                                                    shoots_df_v_sp[, x],
                                                    method = "kendall"))
      return(corr_test_results)
    }
  } else {
    corr_test_list <- lapply(1:ncol(shoots_df_g_sp),
                             function(x) cor.test(shoots_df_g_sp[, x],
                                                  shoots_df_v_sp[, x],
                                                  method = "spearman"))
    return(corr_test_results)
  }
}


# Function get_R
get_r_coeff <- function(corr_test_list, method = "spearman"){
  if(method != "spearman"){
    if(method == "pearson"){
      r_coeff <- sapply(1:length(corr_test_list), function(x) corr_test_list[[x]][["estimate"]][["cor"]])
      return(r_coeff)
    } else {
      r_coeff <- sapply(1:length(corr_test_list), function(x) corr_test_list[[x]][["estimate"]][["tau"]])
      return(r_coeff)
    }
  } else {
    r_coeff <- sapply(1:length(corr_test_list), function(x) corr_test_list[[x]][["estimate"]][["rho"]])
    return(r_coeff)
  }
}

# Function get_corr_pValue
get_corr_pValue <- function(corr_test_list){
  corr_pVal <- sapply(1:length(corr_test_list), function(x) corr_test_list[[x]][["p.value"]])
  return(corr_pVal)
}


#' @title vggvprinextcorr
#' @description The function estimates correlations between number of vegetative shoots
#' in the prior year and generative shoots of the next year and vise versa.
#' Also the correlations can be estimated for generative and vegetative shoot numbers
#' of the same year (default behavior).
#' @param data_file Name of .csv file with data. No defaults.
#' @param data_file_2 Name of second .csv file to be united with the first one.
#' It must be checked, that the data are from the SAME period.
#' Default to NULL
#' @param comparing_mod Key signs which modification of test should be done.
#'   It can be:
#'     "g_vs_pyv" number of generative shoots with prior-year number of vegetative shoots
#'     "v_vs_pyg" number of vegetative shoots with prior-year number of generative shoots
#'     "v_vs_g"   number of vegetative shoots with number of generative shoots
#'      of the same year.
#'      Default to "v_vs_g".
#' @param method The method of estimation of correlation coeffitients.
#'  Can be "spearman", "pearson" or "kendall". Default to "spearman".
#' @param pVal Number (0.05, 0.01 or less). p.value of corr.test
#'  for species to be taken into consideration. Default to 0.05.
#' @return Data frame with correlation coefficients.
#' @export
vggvprinextcorr <- function(data_file, data_file_2 = NULL, comparing_mod = "v_vs_g", method = "spearman", pVal = .05){
  # data_file is .csv file with numbers of shoots
  # comparing_mode is key signs which modification of test should be done.
  # It can be:
  #     "g_vs_pyv" number of generative shoots with past-year number of vegetative shoots
  #     "v_vs_pyg" number of vegetative shoots with past-year number of generative shoots
  #     "v_vs_g"   number of vegetative shoots with number of generative shoots of the same year (default)
  # method can be "spearman" (default),
  #               "pearson",
  #               "kendall"

  # preparations
  # generative shoots
  df_g <- get_tidy_data(data_file, data_file_2, state = "g")
  df_g_sp <- df_g[, 2:ncol(df_g)]

  # vegetative shoots
  df_v <- get_tidy_data(data_file, data_file_2, state = "v")
  df_v_sp <- df_v[, 2:ncol(df_v)]

  # ajasting lists of species
  # for vetetative
  g_names <- which(colnames(df_v_sp) %in% colnames(df_g_sp))
  df_v_sp <- df_v_sp %>% select(g_names)

  # for generative
  v_names <- which(colnames(df_g_sp) %in% colnames(df_v_sp))
  df_g_sp <- df_g_sp %>% select(v_names)

  if(comparing_mod != "v_vs_g"){
    if(comparing_mod == "g_vs_pyv"){
      # rows of shoot numbers to be compared
      shoots_df_v_sp <- df_v_sp[1:nrow(df_v_sp)-1,]
      shoots_df_g_sp <- df_g_sp[2:nrow(df_g_sp),]
      if (method != "spearman"){
        if (method == "pearson"){
          corr_test_list <- lapply(1:ncol(shoots_df_g_sp),
                                   function(x) cor.test(shoots_df_g_sp[, x],
                                                        shoots_df_v_sp[, x],
                                                        method = "pearson"))
        } else {
          corr_test_list <- lapply(1:ncol(shoots_df_g_sp),
                                   function(x) cor.test(shoots_df_g_sp[, x],
                                                        shoots_df_v_sp[, x],
                                                        method = "kendall"))
        }
      } else{
        corr_test_list <- lapply(1:ncol(shoots_df_g_sp),
                                 function(x) cor.test(shoots_df_g_sp[, x],
                                                      shoots_df_v_sp[, x],
                                                      method = "spearman"))
      }

      r_coefficient <- get_r_coeff(corr_test_list, method) # for table with results
      pValue <- get_corr_pValue(corr_test_list) # for table with results

      # get table with results
      spec_names <- colnames(shoots_df_g_sp)
      corr_test_results <- data.frame(Species = spec_names,
                                      R_coefficient = round(r_coefficient, 3),
                                      pValue = round(pValue, 3))
      corr_test_results <- subset(corr_test_results, pValue < pVal)
      return(corr_test_results)
    } else {
      # rows of shoot numbers to be compared
      shoots_df_v_sp <- df_v_sp[2:nrow(df_v_sp),]
      shoots_df_g_sp <- df_g_sp[1:nrow(df_g_sp)-1,]

      if (method != "spearman"){
        if (method == "pearson"){
          corr_test_list <- lapply(1:ncol(shoots_df_g_sp),
                                   function(x) cor.test(shoots_df_g_sp[, x],
                                                        shoots_df_v_sp[, x],
                                                        method = "pearson"))
        } else {
          corr_test_list <- lapply(1:ncol(shoots_df_g_sp),
                                   function(x) cor.test(shoots_df_g_sp[, x],
                                                        shoots_df_v_sp[, x],
                                                        method = "kendall"))
        }

      } else{
        corr_test_list <- lapply(1:ncol(shoots_df_g_sp),
                                 function(x) cor.test(shoots_df_g_sp[, x],
                                                      shoots_df_v_sp[, x],
                                                      method = "spearman"))
      }

      r_coefficient <- get_r_coeff(corr_test_list, method)
      pValue <- get_corr_pValue(corr_test_list)

      # get table with results
      spec_names <- colnames(shoots_df_g_sp)
      corr_test_results <- data.frame(Species = spec_names,
                                      R_coefficient = round(r_coefficient, 3),
                                      pValue = round(pValue, 3))
      corr_test_results <- subset(corr_test_results, pValue < pVal)
      return(corr_test_results)
    }
  } else {
    # rows of shoot numbers to be compared
    shoots_df_v_sp <- df_v_sp
    shoots_df_g_sp <- df_g_sp

    if (method != "spearman"){
      if (method == "pearson"){
        corr_test_list <- lapply(1:ncol(shoots_df_g_sp),
                                 function(x) cor.test(shoots_df_g_sp[, x],
                                                      shoots_df_v_sp[, x],
                                                      method = "pearson"))
      } else {
        corr_test_list <- lapply(1:ncol(shoots_df_g_sp),
                                 function(x) cor.test(shoots_df_g_sp[, x],
                                                      shoots_df_v_sp[, x],
                                                      method = "kendall"))
      }
    } else{
      corr_test_list <- lapply(1:ncol(shoots_df_g_sp),
                               function(x) cor.test(shoots_df_g_sp[, x],
                                                    shoots_df_v_sp[, x],
                                                    method = "spearman"))
    }

    r_coefficient <- get_r_coeff(corr_test_list, method)
    pValue <- get_corr_pValue(corr_test_list)

    # get table with results
    spec_names <- colnames(shoots_df_g_sp)
    corr_test_results <- data.frame(Species = spec_names,
                                    R_coefficient = round(r_coefficient, 3),
                                    pValue = round(pValue, 3))
    corr_test_results <- subset(corr_test_results, pValue < pVal)
    return(corr_test_results)
  }
}

