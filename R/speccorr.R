#' @title Simple formatting of a correlation matrix
#' @description This function provides a simple formatting of a correlation matrix
#' into a table with 4 columns containing :
#' Column 1 : row names (variable 1 for the correlation test)
#' Column 2 : column names (variable 2 for the correlation test)
#' Column 3 : the correlation coefficients
#' Column 4 : the p-values of the correlations

flat_cor_mat <- function(cor_r, cor_p){
  cor_r <- tibble::rownames_to_column(as.data.frame(cor_r), var = "row")
  cor_r <- tidyr::gather(cor_r, column, cor, -1)
  cor_p <- tibble::rownames_to_column(as.data.frame(cor_p), var = "row")
  cor_p <- tidyr::gather(cor_p, column, p, -1)
  cor_p_matrix <- dplyr::left_join(cor_r, cor_p, by = c("row", "column"))
  names(cor_p_matrix) <- c("spec1", "spec2", "r", "p.value")
  return(cor_p_matrix)
}

#' @title speccorr
#' @description Correlations between species.
#' Helper function `flat_cor_mat` comes from
#' https://rstudio-pubs-static.s3.amazonaws.com/240657_5157ff98e8204c358b2118fa69162e18.html
#' @param data_file Name of .csv file with data. No defaults.
#' @param data_file_2 Name of second .csv file to be united with the first one.
#' It must be checked, that the data are from the SAME period.
#' Default to NULL
#' @param need_abbr Boolean. Whether species names should be abbreviated. Default to FALSE.
#' @param state Character. "g" - generative, "v" - vegetative, "v+j" - vegetative and juvenile.
#'  Which states should be selected. If NULL, all shoots will be selected.
#'  Dafault to NULL.
#'  @param type Type of correlation coefficient ("pearson" or "spearman").
#'  Default to "pearson".
#' @param pVal Number (0.05, 0.01 or less). p.value of corr.test
#'  for species to be taken into consideration. Default to 0.05.
#' @return Data frame with correlation coefficients and their p.values between species
#' @export

speccorr <- function(data_file,
                     data_fie2 = NULL,
                     need_abbr = FALSE,
                     state = NULL,
                     type = "pearson",
                     pVal = 0.05){
  df_data <- get_tidy_data(data_file, data_fie2, need_abbr, state)
  df_data <- df_data[, -1]
  if(type != "pearson"){
    corr_matrix <- Hmisc::rcorr(as.matrix(df_data), type = "spearman")
  } else {
    corr_matrix <- Hmisc::rcorr(as.matrix(df_data))
    }
  corr_matrix_r <- corr_matrix$r
  corr_matrix_p <- corr_matrix$P
  table_corr_matrix <- flat_cor_mat(corr_matrix_r, corr_matrix_p)
  table_corr_matrix <- table_corr_matrix %>% filter(p.value < pVal)
  table_corr_matrix <- distinct(table_corr_matrix)
  table_corr_matrix <- table_corr_matrix[!duplicated(table_corr_matrix[,c('r', 'p.value')]),]
  return(table_corr_matrix)
}
