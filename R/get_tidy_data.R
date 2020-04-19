# Function takes species' names and cleaves dot from its start, if it is.
get_sp_fullnames <- function(sp_name){
  if(!grep("^\\.", sp_name)){
    sp_f_name <- sp_name
  } else {
    sp_f_name <- sub("^\\.", "", sp_name)
  }
  return(sp_f_name)
}


#' This function allows to get abbreviations of species' names
#' @param sp_name A vector of full latin species names with authors. No defaults.
#' @param start1 A number for starting symbol in the abbriviation of the genus. Defaults to 1.
#' @param start2 A number for starting symbol in the abbriviation of the second word. Default to 2.
#' @param end1 A number for ending symbol in the abbriviation of the first word. Default to 4.
#' @param end2 A number for ending symbol in the abbriviation of the second word. Default to 7.
#' @importFrom stringr str_split str_sub
get_abbr <- function(sp_name, start1 = 1, end1 = 3, start2 = 1, end2 = 7){
  spec_abbr <- stringr::str_split(sp_name, " ")
  spec_abbr <- sapply(spec_abbr, stringr::str_sub, start1, end1)
  spec_abbr <- sapply(spec_abbr, paste, collapse = "")
  spec_abbr <- stringr::str_sub(spec_abbr, start2, end2)
  return(spec_abbr)
}

#' Gets wide table with shoot numbers of every state on every sample
#' through the years of observation. Returns a tidy long variant of this table
#' @importFrom reshape2 melt
#' @importFrom dplyr mutate
#' @param wide_table_df A wide table got from raw .csv.
#' get_long_table()
get_long_table <- function(wide_table_df){
  # Start to prapare our long table...
  if("state" %in% names(wide_table_df)){
    long_table <- reshape2::melt(wide_table_df,
                       id.vars = c("species", "state"),
                       measured.vars = grep("_\\d{4}$", names(wide_table_df), value = T)
    )
    # ...and give right names to our columns
    colnames(long_table)[3] <- "sample"
    colnames(long_table)[4] <- "shoot_number"
  }
  else {
    long_table <- reshape2::melt(wide_table_df,
                       id.vars = c("species"),
                       measured.vars = grep("_\\d{4}$", names(wide_table_df), value = T)
    )
    # ...and give right names to our columns
    colnames(long_table)[2] <- "sample"
    colnames(long_table)[3] <- "shoot_number"
  }
  # ...then add a column with years
  long_table <- long_table %>% dplyr::mutate(
    year =  as.numeric(
      substr(
        sample, nchar(as.character(sample))-4+1,
        nchar(as.character(sample))
      ) # gets the last 4 characters
    )
  )
  return(long_table)
}

#' Takes long_data
#' Returns wide_transponed_data (column names are species, row names are years)
#' @importFrom reshape2 dcast
#' @param long_data The result of get_long_table() function.
#' get_wide_transponed()
get_wide_transponed <- function(long_data){
  return(reshape2::dcast(long_data, year~species))
}

#' Gets rid of zeros in sumed data.
#' Takes transponed data frame, returns the data frame with the only columns
#' which means are more than 0 (without_zeros)
#' @param transponed_df The result of get_wide_transponed().
get_without_zeros <- function(transponed_df){
  without_zeros <- transponed_df[, which(colMeans(transponed_df) > 0)]
  return(without_zeros)
}

#' @title states_selector()
#' @description Function for selection the state of shoots if any.
#' Checks what state(s) should be selected and makes data frame for it (or them together)
#' @import dplyr
#' @param long_table From get_long_table().
#' @param state  Character. "g" - generative, "v" - vegetative, "v+j" - vegetative and juvenile.
#'  Which states should be selected. If NULL, all shoots will be selected.
#'  Dafault to NULL.

states_selector <- function(long_table, state = NULL){
  long_table$species <- as.character(long_table$species) # to avoid factors in our lists
  if(!is.null(state)){
    if(state == "g"){
      long_data <- long_table %>%
        dplyr::filter(state == "g") %>%
        dplyr::group_by(species, year) %>%
        dplyr::summarise(sum_shoot_number = sum(shoot_number)) %>%
        dplyr::ungroup()

      non_zero <- long_table %>%
        dplyr::group_by(species, year) %>%
        dplyr::summarise(aver_shoot_number = mean(shoot_number)) %>%
        dplyr::filter(aver_shoot_number >= 0.1) %>%
        dplyr::ungroup()

      # getting rid of species with too small amount of soots
      long_data <- long_data %>%
        dplyr::filter(species %in% non_zero$species)

      return(long_data)

    } else {

      if(state == "v"){
        long_data <- long_table %>%
          dplyr::filter(state == "v") %>%
          dplyr::group_by(species, year) %>%
          dplyr::summarise(sum_shoot_number = sum(shoot_number)) %>%
          dplyr::ungroup()

        non_zero <- long_table %>%
          dplyr::group_by(species) %>%
          dplyr::summarise(aver_shoot_number = mean(shoot_number)) %>%
          dplyr::filter(aver_shoot_number != 0) %>%
          dplyr::ungroup()

        # getting rid of species with too small amount of soots
        long_data <- long_data %>%
          dplyr::filter(species %in% non_zero$species)

        return(long_data)

      } else {

        if(state == "v+j"){
          long_data <- long_table %>%
            dplyr::filter(state %in% c("v","j")) %>%
            dplyr::group_by(species, year) %>%
            dplyr::summarise(sum_shoot_number = sum(shoot_number)) %>%
            dplyr::ungroup()

          non_zero <- long_table %>%
            dplyr::group_by(species) %>%
            dplyr::summarise(aver_shoot_number = mean(shoot_number)) %>%
            dplyr::filter(aver_shoot_number != 0) %>%
            dplyr::ungroup()

          # getting rid of species with too small amount of soots
          long_data <- long_data %>%
            dplyr::filter(species %in% non_zero$species)

          return(long_data)
        }
      }
    }


  } else {
    long_data <- long_table %>%
      dplyr::group_by(species, year) %>%
      dplyr::summarise(sum_shoot_number = sum(shoot_number)) %>%
      dplyr::ungroup()

    non_zero <- long_table %>%
      dplyr::group_by(species, year) %>%
      dplyr::summarise(aver_shoot_number = mean(shoot_number)) %>%
      dplyr::filter(aver_shoot_number != 0) %>%
      dplyr::ungroup()

    # getting rid of species with too small amount of soots
    long_data <- long_data %>%
      dplyr::filter(species %in% non_zero$species)
    return(long_data)
  }

}

# Takes .csv file with data
# Returns tidy wide table
get_tidy <- function(csv_file, need_abbr = FALSE, state = NULL){
  raw_df <- read.csv(csv_file, h=T)
  raw_df$species  <- sapply(raw_df$species, get_sp_fullnames)
  if(isTRUE(need_abbr)){
    abbr_df <- raw_df
    abbr_df$species <- get_abbr(abbr_df$species)
    long_df <- get_long_table(abbr_df)
  } else {
    long_df <- get_long_table(raw_df)
  }

  long_df_sum <- states_selector(long_df, state)
  wide_t_df <- get_wide_transponed(long_df_sum)
  wide_t_df <- get_without_zeros(wide_t_df)
  return(wide_t_df)
}



# Takes two data files which shoot numbers must be united
high_plus_low <- function(csv_file, csv_file_2, need_abbr = NULL, state = NULL){
  high_df <- get_tidy(csv_file, need_abbr, state)
  low_df <- get_tidy(csv_file_2, need_abbr, state)
  high_intersect_df <- high_df[intersect(rownames(high_df),
                                         rownames(low_df)),
                               intersect(colnames(high_df),
                                         colnames(low_df))]
  low_intersect_df <- low_df[intersect(rownames(high_df),
                                       rownames(low_df)),
                             intersect(colnames(high_df),
                                       colnames(low_df))]
  year <- high_intersect_df[, 1]
  intersect_df_sum <- high_intersect_df[, -1] + low_intersect_df[, -1]
  intersect_df_sum <- cbind(year, intersect_df_sum)
  return(intersect_df_sum)
}

#' Takes one ore two .csv files
#' Returns tidy wide table with selected types of shoots:
#'    columns are species' sums of shoot numbers per a year for all samples.
#'    The first column is named "year", containes years.
#'    Rows contain shoot numbers of species for a year.
#' @param csv_file Name of .csv file containes raw data. No default.
#' @param csv_file_2 Name of second .csv file to be united with the first one.
#' It must be checked, that the data are from the SAME period.
#' Default to NULL
#' @param need_abbr Boolean, if it is needed to abbtriviate species names. Default to FALSE.
#' @param state State of shoots. Can be NULL for all states of shoots,
#' "g" for generative,
#' "v" for vegetative,
#' "v+j" for vegetative and juvenile.
#' Default to NULL.
#' @export
get_tidy_data <- function(csv_file, csv_file_2 = NULL, need_abbr = FALSE, state = NULL){
  if(!is.null(csv_file_2)){
    return(high_plus_low(csv_file, csv_file_2, need_abbr, state))
  } else { return(get_tidy(csv_file, need_abbr, state))}
}
