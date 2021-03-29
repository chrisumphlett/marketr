#' Determine NPS Opportunity for Unique Values of Selected Attribute
#'
#' Calculate how much NPS would increase if each distinct value of an attribute had a perfect
#' NPS score. Optionally choose a grouping column to do it by the values of that column.
#'
#' The calculation across an entire sample of surveys is simple. A customer experience
#' manager may want to calculate CXi across many different dimensions and filtering
#' in different ways; the functions in this package utilize the tidy framework to streamline
#' calculating CXi along as many dimensions as desired.
#'
#' @param survey_data Raw survey data. Needs to be one row per survey with the nps question
#' in a numeric column called nps_question.
#' @param group_var Column to group on for baseline NPS calculation.
#' @param opp_var Column with attributes that you want to test for opportunity.
#'
#' @return Data frame with baseline NPS and how much NPS would increase by if a given
#' attribute had perfect NPS scores.
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import tidyr
#' @importFrom purrr "map_dfr"
#' @import rlang
#'
#' @export
#' 
#' @examples
#' nps_question <- sample(10, 100, replace = TRUE)
#' nps_date <- rep(seq.Date(from = as.Date("2019-01-01"), to = as.Date("2019-01-10"), by = "day"), 10)
#' nps_group <- rep(c("a", "b", "c", "d"), 25)
#' nps_attr <- rep(c("alpha", "beta", "chi", "delta"), 25)
#' df <- data.frame(nps_question, nps_date, nps_group, nps_attr)
#' # see improvements to overall NPS if each attribute had a perfect score
#' nps_oppy(df, group_var = NULL, opp_var = nps_attr)
#' # see improvements to group-level NPS if each attribute had a perfect score
#' nps_oppy(df, group_var = nps_group, opp_var = nps_attr)
#' 

nps_oppy <- function(survey_data, group_var, opp_var) {
  unique_vals <- {{survey_data}} %>% 
    dplyr::select({{opp_var}}) %>% 
    dplyr::distinct() %>% 
    dplyr::pull()

  inner_fun <- function(.x) {
    base_nps <- nps_calc({{survey_data}}, {{group_var}}) %>%
      dplyr::rename(base_nps = .data$nps) %>%
      dplyr::select(-.data$survey_count)
    
    nps_df <- {{survey_data}} %>% 
      dplyr::mutate(nps_orig = .data$nps_question,
             nps_perfect = 10.0,
             nps_question = dplyr::if_else({{opp_var}} == .x, as.numeric(.data$nps_perfect), as.numeric(.data$nps_orig)))

    ## If a group var is provided, do a left join by the group var
    ## else, do a cross join
    join_var <- rlang::enquo(group_var)
    by_vars <- rlang::set_names(quo_name(join_var), quo_name(join_var))
    
    if (is.na(by_vars) == FALSE){
      nps_df2 <- nps_calc(nps_df) %>%
        dplyr::left_join(base_nps, by = character()) %>% # cross-join
        dplyr::mutate(nps_increase = .data$nps - .data$base_nps,
               group_value = factor(.x))
      
    } else {
      nps_df2 <- nps_calc(nps_df, {{group_var}}) %>%
        dplyr::left_join(base_nps, by = by_vars) %>%
        dplyr::mutate(nps_increase = .data$nps - .data$base_nps,
               group_value = factor(.x))
    }
  }

  nps_oppy_df <- purrr::map_dfr(unique_vals, inner_fun) %>%
    dplyr::select(-.data$survey_count) %>%
    dplyr::rename(oppy_nps = .data$nps)
  
  return(nps_oppy_df)
}
