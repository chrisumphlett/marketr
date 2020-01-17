#' Tidy Calculation of Customer Experience Index
#'
#' Simplifies the calculation of Customer Experience Index (CXi) from raw survey data within
#' the tidyverse framework.
#'
#' Customer Experience Index is a metric created by Forrester to help companies systematically
#' measure customer experience in a way that their research has found is connected to
#' improving customer loyalty. More information can be found at https://go.forrester.com/analytics/cx-index/
#'
#' The calculation across an entire sample of surveys is simple. A customer experience
#' manager may want to calculate CXi across many different dimensions and filtering
#' in different ways; the functions in this package utilize the tidy framework to streamline
#' calculating CXi along as many dimensions as desired.
#'
#' @param survey_data Raw survey data. Needs to be one row per survey with the three CXi question
#' responses having column names of needs, ease and emotion
#' @param ... optional columns by which to group the CXi calculation. There is no limit to
#' the number of grouping variables chosen. Too many will likely result in CXi calculations
#' that are too fragmented / based on very small survey counts.
#' @param cx_high Threshold in scale where response at or above is a "high"
#' @param cx_low Threshold in scale where response at or below is a "low"
#'
#' @return Data frame with CXi and survey count for each combination of the grouping variables
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import tidyr
#' @importFrom tibble "add_column"
#'
#' @export

cxi_calc <- function(survey_data, ..., cx_high = 4, cx_low = 2) {
  cx_high2 <- cx_high
  cx_low2 <- cx_low

  . <- NA # prevent variable binding note for the dot in the add_column function
          # cannot use .data here or else get an error for trying to create dup column

  survey_transpose <- survey_transpose_cxi(survey_data, cx_high = cx_high2, cx_low = cx_low2, ...)

  cols <- c(HIGH = NA_real_, MID = NA_real_, LOW = NA_real_)

  cxi <- survey_transpose %>%
    dplyr::group_by(..., .data$question, .data$response_class) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(.data$response_class, count) %>%
    tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>%
    dplyr::mutate(HIGH = if_else(is.na(.data$HIGH), 0, as.numeric(.data$HIGH)),
                  MID = if_else(is.na(.data$MID), 0, as.numeric(.data$MID)),
                  LOW = if_else(is.na(.data$LOW), 0, as.numeric(.data$LOW)),
                  question_score = (.data$HIGH - .data$LOW) /
                    (.data$HIGH + .data$MID + .data$LOW) * 100)

  cxi2 <- cxi %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(cxi = mean(.data$question_score),
                     survey_count = sum(.data$HIGH + .data$LOW + .data$MID) / 3) %>%
    dplyr::ungroup()

  return(cxi2)
}
