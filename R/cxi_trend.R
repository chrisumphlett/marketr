#' Tidy Calculation of Customer Experience Index trends by group
#'
#' Simplifies the calculation of Customer Experience Index (CXi) trends over time from raw survey
#' data within the tidyverse framework.
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
#' The trend version of the function allows you to specify one column as a date over which to
#' trend the data. This allows quick filtering to eliminate groupings that fail to meet
#' user-specified thresholds for average or minimum survey counts per time period.
#'
#' The resulting data set is set up for creating faceted line plots using ggplot2.
#'
#' @param survey_data Raw survey data. Needs to be one row per survey with the three CXi question
#' responses having column names of needs, ease and emotion
#' @param trend_var Column that represents an element of time, eg week number, date, month & year
#' @param ... optional columns by which to group the CXi calculation. There is no limit to
#' the number of grouping variables chosen. Too many will likely result in CXi calculations
#' that are too fragmented / based on very small survey counts.
#' @param cx_high Threshold in scale where response at or above is a "high"
#' @param cx_low Threshold in scale where response at or below is a "low"
#' @param min_surveys Minimum surveys found in every period for each group to be included
#' @param avg_surveys Average surveys found in every period for each group to be included
#'
#' @return Data frame with CXi and survey count for each combination of the grouping variables over
#' the time variable.
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import tidyr
#' @importFrom tibble "add_column"
#'
#' @export
#' 
#' @examples 
#' needs <- sample(5, 100, replace = TRUE)
#' ease <- sample(5, 100, replace = TRUE)
#' emotion <- sample(5, 100, replace = TRUE)
#' cx_date <- rep(seq.Date(from = as.Date("2019-01-01"), to = as.Date("2019-01-10"), by = "day"), 10)
#' cx_group <- rep(c("a", "b", "c", "d"), 25)
#' df <- data.frame(needs, ease, emotion, cx_date, cx_group)
#' cxi_trend(df, cx_date, cx_group)

cxi_trend <- function(survey_data, trend_var, ..., cx_high = 4, cx_low = 2, min_surveys = 1, avg_surveys = 0) {
  cx_high2 <- cx_high
  cx_low2 <- cx_low

  . <- NA # prevent variable binding note for the dot in the add_column function
          # cannot use .data here or else get an error for trying to create dup column

  survey_transpose <- survey_transpose_cxi(survey_data, cx_high = cx_high2, cx_low = cx_low2, ...)

  cols <- c(HIGH = NA_real_, MID = NA_real_, LOW = NA_real_)

  cxi <- survey_transpose %>%
    dplyr::group_by(..., {{trend_var}}, .data$question, .data$response_class) %>%
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
    dplyr::group_by(..., {{trend_var}}) %>%
    dplyr::summarise(cxi = mean(.data$question_score),
                     survey_count = sum(.data$HIGH + .data$LOW + .data$MID) / 3) %>%
    dplyr::ungroup()

  test <- tryCatch({
    cxi2 %>%
    dplyr::group_by(...) %>%
      dplyr::summarise(avg_survey_ct = mean(.data$survey_count),
                       min_survey_ct = min(.data$survey_count)) %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(cxi2) %>%
      dplyr::filter(.data$avg_survey_ct >= avg_surveys, .data$min_survey_ct >= min_surveys) %>%
      dplyr::select(..., {{trend_var}}, cxi, .data$survey_count, .data$avg_survey_ct, .data$min_survey_ct)
  }, error = function(e) "ERROR_")

  # no grouping var
  if(class(test)[1] == "character") {
    cxi3 <- cxi2 %>%
      dplyr::summarise(avg_survey_ct = mean(.data$survey_count),
                       min_survey_ct = min(.data$survey_count)) %>%
      cbind(cxi2)
  }

  if(class(test)[1] != "character"){
    cxi3 <- cxi2 %>%
      dplyr::group_by(...) %>%
      dplyr::summarise(avg_survey_ct = mean(.data$survey_count),
                       min_survey_ct = min(.data$survey_count)) %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(cxi2) %>%
      dplyr::filter(.data$avg_survey_ct >= avg_surveys, .data$min_survey_ct >= min_surveys) %>%
      dplyr::select(..., {{trend_var}}, cxi, .data$survey_count, .data$avg_survey_ct, .data$min_survey_ct)
  }
  return(cxi3)

}
