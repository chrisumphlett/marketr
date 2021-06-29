#' Tidy Calculation of Net Promoter Score trends by group
#'
#' Simplifies the calculation of Net Promoter Score (NPS) trends over time from
#' raw survey data within the tidyverse framework.
#'
#' Net Promoter Score was originally developed by Fred Reichheld and now is
#' owned by Bain Company and Satmetrix Systems. According to Wikipedia it
#' "is a management tool that can be used to gauge the loyalty of a firm's
#' customer relationships."
#'
#' The trend version of the function allows you to specify one column as a
#' date over which to trend the data. This allows quick filtering to eliminate
#' groupings that fail to meet user-specified thresholds for average or
#' minimum survey counts per time period.
#'
#' The resulting data set is set up for creating faceted line plots using
#' ggplot2.
#'
#' @param survey_data Raw survey data. Needs to be one row per survey with the
#' NPS question in a numeric column called nps_question
#' @param trend_var Column that represents an element of time, eg week number,
#' date, month & year
#' @param ... Optional columns by which to group the NPS calculation. There is
#' no limit to the number of grouping variables chosen. Too many will likely
#' result in NPS calculations that are too fragmented / based on very small
#' survey counts.
#' @param min_surveys Minimum surveys found in every period for each group to
#' be included
#' @param avg_surveys Average surveys found in every period for each group to
#' be included

#'
#' @return Data frame with NPS and survey count for each combination of the
#' grouping variables over
#' the time variable.
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import tidyr
#'
#' @export
#'
#' @examples
#' nps_question <- sample(10, 100, replace = TRUE)
#' nps_date <- rep(seq.Date(from = as.Date("2019-01-01"),
#' to = as.Date("2019-01-10"), by = "day"), 10)
#' nps_group <- rep(c("a", "b", "c", "d"), 25)
#' df <- data.frame(nps_question, nps_date, nps_group)
#' nps_trend(df, nps_date, nps_group)


nps_trend <- function(survey_data, trend_var, ..., min_surveys = 1,
                      avg_surveys = 0) {

  cols <- c(PROMOTER = NA_real_, PASSIVE = NA_real_, DETRACTOR = NA_real_)

  nps <- {{survey_data}} %>%
    dplyr::mutate(response_class = dplyr::case_when(
      nps_question >= 9 ~ "PROMOTER",
      nps_question <= 6 ~ "DETRACTOR",
      TRUE ~ "PASSIVE")) %>%
    dplyr::group_by(..., {{trend_var}}, .data$response_class) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(.data$response_class, count) %>%
    dplyr::mutate(PROMOTER = if_else(is.na(.data$PROMOTER), 0,
                                     as.numeric(.data$PROMOTER)),
                  PASSIVE = if_else(is.na(.data$PASSIVE), 0,
                                    as.numeric(.data$PASSIVE)),
                  DETRACTOR = if_else(is.na(.data$DETRACTOR), 0,
                                      as.numeric(.data$DETRACTOR)),
                  nps = (.data$PROMOTER - .data$DETRACTOR) /
                    (.data$PROMOTER + .data$PASSIVE + .data$DETRACTOR) * 100)

  nps2 <- nps %>%
    dplyr::group_by(..., {{trend_var}}) %>%
    dplyr::summarise(nps = mean(.data$nps),
                     survey_count = sum(.data$PROMOTER + .data$DETRACTOR +
                                          .data$PASSIVE)) %>%
    dplyr::ungroup()

  test <- tryCatch({
    nps2 %>%
      dplyr::group_by(...) %>%
      dplyr::summarise(avg_survey_ct = mean(.data$survey_count),
                       min_survey_ct = min(.data$survey_count)) %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(nps2) %>%
      dplyr::filter(.data$avg_survey_ct >= avg_surveys,
                    .data$min_survey_ct >= min_surveys) %>%
      dplyr::select(..., {{trend_var}}, nps, .data$survey_count,
                    .data$avg_survey_ct, .data$min_survey_ct)
  }, error = function(e) "ERROR_")

  # no grouping var
  if (class(test)[1] == "character") {
    nps3 <- nps2 %>%
      dplyr::summarise(avg_survey_ct = mean(.data$survey_count),
                       min_survey_ct = min(.data$survey_count)) %>%
      cbind(nps2)
  }

  if (class(test)[1] != "character") {
    nps3 <- nps2 %>%
      dplyr::group_by(...) %>%
      dplyr::summarise(avg_survey_ct = mean(.data$survey_count),
                       min_survey_ct = min(.data$survey_count)) %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(nps2) %>%
      dplyr::filter(.data$avg_survey_ct >= avg_surveys,
                    .data$min_survey_ct >= min_surveys) %>%
      dplyr::select(..., {{trend_var}}, nps, .data$survey_count,
                    .data$avg_survey_ct, .data$min_survey_ct)
  }
  return(nps3)

}
