#' Tidy Calculation of Net Promoter Score
#'
#' Simplifies the calculation of Net Promoter Score (NPS) from raw survey data within
#' the tidyverse framework.
#'
#' Net Promoter Score was originally developed by Fred Reichheld and now is owned by
#' Bain Company and Satmetrix Systems. According to Wikipedia it "is a management
#' tool that can be used to gauge the loyalty of a firm's customer relationships."
#'
#' @param survey_data Raw survey data. Needs to be one row per survey with the nps question
#' in a numeric column called nps_question
#' responses having column names of needs, ease and emotion
#' @param ... optional columns by which to group the nps calculation. There is no limit to
#' the number of grouping variables chosen. Too many will likely result in nps calculations
#' that are too fragmented / based on very small survey counts.
#'
#' @return Data frame with nps and survey count for each combination of the grouping variables
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import tidyr
#'
#' @export

nps_calc <- function(survey_data, ...) {

  nps <- {{survey_data}} %>%
    dplyr::mutate(response_class = dplyr::case_when(
      nps_question >= 9 ~ "PROMOTER",
      nps_question <= 6 ~ "DETRACTOR",
      TRUE ~ "PASSIVE")) %>%
    dplyr::group_by(..., .data$response_class) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(.data$response_class, count) %>%
    dplyr::mutate(PROMOTER = if_else(is.na(.data$PROMOTER), 0, as.numeric(.data$PROMOTER)),
                  PASSIVE = if_else(is.na(.data$PASSIVE), 0, as.numeric(.data$PASSIVE)),
                  DETRACTOR = if_else(is.na(.data$DETRACTOR), 0, as.numeric(.data$DETRACTOR)),
                  nps = (.data$PROMOTER - .data$DETRACTOR) /
                    (.data$PROMOTER + .data$PASSIVE + .data$DETRACTOR) * 100)

  nps2 <- nps %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(nps = mean(.data$nps),
                     survey_count = sum(.data$PROMOTER + .data$DETRACTOR + .data$PASSIVE)) %>%
    dplyr::ungroup()

  return(nps2)
}
