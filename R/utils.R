#' Transpose survey data to prep for CXi calculation
#'
#' Transposes survey data from wide to long to prepare for tidy calculation of CXi
#'
#' This function is called by other functions in the package that do the final CXi
#' calculations.
#'
#' @param survey_data Raw survey data. Needs to be one row per survey with the nps question
#' in a numeric column called nps_question
#' @param ... optional colunns by which to group the CXi calculation. There is no limit to
#' the number of grouping variables chosen. Too many will likely result in CXi calculations
#' that are too fragmented / based on very small survey counts.
#' @param cx_high Threshold in scale where response at or above is a "high"
#' @param cx_low Threshold in scale where response at or below is a "low"
#'
#' @return Data frame with one row per survey response for each of the three CXi questions
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import tidyr
#' @noRd


survey_transpose_cxi <- function(survey_data, ..., cx_high, cx_low) {

  survey_transpose <- {{survey_data}} %>%
    tidyr::pivot_longer(cols = c(.data$needs, .data$ease, .data$emotion),
                        names_to = "question", values_to = "response", values_ptypes = list(val = 'character')) %>%
    dplyr::mutate(response_class = dplyr::case_when(
      response >= cx_high ~ "HIGH",
      response <= cx_low ~ "LOW",
      TRUE ~ "MID")) %>%
    dplyr::select(-.data$response)

  return(survey_transpose)
}
