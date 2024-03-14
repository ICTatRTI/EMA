
#' Interpolate EIA key templates
#'
#' @param x character vector of EIA key templates
#' @param EIA_pub character vector for EIA publication
#' @param EIA_scenario character vector for EIA scenario
#' @param ... should be empty
#'
#' @returns character vector with EIA_key interpolated
#'
#' @examples
#' template_df <- tibble(
#' EIA_template = c("{EIA_pub}.{EIA_scenario}.RESTOFIT5678",
#'                  "{EIA_pub}.{EIA_scenario}.RESTOFIT1234"),
#'     EIA_pub = c("AEO.2020", "AEO.2020"),
#'     EIA_scenario = c("REF2020", "REF2020"))
#' mutate(template_df, EIA_key = interp_eia_key_chr(EIA_template,
#'   EIA_pub = "AEO.2020", EIA_scenario = "REF2020"))
#' interp_eia_key_chr(template_df$EIA_template, EIA_pub = "AEO.2020",
#'   EIA_scenario = "REF2020")
#' interp_eia_key_chr(template_df$EIA_template, EIA_pub = "AEO.2021", EIA_scenario = "REF2021")
#' @export
interp_eia_key_chr <- function(x,
                       EIA_pub,
                       EIA_scenario,
                       ...) {

  x %>%
    stringr::str_replace("\\{EIA_pub\\}", EIA_pub) %>%
    stringr::str_replace("\\{EIA_scenario\\}", EIA_scenario)
}

#' Interpolate EIA templates in a data frame
#'
#' @examples
#' template_df <- tibble(
#' EIA_template = c("{EIA_pub}.{EIA_scenario}.RESTOFIT5678",
#'                  "{EIA_pub}.{EIA_scenario}.RESTOFIT1234"),
#'     EIA_pub = c("AEO.2020", "AEO.2020"),
#'     EIA_scenario = c("REF2020", "REF2020"))
#' interp_eia_key_data(template_df, EIA_pub = "AEO.2021", EIA_scenario = "REF2021")
#' @export
interp_eia_key_data <- function(data,
                                ...,
                                template_col = "EIA_template",
                                out_col = "EIA_key") {

  strip_vars <- setdiff(str_subset(names(data), "^EIA_"), out_col)
  dots <- enquos(...)

  data %>%
    mutate(!!sym(out_col) :=
             interp_eia_key_chr(!!sym(template_col), !!!dots )) %>%
    select(-any_of(strip_vars))

}
