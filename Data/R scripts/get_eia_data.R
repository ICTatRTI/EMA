# get_eia_data.R


# Function to divide a vector into increments of x -- returns a list
vector_slice <- function(vec, increment) {
  new_vec <- split(vec, ceiling(seq_along(vec)/increment))
  return(new_vec)
}



#' Download EIA API data based on a crosswalk table
#'
#' @param eia_api_crosswalk Must have EIA_key column
#'
# TODO: what if eia_api_crosswalk has columns which are named same as return, e.g., 'name'
get_eia_api_data <- function(eia_api_crosswalk) {
  eia_api_keys <- unique(eia_api_crosswalk$EIA_key)
  eia_api_keys_slices <- vector_slice(eia_api_keys, 100)

  data_raw <- map_dfr(eia_api_keys_slices,
                      ~eia::eia_series(id = .x, tidy = TRUE))

  notes_vars <- str_subset(names(eia_api_crosswalk), "^notes")

  final_vars <- names(eia_api_crosswalk) %>%
    setdiff(c("EIA_key", notes_vars)) %>%
    c(c("year", "unit", "value", "datasrc"))

  arrange_vars <- setdiff(final_vars, c("EIA_key", "unit", "value", "datasrc"))

  proc_data <- eia_api_crosswalk %>%
    select(-any_of(notes_vars)) %>%
    left_join(data_raw, by = c("EIA_key" = "series_id")) %>%
    tidyr::unnest(data) %>%
    rename(series_id = EIA_key,
           unit = units) %>%

    mutate(
      unit = standardize_eia_units(unit),
      datasrc = series_id
    ) %>%
    dplyr::arrange(across(any_of(arrange_vars))) %>%
    select(all_of(final_vars))

  proc_data
}

#' Download EIA API data and save to file (after adding metadata)
#'
#' @param eia_api_crosswalk
#' @param file path where csv is written
#'
#' @import fs
#'
get_write_eia_api_data <- function(eia_api_crosswalk, file){

  proc_data <- get_eia_api_data(eia_api_crosswalk)

  write_csv(proc_data, file)

  fs::path_rel(file)
}

standardize_eia_units <- function(x) {
  case_when(
    x == "MMmt CO2" ~ "MMTCO2e",
    x == "million metric tons" ~ "MMTCO2e", #STEO
    x == "billion 2012 $" ~ "billion dollars",
    x == "billion chained 2012 dollars (seasonally-adjusted annual rate)" ~ "billion dollars", #STEO
    x == "MMst" ~ "million short_tons",
    x == "MMb/d" ~ "million barrels/day",
    TRUE ~ x
  )
}
