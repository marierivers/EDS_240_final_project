
#' Title
#'
#' @param snow_files 
#'
#' @return
#' @export
#'
#' @examples

read_snow_files_script <- function(files) {
  path <- here("data", files)
  snow_data <- read_delim(path, skip = 14, delim = ",", na = c("-99", "NaN", "")) %>%
    janitor::clean_names() %>% 
    rename(year = number_year) %>% 
    rename(snow_std_err_m = std_err_m) %>% 
    mutate(fractional_year = str_replace(fractional_year, 
                                         pattern = ",", 
                                         replacement = "")) %>% 
    mutate(file_name = files) %>% 
    mutate(site_id = str_replace(file_name,
                              pattern = "_snow_v1.csv",
                              replacement = "")) %>% 
    mutate(date = paste0(year, month, day)) %>% 
    mutate(date = lubridate::ymd(date)) %>% 
    mutate(month = lubridate::month(date)) %>% 
    mutate(day = lubridate::day(date)) %>% 
    mutate(doy = lubridate::yday(date)) %>% 
    mutate(snow_depth_m = as.numeric(snow_depth_m)) %>% 
    mutate(snow_std_err_m = as.numeric(snow_std_err_m)) %>% 
    mutate(swe_m = as.numeric(swe_m)) %>% 
    mutate(swe_std_error_m = as.numeric(swe_std_error_m)) %>%  # this column name is not consistent for all files
    mutate(fractional_year = as.numeric(fractional_year)) %>% 
    mutate(water_year = case_when(date <= ymd('2006-09-30') ~ 2006,
                                  date <=ymd('2007-09-30') ~ 2007,
                                  date <= ymd('2008-09-30') ~ 2008,
                                  date <= ymd('2009-09-30') ~ 2009,
                                  date <= ymd('2010-09-30') ~ 2010,
                                  date <= ymd('2011-09-30') ~ 2011, 
                                  date <= ymd('2012-09-30') ~ 2012,
                                  date <= ymd('2013-09-30') ~ 2013,
                                  date <= ymd('2014-09-30') ~ 2014,
                                  date <= ymd('2015-09-30') ~ 2015,
                                  date <= ymd('2016-09-30') ~ 2016,
                                  date <= ymd('2017-09-30') ~ 2017,
                                  date <= ymd('2018-09-30') ~ 2018,
                                  date <= ymd('2019-09-30') ~ 2019,
                                  date <= ymd('2020-09-30') ~ 2020,
                                  date <= ymd('2021-09-30') ~ 2021,
                                  date <= ymd('2022-09-30') ~ 2022,)) %>% 
    mutate(year_month = format(as.Date(date), "%Y-%m"))
}
