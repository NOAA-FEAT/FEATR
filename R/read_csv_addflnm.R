#' read_csv_addflnm: Function to read in .csv files and append filename into new 
#' column
#'
#' @description read_csv_addflnm is a function to read in .csv files (e.g. Echoview exports)
#' as a table and append the filename as another column in the table.  Is based
#' off read_csv (from package readr)
#'
#' @param flnm - filename (full path) of csv file to read in
#'
#' @examples
# CONV_10m_combine <- NASC.list.to.combine %>% map_df(~read_csv_addflnm(.))
#'
#' @export

read_csv_addflnm <- function(flnm) {
     temp<-suppressMessages(readr::read_csv(flnm)) %>%
          mutate(filename = flnm)
}
