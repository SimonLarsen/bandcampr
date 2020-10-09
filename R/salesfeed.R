#' Get currently selling items.
#'
#' @export
#' @importFrom httr GET content
get_sales_feed <- function() {
  start_date <- as.numeric(Sys.time())
  res <- GET(
    "https://bandcamp.com/api/salesfeed/1/get",
    query = list(start_date = start_date)
  )
  data <- content(res, "parsed")
  return(data$events)
}
