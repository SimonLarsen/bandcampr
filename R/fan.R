#' Get data for a fan account
#'
#' @param username Username of the fan account
#'
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest "%>%" html_node html_attr
#' @importFrom jsonlite fromJSON
get_fan_info <- function(username) {
  html <- read_html(paste0("http://bandcamp.com/", username))

  pagedata <- html %>%
    html_node("#pagedata") %>%
    html_attr("data-blob") %>%
    fromJSON()

  return(pagedata$fan_data)
}
