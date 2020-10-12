#' Get album information
#'
#' @param album_url Full URL to album
#'
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest "%>%" html_node html_text
#' @importFrom jsonlite fromJSON
get_album_info <- function(album_url) {
  html <- read_html(album_url)

  data <- html %>%
    html_node(xpath="//script[@type='application/ld+json']") %>%
    html_text() %>%
    fromJSON

  return(data)
}
