#' Get album information
#'
#' @param album_url Full URL to album
#'
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest "%>%" html_node html_nodes html_text html_attr
get_album_info <- function(album_url) {
  html <- read_html(album_url)

  artist <- html %>%
    html_node(xpath = "//*[@itemprop='byArtist']/a") %>%
    html_text()

  location <- html %>%
    html_node("#band-name-location .location") %>%
    html_text()

  title <- html %>%
    html_nodes(xpath = "//h2[@itemprop='name']") %>%
    html_text() %>%
    trimws()

  tags <- html %>%
    html_nodes(xpath = "//*[@itemprop='keywords']") %>%
    html_text()

  pub <- html %>%
    html_node(xpath = "//*[@itemprop='datePublished']") %>%
    html_attr("content")
  published <- paste(substr(pub, 1, 4), substr(pub, 5, 6),
                     substr(pub, 7, 8), sep = "-")

  description <- html %>%
    html_node(xpath = "//*[@itemprop='description']") %>%
    html_text()
  description <- gsub("\\r", "", description) %>% trimws()

  return(list(
    artist = artist,
    location = location,
    title = title,
    tags = tags,
    url = album_url,
    published = published,
    description = description
  ))
}
