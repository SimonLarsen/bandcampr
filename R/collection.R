#' Get data from fan collection
#'
#' @param username Username of the fan account
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest "%>%" html_node html_nodes html_attr
#' @importFrom jsonlite fromJSON
#' @importFrom httr POST content
get_fan_collection <- function(username) {
  html <- read_html(paste0("http://bandcamp.com/", username))

  pagedata <- html %>%
    html_node("#pagedata") %>%
    html_attr("data-blob") %>%
    fromJSON()


  collection <- unname(pagedata$item_cache$collection)

  fan_id <- pagedata$fan_data$fan_id
  last_token <- pagedata$collection_data$last_token
  batch_size <- pagedata$hidden_data$batch_size

  more_available <- pagedata$collection_count > batch_size
  while(more_available) {
    req <- POST(
      "https://bandcamp.com/api/fancollection/1/collection_items",
      body=list(fan_id=fan_id, older_than_token=last_token, count=batch_size),
      encode="json"
    )

    data <- content(req, "parsed")
    collection <- c(collection, data$items)
    more_available <- data$more_available
    last_token <- data$last_token
  }

  return(collection)
}
