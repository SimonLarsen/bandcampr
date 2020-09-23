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

#' Get data for all items in fan collection
#'
#' @param username Username of the fan account
#'
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest "%>%" html_node html_attr
#' @importFrom jsonlite fromJSON
#' @importFrom httr POST content
#' @importFrom utils txtProgressBar setTxtProgressBar
get_fan_collection <- function(username) {
  html <- read_html(paste0("http://bandcamp.com/", username))

  pagedata <- html %>%
    html_node("#pagedata") %>%
    html_attr("data-blob") %>%
    fromJSON()

  collection <- unname(pagedata$item_cache$collection)

  fan_id <- pagedata$fan_data$fan_id
  last_token <- pagedata$collection_data$last_token
  batch_size <- pagedata$collection_data$batch_size
  hidden_items <- pagedata$collection_data$item_count - length(collection)

  batches_done <- 1
  pb <- txtProgressBar(
    max = ceiling(hidden_items / batch_size),
    initial = batches_done,
    style = 3
  )

  more_available <- hidden_items > 0
  while (more_available) {
    req <- POST(
      "http://bandcamp.com/api/fancollection/1/collection_items",
      body = list(
        fan_id = fan_id,
        older_than_token = last_token,
        count = batch_size
      ),
      encode = "json"
    )

    data <- content(req, "parsed")
    collection <- c(collection, data$items)
    more_available <- data$more_available
    last_token <- data$last_token

    batches_done <- batches_done + 1
    setTxtProgressBar(pb, batches_done)
  }

  return(collection)
}

#' Get data for all items in fan wishlist
#'
#' @param username Username of the fan account
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest "%>%" html_node html_attr
#' @importFrom jsonlite fromJSON
#' @importFrom httr POST content
#' @importFrom utils txtProgressBar setTxtProgressBar
get_fan_wishlist <- function(username) {
  html <- read_html(paste0("http://bandcamp.com/", username, "/wishlist"))

  pagedata <- html %>%
    html_node("#pagedata") %>%
    html_attr("data-blob") %>%
    fromJSON()

  wishlist <- unname(pagedata$item_cache$wishlist)

  fan_id <- pagedata$fan_data$fan_id
  last_token <- pagedata$wishlist_data$last_token
  batch_size <- pagedata$wishlist_data$batch_size
  hidden_items <- pagedata$wishlist_data$item_count - length(wishlist)

  batches_done <- 1
  pb <- txtProgressBar(
    max = ceiling(hidden_items / batch_size),
    initial = batches_done,
    style = 3
  )

  more_available <- hidden_items > 0
  while (more_available) {
    req <- POST(
      "http://bandcamp.com/api/fancollection/1/wishlist_items",
      body = list(
        fan_id = fan_id,
        older_than_token = last_token,
        count = batch_size
      ),
      encode = "json"
    )

    data <- content(req, "parsed")
    wishlist <- c(wishlist, data$items)
    more_available <- data$more_available
    last_token <- data$last_token

    batches_done <- batches_done + 1
    setTxtProgressBar(pb, batches_done)
  }

  return(wishlist)
}

#' Get data for all fan followers
#'
#' @param username Username of the fan account
#' @export
#' @importFrom xml2 read_html
#' @importFrom rvest "%>%" html_node html_attr
#' @importFrom jsonlite fromJSON
#' @importFrom httr POST content
#' @importFrom utils txtProgressBar setTxtProgressBar
get_fan_followers <- function(username) {
  html <- read_html(paste0("http://bandcamp.com/", username, "/wishlist"))

  pagedata <- html %>%
    html_node("#pagedata") %>%
    html_attr("data-blob") %>%
    fromJSON()

  followers <- unname(pagedata$item_cache$followers)

  fan_id <- pagedata$fan_data$fan_id
  last_token <- pagedata$followers_data$last_token
  batch_size <- pagedata$followers_data$batch_size
  hidden_items <- pagedata$followers_data$item_count - length(followers)

  batches_done <- 1
  pb <- txtProgressBar(
    max = ceiling(hidden_items / batch_size),
    initial = batches_done,
    style = 3
  )

  more_available <- hidden_items > 0
  while (more_available) {
    req <- POST(
      "http://bandcamp.com/api/fancollection/1/followers",
      body = list(
        fan_id = fan_id,
        older_than_token = last_token,
        count = batch_size
      ),
      encode = "json"
    )

    data <- content(req, "parsed")
    followers <- c(followers, data$followeers)
    more_available <- data$more_available
    last_token <- data$last_token

    batches_done <- batches_done + 1
    setTxtProgressBar(pb, batches_done)
  }

  return(followers)
}
