#' Get discovery items.
#'
#' @param page Page to get. 0 is first page.
#' @param feed Feed category. One of \code{"top"} (best-selling), \code{"new"}
#'   (new arrivals) or \code{"rec"} (artist-recommended).
#' @param week Week to get feed for. Must be one of \code{"today"},
#'   \code{"this.week"} or a numeric week number.
#' @param genre Filter on genre e.g. \code{"jazz"}.
#'   See Bandcamp front page for options.
#' @param subgenre Filter on subgenre e.g. \code{"bebop"}.
#'   See Bandcamp front page for options.
#' @param format Filter on release format.
#'   Must be one of \code{"digital"}, \code{"vinyl"}, \code{"cd"} or \code{"cassette"}.
#' @param location Filter on location e.g. \code{"paris"}.
#'   See Bandcamp front page for options.
#' @export
#' @importFrom httr GET content
get_discovery_feed <- function(
  page=0,
  feed="top",
  week="this.week",
  genre=NULL,
  subgenre=NULL,
  format=NULL,
  location=NULL
) {
  if (typeof(week) == "character") {
    week <- match.arg(week, c("today", "this.week"))
    week <- switch(
      week,
      today = -1,
      this.week = 0
    )
  }

  if (typeof(location) == "character") {
    location <- tolower(location)
    location <- match.arg(location, names(.location_options))
    location <- .location_options[[location]]
  }

  req <- GET(
    "https://bandcamp.com/api/discover/3/get_web",
    query = list(
      p = page,
      s = feed,
      g = genre,
      t = subgenre,
      f = format,
      gn = location,
      w = week
    )
  )
  data <- content(req, "parsed")
  return(data$items)
}

.location_options <- c(
  anywhere = 0, amsterdam = 2759794, atlanta = 4180439, austin = 4671654,
  baltimore = 4347778, berlin = 2950159, boston = 4930956, brooklyn = 5110302,
  `buenos aires` = 3435907, chicago = 4887398, denver = 5419384,
  detroit = 4990729, dublin = 2964574, glasgow = 3333231, london = 2643743,
  `los angeles` = 5368361, madrid = 3117735, manchester = 2643123,
  melbourne = 2158177, `mexico city` = 3530597, miami = 4164138,
  minneapolis = 5037649, montreal = 6077243, nashville = 4644585,
  `new orleans` = 4335045, `new york city` = 5128581, oakland = 5378538,
  paris = 2988507, philadelphia = 4560349, portland = 5746545,
  `san francisco` = 5391959, seattle = 5809844, sydney = 2147714,
  toronto = 6167865, vancouver = 6173331, `washington, dc` = 4140963
)
