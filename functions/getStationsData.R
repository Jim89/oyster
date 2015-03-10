################# Stations Data Scrape from Wikipedia ##########################
# railway stations data from wikipedia -----------------------------------------

getRailStations <-
  function(url = "http://en.wikipedia.org/wiki/List_of_London_railway_stations") {
    # function that loads a URL and extracts the first table from the page
    # by default it will operate on the Wikipedia list of london railway
    # stations page. The first table contains the location data for each station
  url %>%
  html() %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table %>%
  tbl_df()
}

# tube stations from wikipedia -------------------------------------------------
# starting with Baker street, get the links to the other stations
extractLinks <-
function(url = "http://en.wikipedia.org/wiki/Baker_Street_tube_station") {
  # takes a URL and extracts all links from it
  url %>%
    html() %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("wiki", ., value = T) %>%
    grep("station", ., value = T) %>%
    paste0("http://en.wikipedia.org",.)
}

# function to extract coordinates from a station page
extractGeo <- function(url){
  # takes a URL and uses the CSS selector provided by SelectorGadget widget to
  # extract the dirty coordinates data from it
  url %>%
    html() %>%
    html_nodes("span span span span span span.geo") %>%
    html_text()
}

# function to extract station name
extractTitle <- function(url){
  # takes a URL and uses the CSS selector provided by SelectorGadget widget to
  # extract the first header, which in this case is the wikipedia page title
  url %>%
    html() %>%
    html_nodes("div h1") %>%
    html_text()
}




