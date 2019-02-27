scrape_data <- function(url) {
    # Build index of events
    eventIndex <- build_index(url)
    
    # Subset the events to pull data for (ideally just filter for swimming)
    toPull <- filter(eventIndex, sport == "Swimming", !relay, distance != "50")
    
    # Create vector of urls to pull
    urls <- paste0(url, toPull$url)
    
    # Create vector of events to tag results
    eventNames <- 
      paste(
        toPull$distance
        , toPull$event
        , if_else(toPull$finals, "Finals", "Prelims")
        )
    
    # Pull data
    output <- 
      map2_dfr(
        urls
        , eventNames
        , function(url, event) {mutate(read_page(url), event = event)}
      )
}