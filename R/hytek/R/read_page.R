read_page <- function(url, relay = FALSE) {
    # Read url, split into lines
    lines <- strsplit(
      html_text(
        read_html(
          url
          )
        )
      , '\r\n'
      )[[1]]
    
    # Find lines whcih start times
    startLines <- which(str_detect(lines, '^[0-9 ]{2}[0-9] |^ -- '))
    
    # Find lines with times
    timeLines <- 
      which(
        str_detect(lines, '^[()0-9DQr:+. ]+$') &
        !str_detect(lines, '^ *$')
      )
    
    # Find the last line of each time
    breakLines <- seq(1, length(lines))
    breakLines <- breakLines[!breakLines %in% timeLines]
    
    find_break <- function(start, breaks = breakLines) {
      first(breaks[breaks > start]) - 1
    }
    
    endLines <- map_dbl(startLines + if_else(relay, 2, 0), find_break)
    
    # Create a tibble with starting and ending lines
    startEndLines <- tibble(start = startLines, end = endLines)
    
    # Create list of swim times 
    swimList <- 
      map2(
        startEndLines$start
        , startEndLines$end
        , function(.start, .end, .lines = lines) {.lines[.start:.end]}
        )
    # Send to function to parse each swim for data
    out <- map_dfr(swimList, read_swim)
}