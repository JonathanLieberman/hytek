read_swim <- function(swimLines) {
    # Extract metadata
    textData <- swimLines[1]
    swimmerName <- trimws(substr(textData, 4, 27))
    year <- trimws(substr(textData, 28, 29))
    school <- trimws(substr(textData, 31, 43))
    
    # Extract reaction time
    reactionTime <- 
      parse_time_hytek(
        str_extract(
          swimLines[2]
          , '(?<=r:\\+)[0-9]\\.[0-9]{2}'
          )
        )
    
    # Extract splits
    splits <- 
      unlist(
        str_extract_all(
          swimLines[2:length(swimLines)]
          , '[0-9]*:?[0-9]{2}\\.[0-9]{2}'
          )
        )
    
    # Remove cumulative times
    splits <- splits[seq(1, length(splits), 2)]
    
    # Parse the splits
    splits <- map(splits, parse_time_hytek)
    
    output <-
      bind_rows(
        tibble(
          splitNum = 0
          , time = reactionTime
          )
        , tibble(
          splitNum = seq(1, length(splits))
          , time = do.call("c", splits)
          )
      )
    
    output <- 
      mutate(
        output
        , name = swimmerName
        , year = year
        , school = school
        )
}
