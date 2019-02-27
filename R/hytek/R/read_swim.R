read_swim <- function(swimLines) {
    # Extract Metadata CHANGE THIS LATER
    textData <- str_remove_all(swimLines[1], '[0-9:.@#-]')
    textData <- gsub("^\\s+|\\s+$", "", textData)
    textData <- str_split(textData, "  +")[[1]]
    #textData <- unlist(str_extract_all(swimLines[1], '[a-zA-Z]+'))
    
    #swimmerName <- paste(textData[1:2], collapse = ", ")
    swimmerName <- textData[1]
    textDataSchool <- str_split(textData[2], " ")[[1]]
    year <- textDataSchool[1]
    school <- textDataSchool[2]
    
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