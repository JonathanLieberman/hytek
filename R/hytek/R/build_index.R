#' Build index of events
#' 
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom dplyr tibble mutate filter if_else
#' @importFrom stringr str_detect str_extract
#' @param baseUrl 
#' @export
#' @examples
#' \dontrun{
#' build_index('http://www.foo.com/BarMeet/')
#' }
build_index <- function(baseUrl) {
    # Create the full index url 
    fullUrl <- paste0(baseUrl, 'evtindex.htm')
    
    # Parse the html text for hyperlinks
    links <- rvest::html_nodes(xml2::read_html(fullUrl), 'a')
    
    # Extract link texts and urls from the hyperlinks
    events <- dplyr::tibble(
      link_text = rvest::html_text(links)
      , url = rvest::html_attr(links, 'href')
    )
    
    # Filter for just the events
    events <- dplyr::filter(events, stringr::str_detect(link_text, '^#'))
    
    # Extract event data from text
    events <- dplyr::mutate(
      events
      , event_num = stringr::str_extract(link_text, '(?<=^#)[0-9]+')
      , sex = stringr::str_extract(link_text, '(Men|Women)')
      , distance = stringr::str_extract(link_text, '(?<=(Men |Women ))[0-9]+')
      , sport = dplyr::if_else(stringr::str_detect(link_text, 'Diving'), 'Diving', 'Swimming')
      , event = stringr::str_extract(link_text, '(IM|Medley|Fly|Back|Breast|Free|1 mtr|3 mtr|Platform)')
      , finals = !stringr::str_detect(link_text, 'Prelims')
      )
}