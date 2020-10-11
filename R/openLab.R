#' Open Lab Function
#'
#' This function allows you to open the lab files.
#' @param name Name of the lab to open. Defaults to 'main'. Options include 'robots', 'simple', and 'cosponsors'
#' @keywords labs
#' @export
#' @examples
#' openLab('robots')

openLab <- function(name = 'main') {
  
  # Open main
  if(name=='main') {
    utils::RShowDoc('main_lab', package = 'how2scrape')
  } else if(name=='robots') { # open robot
    utils::RShowDoc('robots', package = 'how2scrape')
  } else if(name=='simple') { # Open simple scrape
    utils::RShowDoc('simple_scrape', package = 'how2scrape')
  } else if(name=='cosponsors') { # Open cosponsors and text scrape
    utils::RShowDoc('cosponsors', package = 'how2scrape')
  } else {
    stop('File not found! Try main, robots, simple, or cosponsor.')
  }
}