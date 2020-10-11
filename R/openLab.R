#' Open Lab Function
#'
#' This function allows you to open the lab files.
#' @param name Name of the lab to open. Defaults to 'intro'. Options include 'robots', 'simple', 'web_scraping', 'api', and 'cosponsors'
#' @keywords labs
#' @export
#' @examples
#' openLab('robots')

openLab <- function(name = 'intro') {

  # Open main
  if(name=='intro') {
    utils::RShowDoc('introduction', package = 'how2scrape')
  } else if(name=='robots') { # open robot
    utils::RShowDoc('robots', package = 'how2scrape')
  } else if(name=='simple') { # Open simple scrape
    utils::RShowDoc('simple_scrape', package = 'how2scrape')
  } else if(name=='web_scraping') { # Open web scraping
    utils::RShowDoc('web_scraping', package = 'how2scrape')
  } else if(name=='cosponsors') { # Open cosponsors and text scrape
    utils::RShowDoc('cosponsors', package = 'how2scrape')
  } else if(name=='api') {
    utils::RShowDoc('api', package = 'how2scrape')
  } else {
    stop('File not found! Try main, robots, simple, or cosponsor.')
  }
}
