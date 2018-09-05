rm(list=ls(all=T))

# load bills df

load('/a/ha-nfs-2-ib/export/data/bswift-1/eajones3/df.RData')

# Libraries

  require(rvest)
  require(dplyr)
  require(stringr)

# Fix text urls

df_bills <- df_bills %>%
  mutate(text_link = gsub('(\\?)', '/text?format=txt&', text_link))

# Cosponsor extracting function

  co_extractor <- function(url) {
    # Libaries
    require(rvest)
    require(dplyr)

    # Read html
    html.cosponsors <- read_html(url)

    # Extract cosponsor name(s)
    cosponsors <- html.cosponsors %>%
      html_nodes('#main a') %>%
      html_text()

    # Extract date of cosponsorship
    date_cosponsor <- html.cosponsors %>%
      html_nodes('td.date') %>%
      html_text()

    # Paste both vectors together
    # Separate by hypen for easy string splits later
    m <- paste(cosponsors, date_cosponsor, sep = ' - ')

    # If N_cosponsors > 1, collapse by semi-colon for easy string split later
    if (length(m) > 1) m <- paste(m, sep = '', collapse = '; ')
    if (length(m) == 0) m <- "No cosponsors"

    # Put in a random crawl-delay between 2 and 6
    d <- runif(1,2,6)
    Sys.sleep(d)

    return(m)

  }

# Text extracting function

  text_extractor <- function(url) {

    # Libraries
    require(rvest)
    require(dplyr)
    require(stringr)

    # Read html
    html <- read_html(url)

    # # Create logical proxy for presence of text
    # proxy_text <- html %>%
    #   html_nodes('.selected a') %>%
    #   html_text() %>%
    #   grepl('\\(',.)

    # Create logical proxy for enrolled bill or not
    proxy_enrolled <- html %>%
      html_nodes('#textVersion') %>%
      html_text() %>%
      grepl('Enrolled',.) %>%
      {if (length(.)==0) . <- FALSE else .}

      if (proxy_enrolled) {

        text <- html %>%
          # extract text of bill
          html_nodes('#billTextContainer') %>%
          html_text() %>%
          # Second check to ensure ther
          # Split into meaningful parts
          str_split(., '\n{2}') %>%
          .[[1]] %>%
          # drop blank elements
          .[. != ''] %>%
          # Drop everything prior to start of main text
          .[-(1:grep('An Act', .)[1])] %>%
          # Remove excess white space
          str_replace_all('[\n\t]', '') %>%
          trimws()

      } else {

        # Extract text
        text <- html %>%
          html_nodes('#billTextContainer') %>%
          html_text() %>%
          # Check to ensure there is text
          { if (length(.) > 0) . <-
              # Split into meaningful parts
              str_split(., '\n{2}') %>%
              .[[1]] %>%
              # Drop blank elements
              .[. != ''] %>%
              # Drop everything prior to start of main text
              .[-(1:grep('BILL|ACT|AMENDMENTS', .)[1])] %>%
              # remove excess white space
              str_replace_all('[\n\t]', '') %>%
              trimws() else . <- "No text available"
              }
      }

    # Put in a random crawl-delay between 2 and 6
    d <- runif(1,2,6)
    Sys.sleep(d)

    return(text)

  }

# Create safe version of text_extractor with purrr::possibly

  try_text <- purrr::possibly(text_extractor, otherwise = NA)

# Parallel Processing Libraries

  library(foreach)
  library(parallel)
  library(doParallel)

# Make cluster of cores

  cl <-  makeCluster(4, type = "FORK")

# Register the cluster

  registerDoParallel(cl)

# Create class which holds multiple results for each loop iteration.
# Each loop iteration populates two properties: $result1 and $result2.
# For a great tutorial on S3 classes, see:
# http://www.cyclismo.org/tutorial/R/s3Classes.html#creating-an-s3-class

  multiResultClass <- function(result1=NULL, result2=NULL)
  {
    me <- list(
      list(result1),
      list(result2)
    )

    ## Set the name for the class
    class(me) <- append(class(me),"multiResultClass")
    return(me)
  }

# Parallel loop over the cosponsor_links and bill text to extract
# respective data

  bill_container <- foreach(i = 1:nrow(df_bills)) %dopar% {
    result <- multiResultClass()
    result[[1]] <- co_extractor(df_bills$cosponsors_link[i])
    result[[2]] <- try_text(df_bills$text_link[i])
    return(result)
  }

# Stop cluster

  stopCluster(cl)

# Save result

  save(bill_container,
       file = '/a/ha-nfs-2-ib/export/data/bswift-1/eajones3/result.RData')
