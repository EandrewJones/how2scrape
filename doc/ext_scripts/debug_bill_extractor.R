#===========================
# Generic scraping function
#===========================

bill_extractor <- function(url) {
  
  # Libraries
  require(rvest, quietly = T)
  require(dplyr, quietly = T)
  require(stringr, quietly = T)
  
  # Read in page
  html.legislation <- read_html(url)
  
  # Extract the bill number
  
  bill_number <- html.legislation %>% 
    html_nodes('.expanded .result-heading a') %>% 
    html_text()
  
  # Extract link to bill text
  
  text_link <- html.legislation %>% 
    html_nodes('.expanded .result-heading a') %>% 
    html_attrs() %>% 
    unlist() %>% 
    unname()
  
  # Extract short title
  
  short_title <- html.legislation %>% 
    html_nodes('.expanded .result-title') %>% 
    html_text()
  
  # Extract sponsor
  
  sponsor <- html.legislation %>% 
    html_nodes('.result-title+ .result-item a:nth-child(2)') %>% 
    html_text()
  
  # Extact number of co-sponsors
  
  N_cosponsors <- html.legislation %>% 
    html_nodes('.expanded .result-item a~ a') %>% 
    html_text() %>% 
    as.numeric()
  
  # Extact link to co-sponsor names
  
  cosponsors_link <- html.legislation %>% 
    html_nodes('.expanded .result-item a~ a') %>% 
    html_attrs() %>% 
    unlist() %>% 
    unname()
  
  # Extract committee
  
  committee <- html.legislation %>% 
    html_nodes('.expanded .result-item:nth-child(5)') %>% 
    html_text() %>% 
    str_replace_all(pattern = "\n", replacement = "") %>%
    str_replace_all(pattern = "Committees:", replacement = "") %>% 
    trimws()
  
  df_bills <- data.frame(bill_number, 
                         short_title,
                         sponsor,
                         N_cosponsors,
                         committee,
                         cosponsors_link,
                         text_link,
                         stringsAsFactors = F)
  
  # Assign df_bills to global environment
  assign('df_bills',
         value = df_bills,
         envir = .GlobalEnv)
}

#=======
# Debug
#=======

# Extract the total bill count using rvest

N_bills <- html.legislation %>% 
  html_nodes('#facetItemsourceLegislationcount') %>% 
  html_text() %>% 
  str_remove_all("\\[|\\]|,") %>% 
  as.numeric()

# Calculate the total number of pages

N_pages <- ceiling(N_bills / 250)

# Enumerate frontier by pasting the prefix url and seq of N_pages together

frontier <- paste0('https://www.congress.gov/search?q={"source":"legislation","congress":"115","type":"bills"}&pageSize=250&page=', 
                   seq(1,N_pages))

# Create safe version of function for debugging
safe_bill <- safely(bill_extractor, otherwise = NA_real_)

# Map over frontier with safe_bill
# Multiprocess the map using furrr to speed things up
require(furrr)
plan(multiprocess)
safe_df <- future_map(frontier, safe_bill)

# Check which pages errors occur on
for (i in 1:N_pages) {print(paste(i, safe_df[[i]][2], sep = ' '))}

#==========================================================
# Tweak parts below as needed and then replace in function
# - Rinse and repeat until function runs error free 
# - see bottom for finalized function
#==========================================================

url <- frontier[27]

# Read in page
html.legislation <- read_html(url)

# Extract short title
short_title <- html.legislation %>% 
  html_nodes('.expanded .result-title') %>% 
  html_text() %>% 
  str_remove_all('\\\\"')
# Some bills are entitled 'Reserved for the Minority Leader.' or 'Reserved for the Speaker."
# They do not offer any other info and break our function if we do not remove them. 
# Save their position to remove
r <- which(short_title=="Reserved for the Minority Leader." | short_title=="Reserved for the Speaker.")
short_title <- short_title %>% 
  {if(length(r) > 0) .[-r] else .}

# Extract the bill number
bill_number <- html.legislation %>% 
  html_nodes('.expanded .result-heading a') %>% 
  html_text() %>% 
  # Drop those reserved for minority leader/speaker
  {if(length(r) > 0) .[-r] else .}

# Extract link to bill text
text_link <- html.legislation %>% 
  html_nodes('.expanded .result-heading a') %>% 
  html_attrs() %>% 
  unlist() %>% 
  unname() %>% 
  {if(length(r) > 0) .[-r] else .}

# Extract sponsor
sponsor <- html.legislation %>% 
  html_nodes('.result-title+ .result-item a:nth-child(2)') %>% 
  html_text()

# Extact number of co-sponsors
# occassionally, this picks up extra elements such as ammendments and introduces NA's into vector when
# converted to numeric, we need to remove these but first store their position in the vector so we can
# remove their associated hyperlinks
N_cosponsors <- html.legislation %>% 
  html_nodes('.expanded .result-item a~ a') %>% 
  html_text() %>% 
  as.numeric 
k <- which(!is.na(N_cosponsors))
N_cosponsors <- N_cosponsors[k]

# Extract link to co-sponsor names
cosponsors_link <- html.legislation %>% 
  html_nodes('.expanded .result-item a~ a') %>% 
  html_attrs() %>% 
  unlist() %>% 
  unname() %>% 
  .[k]

# Extract committee
committee <- html.legislation %>% 
  html_nodes('.expanded .result-item:nth-child(5)') %>% 
  html_text() %>% 
  str_replace_all(pattern = "\n", replacement = "") %>%
  str_replace_all(pattern = "Committees:", replacement = "") %>% 
  trimws()

# Store as df
df_bills <- data.frame(bill_number, 
                       short_title,
                       sponsor,
                       N_cosponsors,
                       committee,
                       cosponsors_link,
                       text_link,
                       stringsAsFactors = F)

#==================
# Final function
#==================

bill_extractor <- function(url) {
  
  # Libraries
  require(rvest, quietly = T)
  require(dplyr, quietly = T)
  require(stringr, quietly = T)
  
  # Read in page
  html.legislation <- read_html(url)
  
  # Extract short title
  short_title <- html.legislation %>% 
    html_nodes('.expanded .result-title') %>% 
    html_text() %>% 
    str_remove_all('\\\\"')
  
  # Some bills are entitled 'Reserved for the Minority Leader.' or 'Reserved for the Speaker."
  # They do not offer any other info and break our function if we do not remove them. 
  # Save their position to remove
  r <- which(short_title=="Reserved for the Minority Leader." | short_title=="Reserved for the Speaker.")
  
  # Extract the bill number
  bill_number <- html.legislation %>% 
    html_nodes('.expanded .result-heading a') %>% 
    html_text() %>% 
    # Drop those reserved for minority leader/speaker
    {if(length(r) > 0) .[-r] else .}
  
  # Extract link to bill text
  text_link <- html.legislation %>% 
    html_nodes('.expanded .result-heading a') %>% 
    html_attrs() %>% 
    unlist() %>% 
    unname() %>% 
    {if(length(r) > 0) .[-r] else .}
  
  # Extract sponsor
  sponsor <- html.legislation %>% 
    html_nodes('.result-title+ .result-item a:nth-child(2)') %>% 
    html_text()
  
  # Extact number of co-sponsors
  # occassionally, this picks up extra elements such as ammendments and introduces NA's into vector when
  # converted to numeric, we need to remove these but first store their position in the vector so we can
  # remove their associated hyperlinks
  N_cosponsors <- html.legislation %>% 
    html_nodes('.expanded .result-item a~ a') %>% 
    html_text() %>% 
    as.numeric 
  k <- which(!is.na(N_cosponsors))
  N_cosponsors <- N_cosponsors[k]
  
  # Extract link to co-sponsor names
  cosponsors_link <- html.legislation %>% 
    html_nodes('.expanded .result-item a~ a') %>% 
    html_attrs() %>% 
    unlist() %>% 
    unname() %>% 
    .[k]
  
  # Extract committee
  committee <- html.legislation %>% 
    html_nodes('.expanded .result-item:nth-child(5)') %>% 
    html_text() %>% 
    str_replace_all(pattern = "\n", replacement = "") %>%
    str_replace_all(pattern = "Committees:", replacement = "") %>% 
    trimws()
  
  # Store as df
  df_bills <- data.frame(bill_number, 
                         short_title,
                         sponsor,
                         N_cosponsors,
                         committee,
                         cosponsors_link,
                         text_link,
                         stringsAsFactors = F)
  
  # Assign df_bills to global environment
  assign('df_bills',
         value = df_bills,
         envir = .GlobalEnv)
}

