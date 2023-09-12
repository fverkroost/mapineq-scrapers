#
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(RSelenium)) install.packages("RSelenium", repos = "http://cran.us.r-project.org")
if(!require(xlsx)) install.packages("xlsx", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
# if(!require(rstudioapi)) install.packages("rstudioapi", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
# if(!require(netstat)) install.packages("netstat", repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")

# Load libraries
library(rvest)
library(plyr)
library(dplyr)
library(RSelenium)
library(xlsx)
library(readxl)
library(stringr)
library(RCurl)
# library(rstudioapi)
# library(netstat)

#
create_remote_driver <<- function(ip_address, input_args){
  remote_driver = remoteDriver$new(
    remoteServerAddr = ip_address,
    port = as.integer(as.character(input_args$port)),
    browserName = "chrome",
    extraCapabilities = list(
      username = as.character(input_args$usrname),
      accessKey = as.character(input_args$passwd),
      "chromeOptions" = list(args = list('--headless'))
    ),
    #  version = "115.0.5790.102",
    # platform="MAC"
  )
  return(remote_driver)
}

#
check_and_dismiss_alert = function(remote_driver){
  for (try in 1:10){
    alert_present = tryCatch({
      suppressMessages(remote_driver$getAlertText()[[1]])
      TRUE
    }, error = function(e){
      return(FALSE)
    })
    if (alert_present){ break }
  }
  if (alert_present){ 
    remote_driver$dismissAlert() 
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#
scrape_school_data = function(
    remote_driver,
    xpath_list
){
  this_row = data.frame(matrix(NA, nrow = 1, ncol = 0))
  for (i in seq_along(xpath_list)){
    this_row[names(xpath_list)[i]] = tryCatch({
      elem = remote_driver$findElement(using = "xpath", value = xpath_list[[i]])
      elem$getElementText()[[1]]
    }, error = function(e){ NA })
  }
  return(this_row)
}

# Switch to window in browser
myswitch = function (remDr, windowId) 
{
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, 
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}

# 
scrape_house_data = function(
    remDr,
    xpaths
){
  df_house = data.frame(matrix(NA, nrow = 1, ncol = length(xpaths)))
  names(df_house) = names(xpaths)
  for (j in seq_along(xpaths)){
    el = wait_and_load_element(remDr, xpaths[[j]])
    if (!is.null(el)){
      df_house[1, j] = el$getElementText()[[1]]
    } else {
      df_house[1, j] = NA
    }
    rm(el)
  }
  return(df_house)
}

# Print with timestamp
tprint = function(x){
  message(paste0('[', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '] ', x, '\n'))
}

# Function that finds an element only when webpage is fully loaded
wait_and_load_element = function(
    remote_driver,
    x_path,
    max_wait_time_sec = 10,
    print_progress = F,
    usage = "xpath"
){
  if (print_progress){ tprint(paste0('     Waiting to load element with XPath: ', x_path)) }
  start_time = Sys.time()
  elem = NULL
  while(is.null(elem) & (as.numeric(Sys.time() - start_time, units = "secs") <= max_wait_time_sec)){
    elem = tryCatch({
      suppressMessages({
        remote_driver$findElement(using = usage, x_path)
      })
    }, error = function(e){ NULL })
  }
  return(elem)
}

# Function that obtains address data from XPaths
obtain_address_information = function(
    x_path,
    nams = NULL
){
  x_path_list = as.list(x_path)
  df = data.frame(matrix(NA, nrow = 1, ncol = length(x_path)))
  for (i in 1:length(x_path_list)){
    df[1, i] = tryCatch({
      wait_and_load_element(remDr, x_path_list[[i]])$getElementText()[[1]]
    }, error = function(e){NA})
  }
  if (is.null(nams)){
    names(df) = names(x_path)
  } else {
    names(df) = nams
  }
  df["scrape_date"] = Sys.time()
  return(df)
}

scrape_neighborhood_data = function(remDr, url){
  
  # Navigate to the correct url
  # remDr$navigate(url)
  
  # Obtain trend information
  n_trends = 4
  n_infos = c('h4', 'h3', 'div')
  trend_df = data.frame(matrix(NA, nrow = n_trends, ncol = length(n_infos)))
  for (i in 1:n_trends){
    for (j in seq_along(n_infos)){
      elem = wait_and_load_element(remDr, paste0('//*[@id="trending"]/div[2]/div[', i, ']/', n_infos[j]))
      trend_df[i, j] = ifelse(!is.null(elem), elem$getElementText()[[1]], NA)
    }
  }
  names(trend_df) = c("name", "value", "trend")
  
  # Obtain demographic information
  n_demos = 5
  demo_df = data.frame(matrix(NA, nrow = n_demos, ncol = 2))
  for (i in 1:n_demos){
    elem = wait_and_load_element(remDr, paste0('//*[@id="buurt"]/div[1]/p[', i, ']'))
    for (j in 1:2){
      demo_df[i, j] = ifelse(!is.null(elem), str_split(elem$getElementText()[[1]], "\\?\n")[[1]][j], NA)
    }
  }
  names(demo_df) = c("name", "value")
  
  # Obtain income per household
  inc_elem = wait_and_load_element(remDr, '//*[@id="buurt"]/div[2]/div[2]/div/span')
  inc_df = data.frame(
    name = "besteedbaar inkomen huishouden",
    value = ifelse(!is.null(inc_elem), inc_elem$getElementText()[[1]], NA)
  )
  
  # Combine data
  buurt_df = plyr::rbind.fill(list(trend_df, demo_df, inc_df))
  
  # Return result
  return(buurt_df)
}



return_to_base = function(
    remDr,
    postcode,
    main_page_url
){
  
  
  # Visit main page URL
  remDr$navigate(main_page_url)
  
  # Enter zip code in search bar
  zip_input = wait_and_load_element(remDr, '//*[@id="search-input"]')
  zip_input$clearElement()
  zip_input$sendKeysToElement(list(postcode, key = "enter"))
  
  # Wait until page is fully loaded
  current_url = main_page_url
  while (!grepl("zoom", current_url)){
    Sys.sleep(1)
    current_url = zip_input$getCurrentUrl()
  }
  rm(zip_input)
  
}


