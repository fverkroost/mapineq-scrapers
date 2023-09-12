# Clear the environment and console
cat("\014")
rm(list = ls())

# Load libraries and functions
# main_dir = getwd()
main_dir = dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(main_dir, 'input_and_functions.R'))

# Define output folder for scraped data
out_folder = file.path(main_dir, "scraped_data/huizenzoeker/")
if (!dir.exists(out_folder)){ dir.create(out_folder, recursive = T) }

# Open the remote browser
my_port = as.character(netstat::free_port())
system(paste0("docker run -d -p ", my_port, ":4444 --shm-size 4g selenium/standalone-chrome:4.2.2"))
Sys.sleep(10)
main_page_url = "https://www.huizenzoeker.nl/koop/"
remDr = remoteDriver$new(
  browserName = "chrome",
  port = as.integer(my_port),
  extraCapabilities = list(
    "chromeOptions" = list(args = list('--headless'))
  )
)
remDr$open(silent = T)
remDr$navigate(main_page_url)

# Refuse cookies
# cookie_elem = wait_and_load_element(remDr, '/html/body/div[1]/div/button[2]')
cookie_elem = wait_and_load_element(remDr, 'cookie-consent-deny', usage = 'class name')
if (!is.null(cookie_elem)){
  cookie_elem$clickElement()
}
rm(cookie_elem)

# Define XPaths for house information
xpaths = list(
  straat_nr = '//*[@id="wrapper"]/div[1]/section[2]/div[1]/h2',
  postcode_plaats = '//*[@id="wrapper"]/div[1]/section[2]/div[1]/h3',
  vraagprijs = '//*[@id="wrapper"]/div[1]/section[2]/div[2]/div',
  beschrijving = '//*[@id="address-info"]/div[1]/p',
  type = '//*[@id="address-info"]/div[2]/div[1]/div[2]/b',
  opp_woon = '//*[@id="address-info"]/div[2]/div[2]/div[2]/b',
  opp_perceel = '//*[@id="address-info"]/div[2]/div[3]/div[2]/b',
  bouwjaar = '//*[@id="address-info"]/div[2]/div[4]/div[2]/b',
  woningwaarde = '//*[@id="cadastral-data"]/div[1]/p[2]',
  treinstation = '//*[@id="wrapper"]/div[1]/div/div[1]/section[3]/div/div[1]/div/p[2]',
  bushalte = '//*[@id="wrapper"]/div[1]/div/div[1]/section[3]/div/div[2]/div/p[2]',
  kinderopvang = '//*[@id="wrapper"]/div[1]/div/div[1]/section[3]/div/div[3]/div/p[2]',
  basisschool = '//*[@id="wrapper"]/div[1]/div/div[1]/section[3]/div/div[4]/div/p[2]',
  supermarkt = '//*[@id="wrapper"]/div[1]/div/div[1]/section[3]/div/div[5]/div/p[2]',
  bioscoop = '//*[@id="wrapper"]/div[1]/div/div[1]/section[3]/div/div[6]/div/p[2]',
  buurt = '//*[@id="neighbourhood-info"]/div[1]/p[1]',
  straat = '//*[@id="neighbourhood-info"]/div[1]/p[2]',
  criminaliteit = '//*[@id="neighbourhood-info"]/div[1]/p[3]',
  eigenschap1 = '//*[@id="wrapper"]/div[1]/div/div[1]/section[5]/div/div[1]/div/p',
  eigenschap2 = '//*[@id="wrapper"]/div[1]/div/div[1]/section[5]/div/div[2]/div/p',
  eigenschap3 = '//*[@id="wrapper"]/div[1]/div/div[1]/section[5]/div/div[3]/div/p',
  eigenschap4 = '//*[@id="wrapper"]/div[1]/div/div[1]/section[5]/div/div[4]/div/p',
  eigenschap5 = '//*[@id="wrapper"]/div[1]/div/div[1]/section[5]/div/div[5]/div/p',
  eigenschap6 = '//*[@id="wrapper"]/div[1]/div/div[1]/section[5]/div/div[6]/div/p'
)

# Obtain the number of results for zip code
nr_elem = wait_and_load_element(remDr, "detail__result_message", usage = "class name")
nr_results = tryCatch({
  nr_elem$getElementText()[[1]]
}, error = function(e){ NULL })
res_count = ifelse(is.null(nr_results) | nr_results == "", "", as.numeric(gsub("([0-9]+).*$", "\\1", nr_results)))

# Initialize data frames and counters
match_cols = c(names(xpaths), "longitude", "latitude")
start_scrape_date = Sys.Date()
sale_file = paste0(out_folder, "sale_data_", gsub("-", "", start_scrape_date), ".csv")
if (file.exists(sale_file)){ 
  sale_data = read.csv(sale_file)
  sale_data = sale_data[, names(sale_data) != "X"]
} else {
  sale_nams = c(match_cols, "index", "scrape_date")
  sale_data = data.frame(matrix(NA, nrow = 0, ncol = length(sale_nams)))
  names(sale_data) = sale_nams
}
nbh_file = paste0(out_folder, "nbh_data_", gsub("-", "", start_scrape_date), ".csv")
if (file.exists(nbh_file)){ 
  nbh_data = read.csv(nbh_file)
  nbh_data = nbh_data[, names(nbh_data) != "X"]
} else {
  nbh_nams = c("name", "value", "trend", "neighborhood_name", "neighborhood_url", "scrape_date")
  nbh_data = data.frame(matrix(NA, nrow = 0, ncol = length(nbh_nams)))
  names(nbh_data) = nbh_nams
}

# Scrape house and neighborhood data for all houses up for sale
counter_this_page = 0
pb = txtProgressBar(min = 0, max = res_count, style = 3, width = 50, char = "=")  
for (idx in seq_len(res_count)){
  
  tryCatch({
    
    # Obtain the current housing ad
    house_ad = wait_and_load_element(
      remDr, 
      paste0("woningmarkt-list-item-", counter_this_page), 
      usage = "class name", 
      max_wait_time_sec = 60
    )
    #house_ad$setTimeout()
    
    # If a housing ad is available, navigate to it and scrape the data
    # If not available, navigate to the next page of results and start from the top
    if (!is.null(house_ad)){
      
      # Obtain the address to check if data has already been scraped or not
      splits = str_split(house_ad$getElementText()[[1]], "\n")[[1]]
      house_address = splits[1]
      
      # If data for this house has not been scraped yet, then do so now
      match_sale = which(sale_data$straat_nr == house_address) # & substr(sale_data$scrape_date, 1, 10) == Sys.Date())
      if (length(match_sale) <= 0){ 
        
        # Print address to collect data for
        tprint(paste0('   Scraping house data for ', house_address))
        
        # Find house element in list of results and save coordinates
        house_long = house_ad$getElementAttribute("data-lon")[[1]]
        house_lat = house_ad$getElementAttribute("data-lat")[[1]]
        
        # Obtain the URL of the specific housing ad
        house_card = house_ad$findChildElement("class name", "woningmarkt-card")
        house_script = house_card$getElementAttribute("onclick")[[1]]
        rm(house_ad, house_card)
        
        # Use switcher to navigate to new tab and scrape data for house there
        # because reloading the results gives dynamic and changing result each time
        remDr$executeScript("window.open('');")
        windows_handles = remDr$getWindowHandles()
        # myswitch(remDr, windows_handles[[2]])
        remDr$switchToWindow(windows_handles[[2]])
        remDr$navigate(main_page_url)
        remDr$executeScript(house_script)
        rm(house_script)
        
        # Scrape data from house page
        df_house = scrape_house_data(remDr, xpaths)
        df_house["longitude"] = house_long
        df_house["latitude"] = house_lat
        df_house["index"] = idx
        df_house["scrape_date"] = Sys.time()
        match_rows = nrow(match_df(sale_data, df_house, on = match_cols))
        if (match_rows <= 0){ sale_data = rbind(sale_data, df_house) }
        
        # Save data to file
        write.csv(sale_data, sale_file, row.names = F)
        
        # Obtain URL to navigate to the neighborhood page
        buurt_elem = wait_and_load_element(remDr, '//*[@id="trending"]/div[1]/h2')
        if (!is.null(buurt_elem)){
          buurt_url = buurt_elem$getCurrentUrl()[[1]]
          nbh = gsub("Woningmarkt ", "", buurt_elem$getElementText()[[1]])
        } else {
          nbh_elem = wait_and_load_element(remDr, "woningmarkt-image-button", usage = "class name")
          url_of_house = nbh_elem$getCurrentUrl()[[1]]
          nbh = str_split(df_house$postcode_plaats[1], ", ")[[1]][2]
          splits = str_split(url_of_house, "/")[[1]]
          buurt_url = paste0(splits[1:(length(splits)-3)], collapse = "/")
        }
        
        # Close house tab and switch to main tab of results
        remDr$closeWindow()
        # myswitch(remDr, windows_handles[[1]])
        remDr$switchToWindow(windows_handles[[1]])
        rm(windows_handles)
        
        # Scrape data for neighborhood (if not done so already)
        rm(buurt_elem)
        match_nbh = which(nbh_data$neighborhood_name == nbh) # & substr(nbh_data$scrape_date, 1, 10) == Sys.Date())
        if (length(match_nbh) <= 0){
          
          # Print neighborhood to collect data for
          tprint(paste0('   Scraping neighborhood data for ', nbh))
          
          # Open neighborhood page
          remDr$executeScript("window.open('');")
          windows_handles = remDr$getWindowHandles()
          # myswitch(remDr, windows_handles[[2]])
          remDr$switchToWindow(windows_handles[[2]])
          remDr$navigate(buurt_url)
          
          # Scrape neighborhood data
          nbh_df = scrape_neighborhood_data(remDr, buurt_url)
          nbh_df["neighborhood_name"] = rep(nbh, nrow(nbh_df))
          nbh_df["neighborhood_url"] = rep(buurt_url, nrow(nbh_df))
          nbh_df["scrape_date"] = Sys.time()
          nbh_data = rbind(nbh_data, nbh_df)
          rm(nbh_df)
          
          # Close neighborhood tab and switch to main tab of results
          remDr$closeWindow()
          # myswitch(remDr, windows_handles[[1]])
          remDr$switchToWindow(windows_handles[[1]])
          rm(windows_handles)
          
          # Save data to file
          write.csv(nbh_data, nbh_file, row.names = F)
          
        } else {
          
          tprint(paste0('   Skipping neighborhood ', nbh, ': data already scraped'))
        }
        
      } else {
        
        tprint(paste0('   Skipping address ', house_address, ': data already scraped'))
        
      }
      
      # Increase the page counter by one
      counter_this_page = counter_this_page + 1
      
    } else {
      
      # Navigate to next page of results and reset page counter
      next_xp = '//*[@id="wrapper"]/section/div[2]/div[2]/div/div[2]/ul/li[2]/button'
      next_elem = wait_and_load_element(remDr, next_xp)
      if (!is.null(next_elem)){ next_elem$clickElement() }
      #next_elem$setTimeout()
      rm(next_elem)
      counter_this_page = 0
      
    }
    
  }, error = function(cond){
    
    # Print skip and error message
    tprint(paste0('   Skipping index ', idx, ' because of problems with scraping data'))
    cat("Reason: ERROR ", conditionMessage(cond), "\n")
    
    # Increase the page counter by one
    counter_this_page = counter_this_page + 1
    
  }, finally = {
    
    # Update the progress bar
    setTxtProgressBar(pb, idx)
    
  })
  
}

# Close the remote driver
remDr$close()

# Close progress bar
close(pb)

# Kill the terminal
# rstudioapi::terminalKill(termId)

# Stop Docker container
stopping = system("docker ps", intern = T)
cont_name = str_split(stopping[grepl(my_port, stopping)], "   ")[[1]][7]
system(paste0("docker stop ", cont_name))
