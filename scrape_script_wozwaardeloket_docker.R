# Clear the environment and console
cat("\014")
rm(list = ls())

# Load libraries and functions
# main_dir = getwd()
main_dir = dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(main_dir, 'input_and_functions.R'))

# Define output folder for scraped data
out_folder = file.path(main_dir, "scraped_data/wozwaardeloket/")
if (!dir.exists(out_folder)){ dir.create(out_folder, recursive = T) }

# List of zip codes
temp = tempfile()
download.file('https://download.cbs.nl/postcode/2023-cbs_pc6_2022_v1.zip', temp)
zip_codes_nl = read_xlsx(unzip(temp, "pc6_2022_v1.xlsx"), range = cell_cols("A"))
unlink(temp)
zip_codes_nl = data.frame(zip_codes_nl)[, 1]
zip_codes_nl = zip_codes_nl[(which(zip_codes_nl == "Code") + 1):length(zip_codes_nl)]

# Open the remote browser
my_port = as.character(netstat::free_port())
system(paste0("docker run -d -p ", my_port, ":4444 --shm-size 4g selenium/standalone-chrome:4.2.2"))
Sys.sleep(10)
main_page_url = "https://www.wozwaardeloket.nl/#"
remDr = remoteDriver$new(
  browserName = "chrome",
  port = as.integer(my_port),
  extraCapabilities = list(
    "chromeOptions" = list(args = list('--headless'))
  )
)
remDr$open(silent = T)
remDr$navigate(main_page_url)

# Wait until the page is loaded and continue to the interactive map
map_elem = wait_and_load_element(remDr, '//*[@id="kaart-bekijken-btn"]')
map_elem$clickElement()
main_url = map_elem$getCurrentUrl()[[1]]

# Define elements of address information
xpaths = list(
  straat = '//*[@id="adres-straat"]',
  koppel = '//*[@id="adres-koppel"]',
  postcode = '//*[@id="adres-postcode"]',
  woonplaats = '//*[@id="adres-woonplaats"]',
  objectnummer = '//*[@id="kenmerk-wozobjectnummer"]',
  grondoppervlakte = '//*[@id="kenmerk-grondoppervlakte"]',
  bouwjaar = '//*[@id="kenmerk-bouwjaar"]',
  gebruiksdoel = '//*[@id="kenmerk-gebruiksdoel"]',
  oppervlakte = '//*[@id="kenmerk-oppervlakte"]',
  objectid = '//*[@id="link-adresseerbaarobjectid"]',
  nummeraanduiding = '//*[@id="link-nummeraanduidingid"]'
)

# Initialize data to obtain from and save to
woz_file = paste0(out_folder, "woz_data_", gsub("-", "", Sys.Date()), ".csv")
if (file.exists(woz_file)){
  saved_addresses = read.csv(woz_file)
  saved_addresses = saved_addresses[, names(saved_addresses) != "X"]
} else {
  nams = c(names(xpaths), "scrape_date", "index")
  saved_addresses = data.frame(matrix(NA, nrow = 0, ncol = length(nams)))
  names(saved_addresses) = nams
}

# Obtain data for each of the zip codes
counter = 1
for (postcode in zip_codes_nl){
  
  out = "error"
  for (try in 1:10){
    out = tryCatch({
      
      # Check if there is a maximum request error and click cross if yes
      error_xp = '//*[@id="cdk-overlay-1"]/app-error/app-modal/div[2]/div/div[2]/p'
      error_elem = wait_and_load_element(remDr, error_xp, max_wait_time_sec = 10)
      if (!is.null(error_elem)){
        close_elem = wait_and_load_element(remDr, '//*[@id="modal-close"]')
        close_elem$clickElement()
        rm(close_elem)
      }
      rm(error_elem)
      
      # Insert post code to get list of results
      zip_input = wait_and_load_element(remDr, '//*[@id="ggcSearchInput"]')
      zip_input$clearElement()
      zip_input$sendKeysToElement(list(postcode))
      zip_input$setTimeout()
      
      # Click on the first row of suggested addresses
      sugg_elem = wait_and_load_element(remDr, '//*[@id="ggcSuggestionList-0"]')
      if (!is.null(sugg_elem)){ 
        sugg_elem$clickElement() 
      } else {
        zip_input$sendKeysToElement(list("\uE007"))
      }
      rm(sugg_elem, zip_input)
      
      # Wait until the page is loaded and continue to the interactive map
      # Expand to see the first result of zip code
      result_elem = wait_and_load_element(remDr, '//*[@id="resultaat-0"]')
      result_elem$setTimeout()
      result_elem$clickElement()
      rm(result_elem)
      
      # Number of last result
      end_xp = '/html/body/app-root/div/main/app-sidebar/div[1]/div[2]/div/app-sidebar-block[3]/section/div[2]/app-result-nav/nav/ul/li[4]/span'
      end_elem = wait_and_load_element(remDr, end_xp)
      last_result_nr = as.numeric(gsub("\\D", "", end_elem$getElementText()[[1]])) 
      rm(end_elem)
      
      # Return positive result
      "success"
      
    }, error = function(e){ "error" })
    
    if (out == "success"){ break }
  }
  if (out == "error"){ 
    tprint(paste0('     Skipping postal code ', postcode, ' because of lacking results'))
    next 
  }
  
  # Print message
  tprint(paste0('     Collecting WOZ data for properties at zip code ', postcode))
  
  # Loop through houses with this specific zip code and show progress bar
  pb = txtProgressBar(min = 0, max = last_result_nr, style = 3, width = 50, char = "=") 
  for (idx in seq_len(last_result_nr)){
    
    tryCatch({
      
      # Check if there is a maximum request error and click cross if yes
      error_xp = '//*[@id="cdk-overlay-1"]/app-error/app-modal/div[2]/div/div[2]/p'
      error_elem = wait_and_load_element(remDr, error_xp, max_wait_time_sec = 10)
      if (!is.null(error_elem)){
        close_elem = wait_and_load_element(remDr, '//*[@id="modal-close"]')
        close_elem$clickElement()
        rm(close_elem)
      }
      rm(error_elem)
      
      # Obtain the postcode information
      house_postcode = obtain_address_information(xpaths$postcode, "pc")$pc
      
      # Reclick list of results if not done so yet
      if (is.na(house_postcode)){
        count_na = 0
        result_elem = NULL
        while (count_na <= 10 & is.null(result_elem)){
          result_elem = wait_and_load_element(remDr, '//*[@id="resultaat-0"]', max_wait_time_sec = 120)
          count_na = count_na + 1
        }
        result_elem$clickElement()
        rm(result_elem)
      }
      
      # Scrape WOZ housing data
      if (!is.na(house_postcode)){
        
        if (house_postcode != postcode){
          # Print address to collect data for
          tprint('   Warning: postcode does not match target, but preparing to scrape data anyway')
        }
        
        # Obtain the address information
        house_info = obtain_address_information(xpaths)
        
        # Check if zip code is actually the one required or just a close suggestion
        match_idx = which(saved_addresses$objectnummer == house_info$objectnummer & 
                            substr(saved_addresses$scrape_date, 1, 10) == Sys.Date())
        if (length(match_idx) <= 0){
          
          # Expand all rows of WOZ values
          expand_elem = wait_and_load_element(remDr, '//*[@id="waarden-show-all"]')
          if (!is.null(expand_elem)){ 
            expand_elem$clickElement() 
            rm(expand_elem)
          }
          
          # Obtain table of WOZ values
          woz_xp = '/html/body/app-root/div/main/app-sidebar/div[1]/div[2]/div/app-sidebar-block[3]/section/div[2]/app-result/table'
          woz = remDr$getPageSource()[[1]] %>%
            read_html() %>%
            html_nodes(xpath = woz_xp) %>%
            html_table()
          
          if (length(woz) > 0){
            woz = woz[[1]]
            
            # Combine address and WOZ data
            house_info_exp = do.call("rbind", replicate(nrow(woz), house_info, simplify = FALSE))
            house_woz = cbind(house_info_exp, woz)
            house_woz["index"] = rep(counter, nrow(house_woz))
            names(house_woz)[grepl("WOZ", names(house_woz)) & grepl("waarde", names(house_woz))] = "woz_waarde" 
            #gsub('[[:punct:] ]+', '', "WOZ_waarde")
            
            # Save house info to saved addresses data frame
            saved_addresses = rbind(
              saved_addresses %>% mutate_all(as.character), 
              house_woz %>% mutate_all(as.character)
            )
            write.csv(saved_addresses, woz_file)
            
            # Add counter for house index
            counter = counter + 1
            
          } else {
            
            # Print address to collect data for
            tprint(paste0('   Skipping ', house_info$straat, ': no WOZ data available'))
            
          }
          
        } else {
          
          # Print address to collect data for
          tprint(paste0('   Skipping ', house_info$straat, ': data already scraped'))
          
        }
        
      } 
      
    }, error = function(cond){
      
      # Print skip and error message
      tprint(paste0('   Skipping index ', idx, ' because of problems with scraping data'))
      cat("Reason: ERROR ", conditionMessage(cond), "\n")
      
    }, finally = {
      
      # Navigate to the next house in line
      next_xp = '/html/body/app-root/div/main/app-sidebar/div[1]/div[2]/div/app-sidebar-block[3]/section/div[2]/app-result-nav/nav/ul/li[5]/a'
      next_elem = wait_and_load_element(remDr, next_xp)
      next_elem$clickElement()
      rm(next_elem)
      
      # Update the progress bar
      setTxtProgressBar(pb, idx)
      
    })
    
  }
  
  # Close progress bar
  close(pb)
  
}

# Close the remote driver
remDr$close()

# Combine the data
# woz_df = rbind.fill(woz_data)

# Kill the terminal
# rstudioapi::terminalKill(termId)

# Stop Docker container
stopping = system("docker ps", intern = T)
cont_name = str_split(stopping[grepl(my_port, stopping)], "   ")[[1]][7]
system(paste0("docker stop ", cont_name))
