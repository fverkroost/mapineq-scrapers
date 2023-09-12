# Clear the environment and console
cat("\014")
rm(list = ls())

# Load libraries and functions
main_dir = dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(main_dir, 'input_and_functions.R'))

# Define output folder for scraped data
out_folder = file.path(main_dir, "scraped_data/kinderopvang/")
if (!dir.exists(out_folder)){ dir.create(out_folder, recursive = T) }

#------------------------------------------------------
# Obtain data of daycare facilities
#------------------------------------------------------

# Download data from kinderopvang
ko_df = read.csv('https://www.landelijkregisterkinderopvang.nl/opendata/export_opendata_lrk.csv', sep = ";")

#------------------------------------------------------
# Obtain data of GPS coordinates
#------------------------------------------------------

if (file.exists("gps_coordinates_addresses.csv")){
  gps_df = read.csv("gps_coordinates_addresses.csv")
  gps_df = gps_df[, names(gps_df) != "X"]
  match_idx = match(ko_df$opvanglocatie_adres_volledig, gps_df$opvanglocatie_adres_volledig)
  ko_df["gps_coords"] = gps_df$gps_coords[match_idx]
} else {
  ko_df["gps_coords"] = NA
}

#------------------------------------------------------
# Obtain GPS coordinates of daycare facilities
#------------------------------------------------------

# Open the remote browser
my_port = as.character(netstat::free_port())
system(paste0("docker run -d -p ", my_port, ":4444 --shm-size 4g selenium/standalone-chrome:4.2.2"))
Sys.sleep(10)
main_page_url = "https://www.gps-coordinaten.nl"
remDr = remoteDriver$new(
  browserName = "chrome",
  port = as.integer(my_port),
  extraCapabilities = list(
    "chromeOptions" = list(args = list('--headless'))
  )
)
remDr$open(silent = T)
remDr$navigate(main_page_url)

# Accept selected cookies
cookie_elem = remDr$findElement("selectedcookies", using = "id")
if (!is.null(cookie_elem)){ cookie_elem$clickElement() }

# Get indices for which we do not have the GPS coordinates yet
miss_idx = which(is.na(ko_df$gps_coords))

# Loop through daycare facilities and obtain their GPS coordinates
pb = txtProgressBar(min = 0, max = length(miss_idx), style = 3, width = 50, char = "=") 
counter = 1
for (idx in miss_idx){
  
  # driver.switchTo().alert().accept();
  
  # Print status
  current_adres = ko_df$opvanglocatie_adres[idx]
  tprint(paste0('     Getting GPS coordinates for ', current_adres))
  
  # Get full address of daycare facility
  full_address = list(
    current_adres, " ",
    ko_df$opvanglocatie_postcode[idx], " ",
    ko_df$opvanglocatie_woonplaats[idx], "\uE007"
  )
  
  # Fill out address in textbox
  address_elem = remDr$findElement("address", using = "id")
  address_elem$clearElement()
  address_elem$sendKeysToElement(full_address)
  rm(address_elem)
  
  # Check if an alert is open and if so, accept it
  check_and_dismiss_alert(remDr)
  
  # Click button to get GPS coordinates
  gps_elem = remDr$findElement('//*[@id="wrap"]/div[2]/div[3]/div[1]/form[1]/div[2]/div/button', using = "xpath")
  gps_elem$clickElement()
  rm(gps_elem)
  
  # 
  out_alert = check_and_dismiss_alert(remDr)
  if (out_alert){
    
    #
    message('Skipping address because geolocation was not found.')
    latlong = NA
    remDr$dismissAlert()
    
  } else {
    
    # Obtain GPS coordinates of address
    latlong_elem = remDr$findElement("latlong", using = "id")
    latlong = latlong_elem$getElementAttribute("value")[[1]]
    latlong_elem$clearElement()
    rm(latlong_elem)
    
  }
  rm(out_alert)
  
  # Add coordinates to data frame
  ko_df$gps_coords[idx] = latlong
  
  #
  complete_address = do.call(paste, full_address[1:5])
  ko_df[idx, "opvanglocatie_adres_volledig"] = complete_address
  new_row = data.frame(opvanglocatie_adres_volledig = complete_address, gps_coords = latlong)
  if (exists("gps_df")){
    gps_df = rbind(gps_df, new_row)
  } else {
    gps_df = new_row
  }
  write.csv(gps_df, paste0(out_folder, "gps_coordinates_addresses.csv"), row.names=F)
  
  # Update the progress bar
  setTxtProgressBar(pb, counter)
  counter = counter + 1
  
  # Sleep for some seconds
  Sys.sleep(runif(1, 0, 5))
  
}
close(pb)

#------------------------------------------------------
# Clean and wrangle daycare facility data
#------------------------------------------------------

write.csv(ko_df, paste0(out_folder, "kinderopvang_data_", gsub("-", "", Sys.Date()), ".csv"))

# Stop Docker container
stopping = system("docker ps", intern = T)
cont_name = str_split(stopping[grepl(my_port, stopping)], "   ")[[1]][7]
system(paste0("docker stop ", cont_name))

