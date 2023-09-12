# Clear the environment and console
cat("\014")
rm(list = ls())

# Load libraries and functions
main_dir = dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(main_dir, 'input_and_functions.R'))

# Define paths on website
xpaths = list(
  adres = '//*[@id="top"]/main/header/p/span[1]',
  postcode_woonplaats = '//*[@id="top"]/main/header/p/span[2]',
  eigenschap1 = '//*[@id="top"]/main/header/ul[1]/li[1]/span',
  eigenschap2 = '//*[@id="top"]/main/header/ul[1]/li[2]/span',
  eigenschap3 = '//*[@id="top"]/main/header/ul[1]/li[3]/span',
  eigenschap4 = '//*[@id="top"]/main/header/ul[1]/li[4]/span'
)

# Define output folder for scraped data
out_folder = file.path(main_dir, "scraped_data/basisscholen/")
if (!dir.exists(out_folder)){ dir.create(out_folder, recursive = T) }

# List of zip codes
temp = tempfile()
download.file('https://download.cbs.nl/postcode/2023-cbs_pc6_2022_v1.zip', temp)
zip_codes_nl = read_xlsx(unzip(temp, "pc6_2022_v1.xlsx"), range = cell_cols("A"))
unlink(temp)
zip_codes_nl = data.frame(zip_codes_nl)[, 1]
zip_codes_nl = zip_codes_nl[(which(zip_codes_nl == "Code") + 1):length(zip_codes_nl)]
zip_codes_nl = unique(substr(zip_codes_nl, 1, 4))

# Open the remote browser
my_port = as.character(netstat::free_port())
system(paste0("docker run -d -p ", my_port, ":4444 --shm-size 4g selenium/standalone-chrome:4.2.2"))
Sys.sleep(10)
main_page_url = "https://scholenopdekaart.nl/"
remDr = remoteDriver$new(
  browserName = "chrome",
  port = as.integer(my_port),
  extraCapabilities = list(
    "chromeOptions" = list(args = list('--headless'))
  )
)
remDr$open(silent = T)
remDr$navigate(main_page_url)

# Accept cookies
cookie_elem = tryCatch({
  remDr$findElement(using = "id", value = "CybotCookiebotDialogBodyLevelButtonLevelOptinAllowAll")
}, error = function(e){ NULL })
if (!is.null(cookie_elem)){ cookie_elem$clickElement() }

# Target number of schools
nr_elem = remDr$findElement(using = "xpath", value = '//*[@id="top"]/main/home-zoeken-component/nav/h2')
nr_results = tryCatch({
  as.numeric(gsub("\\.", "", parse_number(nr_elem$getElementText()[[1]])))
}, error = function(e){ NULL })

# Load existing data
start_scrape_date = Sys.Date()
school_file = paste0(out_folder, "school_data_", gsub("-", "", start_scrape_date), ".csv")
if (file.exists(school_file)){ 
  school_df = read.csv(school_file)
  school_df = school_df[, names(school_df) != "X"]
} else {
  school_nams = c(names(xpaths), "school_name", "index", "scrape_date")
  school_df = data.frame(matrix(NA, nrow = 0, ncol = length(school_nams)))
  names(school_df) = school_nams
}

# 
count_idx = 1
pb = txtProgressBar(min = 0, max = length(zip_codes_nl), style = 3, width = 50, char = "=")  
for (pc_idx in seq_along(zip_codes_nl)){
  
  # Find the search bar and insert the postal code to query
  search_elem = remDr$findElement(using = "id", value = "search-query")
  if (!is.null(search_elem)){
    search_elem$clearElement()
    search_elem$sendKeysToElement(list(zip_codes_nl[pc_idx], "\uE007"))
  }
  
  # Obtain URL and decrease radius
  url_start = remDr$getCurrentUrl()[[1]]
  url_end = gsub("straal=2.5", "straal=1", url_start)
  remDr$navigate(url_end)
  
  # Keep clicking expand button until no longer possible such that all schools are visible
  more_xpath = '//*[@id="top"]/main/search-result-page-component/search-result-list-component/p/button'
  more_elem = tryCatch({
    remDr$findElement(using = "xpath", value = more_xpath)
  }, error = function(e){ NULL })
  while (!is.null(more_elem)){
    more_elem$clickElement()
    more_elem = tryCatch({
      remDr$findElement(using = "xpath", value = more_xpath)
    }, error = function(e){ NULL })
  }
  
  # 
  idx = 1
  detail_elem = tryCatch({
    detail_xpath = paste0('//*[@id="top"]/main/search-result-page-component/search-result-list-component/ol/li[', 
                          idx, ']/search-result-item-component/article/ul[2]/li[1]/a')
    remDr$findElement(using = "xpath", value = detail_xpath)
  }, error = function(e){ NULL })
  
  # 
  while (!is.null(detail_elem)){
    
    # Find the name of the school and check presence in existing data
    school_xpath = paste0('//*[@id="top"]/main/search-result-page-component/search-result-list-component/ol/li[', 
                          idx, ']/search-result-item-component/article/h3')
    school_elem = remDr$findElement(using = "xpath", value = school_xpath)
    school_name = school_elem$getElementText()[[1]]
    
    # 
    if (!school_name %in% school_df$school_name){
      
      # Print message
      tprint(paste0('   Scraping school data: ', school_name))
      
      # Find the URL of the particular school
      school_url = detail_elem$getElementAttribute('href')[[1]]
      school_url = paste0(school_url, 'resultaten/')
      
      # Open new window and switch to it to scrape school data
      remDr$executeScript("window.open('');")
      windows_handles = remDr$getWindowHandles()
      # myswitch(remDr, windows_handles[[2]])
      remDr$switchToWindow(windows_handles[[2]])
      remDr$navigate(school_url)
      
      # Scrape data for specific school
      school_row = scrape_school_data(remDr, xpaths)
      school_row["school_name"] = school_name
      school_row["index"] = count_idx
      school_row["scrape_date"] = Sys.time()
      
      # Return to list of results for postcode
      remDr$closeWindow()
      # myswitch(remDr, windows_handles[[1]])
      remDr$switchToWindow(windows_handles[[1]])
      
      # Add data to scraping data and save to file
      school_df = rbind(school_df, school_row)
      write.csv(school_df, school_file)
      
      #
      count_idx = count_idx + 1
      
    } else {
      # Print message
      tprint(paste0('   Skipping school data: ', school_name, ' -- already scraped.'))
    }
    
    # Add to counter
    idx = idx + 1
    
    # Find the next school in the list of results
    detail_elem = tryCatch({
      detail_xpath = paste0('//*[@id="top"]/main/search-result-page-component/search-result-list-component/ol/li[', 
                            idx, ']/search-result-item-component/article/ul[2]/li[1]/a')
      remDr$findElement(using = "xpath", value = detail_xpath)
    }, error = function(e){ NULL })
    
  }
  
  setTxtProgressBar(pb, pc_idx)
  
}
close(pb)
remDr$close()

cat(paste0('Scraping success rate of : ', round(count_idx / nr_results * 100), '%'))

# Stop Docker container
stopping = system("docker ps", intern = T)
cont_name = str_split(stopping[grepl(my_port, stopping)], "   ")[[1]][7]
system(paste0("docker stop ", cont_name))
