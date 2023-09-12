# Clear the environment and console
cat("\014")
rm(list = ls())

# Load libraries and functions
main_dir = dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(main_dir, 'input_and_functions.R'))

# Define output folder for scraped data
out_folder = file.path(main_dir, "scraped_data/opkoopbescherming/")
if (!dir.exists(out_folder)){ dir.create(out_folder, recursive = T) }

# Open the remote browser
my_port = as.character(netstat::free_port())
system(paste0("docker run -d -p ", my_port, ":4444 --shm-size 4g selenium/standalone-chrome:4.2.2"))
Sys.sleep(10)
main_page_url = "https://www.blisshypotheekadviseurs.nl/opkoopbescherming/#overzichtgemeenten"
remDr = remoteDriver$new(
  browserName = "chrome",
  port = as.integer(my_port),
  extraCapabilities = list(
    "chromeOptions" = list(args = list('--headless'))
  )
)
remDr$open(silent = T)
remDr$navigate(main_page_url)
date_elem = remDr$findElement('//*[@id="post-17477"]/div/div[2]/p[17]', using = 'xpath')
date_table = gsub("Laatst bijgewerkt: ", "", date_elem$getElementText()[[1]])
remDr$close()

# Check if data has been updated since last collection
data_file = paste0(out_folder, "opkoopbescherming_gemeente_", gsub("-", "", date_table), ".csv")
if (!file.exists(data_file)){
  # Collect table of opkoopbescherming per gemeente
  okb_df = main_page_url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="tablepress-22"]') %>%
    html_table()
  okb_df = okb_df[[1]]
  write.csv(okb_df, data_file)
}

# Stop Docker container
stopping = system("docker ps", intern = T)
cont_name = str_split(stopping[grepl(my_port, stopping)], "   ")[[1]][7]
system(paste0("docker stop ", cont_name))

