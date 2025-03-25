library(here)
source(here("R_Scripts/1_data_import.R"))

#This code downloads the StatsCan Census data for Federal Electoral Districts
# under the 2013 Representation Order
# As of February 26 the URL was https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger/comp/GetFile.cfm?Lang=E&FILETYPE=CSV&GEONO=010
#Read StatsCan data
statscanurl<-"https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger/comp/GetFile.cfm?Lang=E&FILETYPE=CSV&GEONO=010"
# Set the path for the temporary file
temp_file_path <- tempfile(fileext = ".zip")
# Download the file from the URL and save it to the temporary file
download.file(statscanurl, destfile = temp_file_path, mode = "wb")
#Make a temporary directory called temp_dir
temp_dir <- tempdir()
#unzip what is downloaded in temp_file_path into temp_dir
unzip(temp_file_path, exdir = temp_dir)
#List files
list.files(temp_dir)
#Read in the statscan census counts
census<-read.csv(file.path(temp_dir, "98-401-X2021010_English_CSV_data.csv"))
#Filter only the ontario electoral ridings
census %>% 
  #Store in on_fed
filter(ALT_GEO_CODE>35000&ALT_GEO_CODE<35999)->on_fed

#Filter the row that has french as mother tongue
on_fed %>% 
  filter(CHARACTERISTIC_ID==397) %>% 
  select(GEO_NAME, C10_RATE_TOTAL)
