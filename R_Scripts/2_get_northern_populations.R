#source("R_Scripts/1_data_import.R")
base_url<-'https://voterinformationservice.elections.on.ca/en/electoral-district/'
urls<-paste(base_url, northern$ED_ID, "-", northern$ENGLISH_NA, sep="")
urls
urls<-str_replace_all(urls, "--", "-")
urls<-str_replace_all(urls, " ", "-")
urls
urls %>% 
  map(., read_html_live)->pages
pages %>% 
map(., \(x) {
    x<-html_elements(x, ".ed-value")
    x<-html_text(x)
    x<-tail(x)[c(1,3)]
    x<-str_remove_all(x, ",")
    x<-as.numeric(x)
   # # x<-as.numeric()
   # x<-data.frame(x)
    }) %>%unlist() %>%  
  matrix(., ncol=2, byrow=T) %>% 
  data.frame() %>% set_names(c("ED_ID", "Population"))->northern_population
rm(pages)