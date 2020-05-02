#From raw data to 'lijst' 
#Source: these files
#https://datasets.iisg.amsterdam/dataset.xhtml?persistentId=hdl:10622/RPBVK4

#put the data in new dir, then proceed

#Packages
library(readxl)
library(janitor)
library(stringr)
library(tidyverse)
library(stringdist)

# Data files
files <- list.files(pattern = "hdng[0-9]+.xls")
lijst <- NULL

for(i in 1:length(files)) {
    lijst[[i]] <- read_xls(files[i], 
                           skip = 1)
}

#Merge the data
lijst <- lijst %>%
  lapply(clean_names)

lijst <- lijst %>%
  lapply(pivot_longer, -c("cbsnr", "naam", "acode")) 

lijst <- lijst %>%
  purrr::reduce(rbind)

lijst <- lijst %>%
  separate(name, into = c("main.cat", "year", "var"), sep = c(1,4)) %>%
  mutate(year = as.numeric(paste(1, year, sep = "")))

lijst <- lijst %>%
  distinct()
  
lijst <- lijst %>% 
  pivot_wider(names_from = var, values_from = value)

#Use this to include data in package
usethis::use_data(lijst)
