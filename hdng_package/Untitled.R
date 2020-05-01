#Packages
library(readxl)
library(janitor)
library(stringr)
library(tidyverse)
library(stringdist)

#WD
setwd("./Downloads/dataverse_files")

#Read files
## Variable names
variable_names <- read_xls("hdng variabelen.xls", col_names = F) %>%
    remove_empty("cols")

colnames(variable_names) <- c("var", 
                              "descr", 
                              "dataset", 
                              "year", 
                              "category")

variable_names <- variable_names %>%
    separate(var, into = c("main.cat","year","var"), sep = c(1,4)) %>%
    mutate(year = paste("1", year, sep = ""))

categories <- data.frame(indicator = letters[1:11], 
                         meaning = c( 
                           "Beroepen",
                           "Bedrijvigheid",
                           "Godsdienst",
                           "District",
                           "Bevolking",
                           "Politiek",
                           "Onderwijs",
                           "Welvaart",
                           "Voorzieningen",
                           "Woningen",
                           "Openheid")
) %>%
  mutate_all(as.character)

merge(variable_names, categories, 
      by.x = "main.cat", 
      by.y = "indicator") %>%
  select(1:4, 7) -> variable_names

## Rearrange variable_names according to data availability
variable_names <- variable_names %>%
  pivot_wider(names_from = year,  
              values_from = year,
              values_fn = list(year = length),
              values_fill = list(year = 0)) 

variable_names <- variable_names %>%
  select(names(variable_names) %>%
           sort())

variable_names <- variable_names %>%
  select(117:120, 1:116)

# Data files
files <- list.files(pattern = "*.xls")[-1]
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

#Functie om achter de naam van variabelen te komen per categorie
#Werkt op basis van de bestaande data in variable_names

hdng_names_cats <- function(cat, from = 1000, to = 2000, show.only.available = TRUE) {
    
    if(missing(cat)) { #Output for empty function call
        
        unique(variable_names$meaning)
    }
    
    else {
      
      match <- categories[amatch(cat, 
                                     unique(
                                       variable_names$meaning #Typo proof 
                                       ), 
                                     maxDist = 5),2]
      
      isthere <- function(x) grepl("1", x)
      
      variable_names %>%
        filter(meaning == match) %>%
        select(c(1:4, num_range(prefix = NULL, from:to))) -> allvars
        # Basic dataset, only variables of category, and filter from and to
      
      plyr::colwise(isthere)(allvars) %>% #Compute whether the data is available at
        plyr::colwise(sum)(.) %>%         #Least once in the time period
        select_if(. > 0) %>% 
        colnames() -> vars
      
      avail <- allvars %>%          #Filter the data to at least once available vars
        select(c(1:4), all_of(vars)) %>%
        .[rowSums(.[,-c(1:4)]) > 0,]
      
        if(show.only.available == FALSE) {
        allvars
        }
      
        else {
          avail
        }
    }
}



# Functie om de data te extracten

hdng_get_data <- function(variables, 
                          gemeenten, 
                          from = 1000, 
                          to = 2000, 
                          clean = TRUE,
                          col.names = FALSE) {
    
    lijst %>%
      select(c(1:5), all_of(variables)) -> query #Here I should get the labels already
  
  if(missing(gemeenten)){                     #Two trajectories: one without gemeenten
    query %>%
      filter(year >= from, year <= to) -> query   #Filter implements from, to and clean args
    
    if(clean == TRUE) {
      query %>%
        filter_at(vars(-c(1:5)), any_vars(!is.na(.))) -> query
    }
    
    else{
      query
    }
  } 
  
  else {                                    #Two trajectories: one with gemeenten
    query %>%                     #Filter implements from, to, clean and gemeenten args
      filter(year >= from, year <= to, is.element(naam,toupper(gemeenten))) -> query
    
    if(clean == TRUE) {
      query %>%
        filter_at(vars(-c(1:5)), any_vars(!is.na(.))) -> query
    }
    
    else{
      query
    }
  }
  
  if(col.names == TRUE) {
  colnames(query)[-c(1:5)] <- variable_names[match(colnames(query), 
                                                   variable_names$var),1]$descr[-c(1:5)]
  
  query
  }
  
  query
  
}


#Things to do:
# Get functie met behulp van textmatches 
#hdng_get_data_search <- 
  
