#Making the variable variable_names
#Source: these files
#https://datasets.iisg.amsterdam/dataset.xhtml?persistentId=hdl:10622/RPBVK4

#put the data in new dir, then proceed
#Packages
library(readxl)
library(janitor)
library(stringr)
library(tidyverse)
library(stringdist)

#Read files
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

#Use this to include data in package
usethis::use_data(lijst)
