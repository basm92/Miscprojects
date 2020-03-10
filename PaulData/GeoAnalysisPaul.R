#Set the working directory appropriately
setwd("/Data")

#Load libraries
library(readxl)
library(tidyverse)

politicians <- read_xlsx("Data/tk_1815tot1950uu.xlsx", sheet = 1)
politicians2 <- read_xlsx("Data/tk_1815tot1950uu.xlsx", sheet = 2)

birthplace <- politicians2 %>%
    filter(rubriek == "3010") %>%
    select(1,3)

deathplace <- politicians2 %>%
    filter(rubriek == "3020") %>%
    select(1,3)

matches1 <- match(politicians$`b1-nummer`, birthplace$`b1-nummer`)
matches2 <- match(politicians$`b1-nummer`, deathplace$`b1-nummer`)

politicians <- politicians %>%
    mutate(birthplace = birthplace$waarde[matches1],
           deathplace = deathplace$waarde[matches2])

