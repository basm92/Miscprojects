##hdng_names_cats

# A function that helps the user explore the available variables
# Returns all categories on an empty call
# Returns all variables in a category on a call with category (typo robust)

# Allows the user to specify availability
# Needs data: variable_names

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
