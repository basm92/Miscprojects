#closest_matches
#Function to get the n (to be specified by the user) closest matches
#Unconditional or within a category

#User can indicate whether to give back the full names or the abbreviations
#Needs data: variable_names



closest_matches <- function(a, category = NULL, n = 10, variable.names = FALSE, ...){
  
  if(!missing(category)) {
    
    if(!is.element(category, variable_names$meaning)){
      stop('Please enter a valid category')
    }
    
    variable_names <- variable_names %>%
      filter(is.element(meaning, category))
  }
  
  matrix <- stringdistmatrix(a,variable_names$descr, ...)
  temp <- list()
  
  for(i in 1:nrow(matrix)) {
    as.data.frame(matrix) %>%
      .[i, order(matrix[i,], decreasing = F)] %>%
      colnames() %>%
      str_remove_all(., "V") %>%
      as.numeric() -> temp[[i]]
  }
  
  if(length(a) > 1) {
    temp <- temp %>% #More keywords
      purrr::reduce(rbind) %>%
      .[,1:n]
  }
  
  else {
    temp <- temp %>% #1 keyword ###DEBUG: Dit object heeft geen rijen, en die zijn nodig voor for loop
      purrr::reduce(rbind) %>%
      .[1:n] %>%
      matrix(., ncol = n)           #solution: matrix - causes other bugs.. fuck
  }
  
  if(variable.names == TRUE){
    
    for(j in 1:nrow(temp)){
      out[[j]] <- variable_names$descr[temp[j,]]
    }
    
  }
  
  else {
    
    for(j in 1:nrow(temp)){
      out[[j]] <- variable_names$var[temp[j,]]
    }
  }
  
  out <- out %>%
    purrr::reduce(cbind)
  
  if(length(a) > 1) {
    colnames(out) <- a
    out
  }
  
  else {
    out
  }
  
}
