# Functie om de data te extracten

#Needs data: 
#lijst


hdng_get_data <- function(variables, 
                          gemeenten, 
                          from = 1000, 
                          to = 2000, 
                          clean = TRUE,
                          col.names = FALSE) {
  
  lijst %>%
    select(c(1:5), all_of(variables)) -> query 
  
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