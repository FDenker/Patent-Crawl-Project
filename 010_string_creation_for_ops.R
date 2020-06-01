#### cleanup and stringcreation for ops####



create_ops_strings <- function(){
  uspat_foreigncitiations$int_pat_number_for_query <- gsub(" ","",uspat_foreigncitiations$number)
  uspat_foreigncitiations$int_pat_number_for_query  <- paste0('DE', uspat_foreigncitiations$int_pat_number_for_query)
  uspat_foreigncitiations$valid_pat_id<- grepl("DE[0-9]{5,12}[A-Z]?[0-9]?$",uspat_foreigncitiations$int_pat_number_for_query)
  valid_uspat_fcit <- subset(uspat_foreigncitiations,valid_pat_id==TRUE)
  ##writing all the patent ids into strings for depatisnet crawler###
  string_for_lookup_pat_id <- c("")
  for(i in 1:ceiling(nrow(valid_uspat_fcit)/80)){
    if(i!=ceiling(nrow(valid_uspat_fcit)/80)){
      string_for_lookup_pat_id[i] <- toString(valid_uspat_fcit$int_pat_number_for_query[((i-1)*80):(i*80)] ) 
    }else{
      string_for_lookup_pat_id[i] <- toString(valid_uspat_fcit$int_pat_number_for_query[((i-1)*80):nrow(valid_uspat_fcit)])
    }
    
  }
  return(string_for_lookup_pat_id)
}

ops_string_for_query <- create_ops_strings()


