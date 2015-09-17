
df.options <- c('data.frame','df','summary','easy')
list.options <- c('list','full')

#' Set API key for current session.
#' @param apikey API key
set_apikey <- function(apikey) options(mmapikey=apikey)

#' Check type parameter is correct
#' @param type type of data to be returned
check_type <- function(type){
  type <- tolower(type)
  if(! type %in% c(df.options,list.options) )
    stop(paste('Type needs to be one of the following: ',paste(df.options,collapse=', '),', ',paste(list.options,collapse=', '),sep='' ))
  return(type)
}

#' Check API call executed correctly
check_status_code <- function(code){
  if(code == 400) stop('The request had bad syntax or was inherently impossible to be satisfied.')
  if(code == 401) stop('Authentication failed, probably because of a bad API key.')
  if(code == 402) stop('A limit was reached, either you exceeded per hour requests limits or your balance is insufficient.')
  if(code == 403) stop('You are not authorized to perform this operation or the api version you\'re trying to use has been shut down.')
  if(code == 404) stop('Requested resource was not found.')
  if(code == 405) stop('Requested method was not found.')
  if(code != 200) stop("Unknown Status Code.")
}
