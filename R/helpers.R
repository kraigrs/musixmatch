
df.options <- c('data.frame','df','summary','easy')
list.options <- c('list','full')

# Set API key for current session.
set_apikey <- function(apikey) options(mmapikey=apikey)

# Check type parameter is correct
check_type <- function(type){
  type <- tolower(type)
  if(! type %in% c(df.options,list.options) )
    stop(paste('Type needs to be one of the following: ',paste(df.options,collapse=', '),', ',paste(list.options,collapse=', '),sep='' ))
  return(type)
}
