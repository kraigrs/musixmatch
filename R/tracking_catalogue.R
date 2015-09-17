get_tracking_url <- function(domain,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),domain=domain,feedback,format='xml')

  request <- api_call('tracking.url.get',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_chart_artist(content(request))
  else result <- get_full_list(content(request))

  result
}

get_catalogue_dump <- function(simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),feedback,format='xml')

  request <- api_call('catalogue.dump.get',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_chart_artist(content(request))
  else result <- get_full_list(content(request))

  result
}
