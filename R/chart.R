
get_chart_artist <- function(country,page_size=100,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),country=country,page=page,page_size=page_size,format='xml')
  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('page')])

  request <- api_call('chart.artists.get',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_chart_artist(content(request))
  else result <- get_full_list(content(request))

  result
}

get_chart_tracks <- function(country,page=1,page_size=100,f_has_lyrics=1,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),country=country,page=page,page_size=page_size,f_has_lyrics=f_has_lyrics,format='xml')
  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('page','f_has_lyrics')])

  request <- api_call('chart.tracks.get',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_chart_artist(content(request))
  else result <- get_full_list(content(request))

  result
}
