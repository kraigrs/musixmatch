
get_matcher_lyrics <- function(simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),q_track=q_track,q_artist=q_artist,feedback,format='xml')
  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('q_track','q_artist')])

  request <- api_call('matcher.lyrics.get',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_chart_artist(content(request))
  else result <- get_full_list(content(request))

  result
}

get_matcher_track <- function(simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),q_track=q_track,q_artist=q_artist,q_album=q_album,feedback,format='xml')
  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('q_track','q_artist','q_album','f_has_lyrics','f_has_subtitle')])

  request <- api_call('matcher.track.get',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_chart_artist(content(request))
  else result <- get_full_list(content(request))

  result
}

get_matcher_subtitle <- function(q_track,q_artist,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),q_track=q_track,q_artist=q_artist,feedback,format='xml')
  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('q_track','q_artist','f_subtitle_length','f_subtitle_length_max_deviation')])

  request <- api_call('matcher.subtitle.get',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_chart_artist(content(request))
  else result <- get_full_list(content(request))

  result
}
