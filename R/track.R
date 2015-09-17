
#' get lyrics from a song
#'
#' @param track_id musiXmatch ID of song
#' @return a data.frame or list containing the data from the API call
get_track_lyrics <- function(track_id,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'), track_id=track_id, format='xml')
  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('track_mbid')])

  request <- api_call('track.lyrics.get',body)

  check_status_code(status_code(request))

  if( simplify )  result <- simplify_get_lyrics(content(request))
  else result <- get_full_list(content(request))

  result
}


### IN DEV ###


search_track <- function(page_size=100,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),
               q=q,
               q_lyrics=q_lyrics,
               page=page,
               page_size=page_size,
               f_has_lyrics=f_has_lyrics,
               format='xml')
  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('q_track','q_artist','q_lyrics',
                                              'page','f_has_lyrics','f_artist_id',
                                              'f_msuci_genre_id','f_artist_mbid',
                                              'f_lyrics_language','s_track_rating',
                                              's_artist_rating','quorum_factor')])


  request <- api_call('track.search',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_search_track(content(request))
  else result <- get_full_list(content(request))

  result
}

get_track <- function(track_id,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),track_id=track_id,format='xml')
  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('track_mbid')])


  request <- api_call('track.get',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_track(content(request))
  else result <- get_full_list(content(request))

  result
}

get_track_subtitle <- function(track_id,subtitle_format='irc',simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),track_id=track_id,subtitle_format=subtitle_format,format='xml')
  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('track_mbid','subtitle_format','f_subtitle_length','f_subtitle_length_max_deviation')])

  request <- api_call('track.subtitle.get',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_chart_artist(content(request))
  else result <- get_full_list(content(request))

  result
}

get_track_snippet <- function(track_id,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),track_id=track_id,format='xml')

  request <- api_call('track.snippet.get',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_chart_artist(content(request))
  else result <- get_full_list(content(request))

  result
}

post_track_lyrics <- function(track_id,lyrics_body,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),track_id=track_id,lyrics_body=lyrics_body,format='xml')

  request <- api_call('track.lyrics.post',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_chart_artist(content(request))
  else result <- get_full_list(content(request))

  result
}

post_track_lyrics_feedback <- function(track_id,lyrics_id,feedback,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),track_id=track_id,lyrics_id=lyrics_id,feedback,format='xml')

  request <- api_call('track.lyrics.feedback.post',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_chart_artist(content(request))
  else result <- get_full_list(content(request))

  result
}
