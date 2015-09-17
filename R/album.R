


#' Get a list of songs on the album
#'
#' @param album_id ID of album on musiXmatch
#' @return a data.frame or list containing the data from the API call
get_album_tracks <- function(album_id,page_size=100,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'), album_id=album_id,page_size=page_size, format='xml')
  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('album_mbid','f_has_lyrics','page')])

  request <- api_call('album.tracks.get',body)

  check_status_code(status_code(request))

  if( simplify )  result <- simplify_get_album_tracks(content(request))
  else result <- get_full_list(content(request))

  result
}


## IN DEV ##


get_album <- function(album_id,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'), album_id=album_id, format='xml')

  request <- api_call('album.get',body)

  check_status_code(status_code(request))

  if( simplify  && FALSE )  result <- simplify_get_album(content(request))
  else result <- get_full_list(content(request))

  result
}
