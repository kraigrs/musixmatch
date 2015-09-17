


#' Search for artists in the database
#'
#' @param artist string of the artist to search for, e.g. "Slayer"
#' @param type type of object to return.
#' @param ... other API parameters, e.g. artist_id, artist_mbid.
#' 'data,frane' returns a data.frame of common elements of interest.
#' 'list' will return the full XML results as a list
#' @return a data.frame or list containing the data from the API call
#' @examples
#' get_artist('slayer')
#' get_artist('slayer',type='list')
search_artist <- function(q_artist,page_size=100,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'), q_artist=q_artist, page_size=page_size,format='xml')

  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('f_artist_id','f_artist_mbid','page')])

  request <- api_call('artist.search',body)

  check_status_code(status_code(request))

  if( simplify )  result <- simplify_search_artist(content(request))
  else result <- get_full_list(content(request))

  result
}

#' Get album discography of an artist
#'
#' @param artist_id MusiXmatcht artist id
#' @param number of pages in XML document
#' @param return type
#' @return a data.frame or list containing the data from the API call
get_artist_albums <- function(artist_id,page_size=100,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'), artist_id=artist_id,page_size=page_size, format='xml')

  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('artist_mbid','g_album_name','s_release_date','page')])

  request <- api_call('artist.albums.get',body)

  check_status_code(status_code(request))

  if( simplify )  result <- simplify_get_artist_albums(content(request))
  else result <- get_full_list(content(request))

  result
}



## IN DEV ##

get_artist <- function(artist_id,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'), artist_id=artist_id, format='xml')
  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('artist_mbid')])

  request <- api_call('artist.get',body)

  check_status_code(status_code(request))

  if( simplify && FALSE )  result <- simplify_get_artist(content(request))
  else result <- get_full_list(content(request))

  result
}


get_artist_related <- function(artist_id,page=1,page_size=10,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),
               artist_id=artist_id,
               page=1,
               page_size=page_size,
               format='xml')
  # Load optional arguments from dots
  dots <- list(...)
  body <- append(body,dots[names(dots) %in% c('artist_mbid','page')])

  request <- api_call('artist.related..get',body)

  check_status_code(status_code(request))

  if( simplify && FALSE)  result <- simplify_get_artist_related(content(request))
  else result <- get_full_list(content(request))

  result
}
