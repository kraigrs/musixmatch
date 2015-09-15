suppressPackageStartupMessages(library(XML))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(httr))

base_url <- 'http://api.musixmatch.com/ws/1.1/'




#' Make general musiXmatch API call
#'
#' @param method API function to call.
#' Options include artist.search, artist.albums.get, album.tracks.get.
#' More information can be found at https://developer.musixmatch.com/documentation/api-methods
api_call <- function(method,body)
  GET(url=paste(base_url,method,'?',sep=''),query=body,encode='json')



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
search_artist <- function(artist,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'), q_artist=artist, format='xml')

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

  request <- api_call('artist.albums.get',body)

  check_status_code(status_code(request))

  if( simplify )  result <- simplify_get_artist_albums(content(request))
  else result <- get_full_list(content(request))

  result
}

#' Get a list of songs on the album
#'
#' @param album_id ID of album on musiXmatch
#' @return a data.frame or list containing the data from the API call
get_album_tracks <- function(album_id,page_size=100,f_has_lyrics=1,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'), album_id=album_id,page_size=page_size,f_has_lyrics=f_has_lyrics, format='xml')

  request <- api_call('album.tracks.get',body)

  check_status_code(status_code(request))

  if( simplify )  result <- simplify_get_album_tracks(content(request))
  else result <- get_full_list(content(request))

  result
}

#' get lyrics from a song
#'
#' @param track_id musiXmatch ID of song
#' @return a data.frame or list containing the data from the API call
get_lyrics <- function(track_id,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'), track_id=track_id, format='xml')

  request <- api_call('track.lyrics.get',body)

  check_status_code(status_code(request))

  if( simplify )  result <- simplify_get_lyrics(content(request))
  else result <- get_full_list(content(request))

  result
}


#### IN DEVELOPMENT ###

get_artist <- function(artist_id,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'), artist_id=artist_id, format='xml')

  request <- api_call('artist.get',body)

  check_status_code(status_code(request))

  result <- content(request)
#   if( simplify )  result <- simplify_get_artist(content(request))
#   else result <- get_full_list(content(request))

  result
}


get_artist_related <- function(artist_id,page=1,page_size=10,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),
               artist_id=artist_id,
               page=1,
               page_size=page_size,
               format='xml')

  request <- api_call('artist.related..get',body)

  check_status_code(status_code(request))

  result <- content(request)
  #   if( simplify )  result <- simplify_get_artist(content(request))
  #   else result <- get_full_list(content(request))

  result
}


get_album <- function(album_id,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'), album_id=album_id, format='xml')

  request <- api_call('album.get',body)

  check_status_code(status_code(request))

  result <- content(request)
  #   if( simplify )  result <- simplify_get_artist(content(request))
  #   else result <- get_full_list(content(request))

  result
}

search_track <- function(q,q_lyrics,page=1,page_size=100,f_has_lyrics=1,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),
               q=q,
               q_lyrics=q_lyrics,
               page=page,
               page_size=page_size,
               f_has_lyrics=f_has_lyrics,
               format='xml')

  request <- api_call('track.search',body)

  check_status_code(status_code(request))

  result <- content(request)
  #   if( simplify )  result <- simplify_get_artist(content(request))
  #   else result <- get_full_list(content(request))

  result
}

get_track <- function(track_id,simplify=TRUE,...){

  body <- list(apikey=getOption('mmapikey'),track_id=track_id,format='xml')

  request <- api_call('track.get',body)

  check_status_code(status_code(request))

  result <- content(request)
  #   if( simplify )  result <- simplify_get_artist(content(request))
  #   else result <- get_full_list(content(request))

  result
}

## DEV
# track.search
# track.get

## TEST
# track.lyrics.get
# artist.get
# artist.search
# artist.albums.get
# artist.related.get
# album.get
# album.tracks.get

## NEED
# chart.artists.get
# chart.tracks.get
# track.subtitle.get
# track.snippet.get
# track.lyrics.post
# track.lyrics.feedback.post
# matcher.lyrics.get
# matcher.track.get
# matcher.subtitle.get
# tracking.url.get
# catalogue.dump.get


