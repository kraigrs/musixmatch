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


#### IN DEVELOPMENT ###




## DEV
# track.search
# track.get
# album.get
# artist.related.get
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


## TEST
# track.lyrics.get
# artist.get
# artist.search
# artist.albums.get
# album.tracks.get

## NEED


