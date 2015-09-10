
# various functions for accessing the musixmatch API

options(stringsAsFactors = FALSE)

library(stringr)
library(XML)

apikey = ""

#artist = "Tool"
artist = "460"

#artist = "Taylor Swift"
#artist = "259675"

get_artist <- function(artist, apikey){
  
  call = paste("http://api.musixmatch.com/ws/1.1/artist.search?",
               "q_artist=",artist,
               "&apikey=",apikey,
               "&format=xml", sep = "")
  
  xml <- xmlParse(call)
  
  artist_ids <- xmlToDataFrame(nodes=getNodeSet(xml, "//artist/artist_id"))
  artist_names <- xmlToDataFrame(nodes=getNodeSet(xml, "//artist/artist_name"))
  
  return(data.frame(id = artist_ids$text, name = artist_names$text))
}

get_albums <- function(artist, apikey){
  
  call = paste("http://api.musixmatch.com/ws/1.1/artist.albums.get?",
               "artist_id=", artist,
               "&apikey=", apikey,
               "&page_size=100",
               "&format=xml", sep = "")
  
  xml <- xmlParse(call)
  
  albums <- xmlToDataFrame(nodes=getNodeSet(xml, "//album/album_id"))
  #albums <- xmlToDataFrame(nodes=getNodeSet(xml, "//album/album_name"))
  
  return(albums$text)
}

get_tracks <- function(album, apikey){
  
  call = paste("http://api.musixmatch.com/ws/1.1/album.tracks.get?",
               "album_id=", album,
               "&apikey=", apikey,
               "&page_size=100",
               "&f_has_lyrics=1",
               "&format=xml", sep = "")
  
  xml <- xmlParse(call)
  
  tracks <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/track_id"))
  
  return(tracks$text)
}
  
get_lyrics <- function(track, apikey){
  
  call = paste("http://api.musixmatch.com/ws/1.1/track.lyrics.get?",
               "track_id=", track,
               "&apikey=", apikey,
               "&format=xml", sep = "")
  
  xml <- xmlParse(call)
  
  lyrics <- tryCatch(xmlToDataFrame(nodes=getNodeSet(xml, "//lyrics_body")),
                     error = function(e) print("NA"))
  
  lyrics_clean <- lyrics %>% 
    as.character() %>% 
    gsub("This Lyrics is NOT for Commercial use","", .) %>% 
    gsub("\\n", " ", .)
  
  return(lyrics_clean)
}
  
get_corpus <- function(artist, apikey){
  
  #get albums for particular artist
  albums <- get_albums(artist, apikey)
  
  #loop through albums
  tracks <- lapply(albums, get_tracks, apikey) %>% unlist()
  
  #loop through tracks
  lyrics <- lapply(tracks, get_lyrics, apikey) %>% unlist()
  
  obj <- list(lyrics = data.frame(lyrics),
              n_albums = length(albums),
              n_tracks = length(tracks))
  
  return(obj)
}

collection <- get_corpus(artist, apikey)
