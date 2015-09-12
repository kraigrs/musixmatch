

# Set API key for current session.
set_apikey <- function(apikey) options(mmapikey=apikey)

# Search for artists in the database
# artist_id, artist_mbid not working yet -- need to find example
# Would like to add genre in -- hard to parse.
get_artist <- function(artist,
                       artist_id=NA,
                       artist_mbid=NA
                       ){
  library(stringr)
  library(XML)

  call = paste("http://api.musixmatch.com/ws/1.1/artist.search?",
               "q_artist=",artist,
               "&apikey=",getOption('mmapikey'),
               "&format=xml", sep = "")

  xml <- xmlParse(call)

  artist_ids <- xmlToDataFrame(nodes=getNodeSet(xml, "//artist/artist_id"),stringsAsFactors = FALSE)
  artist_names <- xmlToDataFrame(nodes=getNodeSet(xml, "//artist/artist_name"),stringsAsFactors = FALSE)
  artist_country <- xmlToDataFrame(nodes=getNodeSet(xml, "//artist/artist_country"),stringsAsFactors = FALSE)

  return(data.frame(id = artist_ids$text,
                    name = artist_names$text,
                    country = artist_country$text,
                    stringsAsFactors = FALSE))
}

# Get album discography of an artist
get_albums <- function(artist_id,
                       page_size=100){
  library(stringr)
  library(XML)

  call = paste("http://api.musixmatch.com/ws/1.1/artist.albums.get?",
               "artist_id=", artist_id,
               "&apikey=", getOption('mmapikey'),
               "&page_size=",page_size,
               "&format=xml", sep = "")

  xml <- xmlParse(call)

  album_id <- xmlToDataFrame(nodes=getNodeSet(xml, "//album/album_id"),stringsAsFactors = FALSE)$text
  album_mbid <- xmlToDataFrame(nodes=getNodeSet(xml, "//album/album_mbid"),stringsAsFactors = FALSE)$text
  album_name <- xmlToDataFrame(nodes=getNodeSet(xml, "//album/album_name"),stringsAsFactors = FALSE)$text
  album_track_count <- xmlToDataFrame(nodes=getNodeSet(xml, "//album/album_track_count"),stringsAsFactors = FALSE)$text
  album_release_date <- xmlToDataFrame(nodes=getNodeSet(xml, "//album/album_release_date"),stringsAsFactors = FALSE)$text
  album_release_type <- xmlToDataFrame(nodes=getNodeSet(xml, "//album/album_release_type"),stringsAsFactors = FALSE)$text
  album_rating <- xmlToDataFrame(nodes=getNodeSet(xml, "//album/album_rating"),stringsAsFactors = FALSE)$text


  return(data.frame(album_id,
                    album_mbid,
                    album_name,
                    album_track_count,
                    album_release_date,
                    album_release_type,
                    album_rating))
}

get_tracks <- function(album){
  library(stringr)
  library(XML)
  options(stringsAsFactors = FALSE)

  call = paste("http://api.musixmatch.com/ws/1.1/album.tracks.get?",
               "album_id=", album,
               "&apikey=", getOption('mmapikey'),
               "&page_size=100",
               "&f_has_lyrics=1",
               "&format=xml", sep = "")

  xml <- xmlParse(call)

  tracks <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/track_id"),stringsAsFactors = FALSE)

  return(tracks$text)
}

get_lyrics <- function(track){
  library(stringr)
  library(XML)
  options(stringsAsFactors = FALSE)

  call = paste("http://api.musixmatch.com/ws/1.1/track.lyrics.get?",
               "track_id=", track,
               "&apikey=", getOption('mmapikey'),
               "&format=xml", sep = "")

  xml <- xmlParse(call)

  lyrics <- tryCatch(xmlToDataFrame(nodes=getNodeSet(xml, "//lyrics_body"),stringsAsFactors = FALSE),
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

# collection <- get_corpus(artist, apikey)
