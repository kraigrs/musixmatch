library(XML)

# Search for artists in the database
# artist_id, artist_mbid not working yet -- need to find example
# Would like to add genre in -- hard to parse.
get_artist <- function(artist,type='data.frame',...){

  type <- check_type(type)

  call <- paste("http://api.musixmatch.com/ws/1.1/artist.search?",
               "q_artist=",artist,
               "&apikey=",getOption('mmapikey'),
               "&format=xml", sep = "")
  xml <- xmlParse(call)

  # !!! Is it inefficient here to pass XML
  # Better to pass http request?
  # Should pass by reference?
  # Maybe doesn't matter

  if( type %in% df.options)
    result <- get_artist_df(xml)
  if ( type %in% list.options)
    result <- get_artist_list(xml)
  result
}

# Returns XML parsed into easy to use data.frame
get_artist_df <- function(xml){

  artist_ids <- xmlToDataFrame(nodes=getNodeSet(xml, "//artist/artist_id"),stringsAsFactors = FALSE)$text
  artist_names <- xmlToDataFrame(nodes=getNodeSet(xml, "//artist/artist_name"),stringsAsFactors = FALSE)$text
  artist_country <- xmlToDataFrame(nodes=getNodeSet(xml, "//artist/artist_country"),stringsAsFactors = FALSE)$text

  return(data.frame(id = artist_ids,
                    name = artist_names,
                    country = artist_country,
                    stringsAsFactors = FALSE))
}

# This returns a list containing all of the XML fields
get_full_list<- function(xml) return(xmlToList(xml))

# Get album discography of an artist
get_albums <- function(artist_id,page_size=100,type='data.frame',...){
  type <- check_type(type)

  call = paste("http://api.musixmatch.com/ws/1.1/artist.albums.get?",
               "artist_id=", artist_id,
               "&apikey=", getOption('mmapikey'),
               "&page_size=",page_size,
               "&format=xml", sep = "")

  xml <- xmlParse(call)

  if( type %in% df.options)
    result <- get_albums_df(xml)
  if ( type %in% list.options)
    result <- get_full_list(xml)
  result
}

get_albums_df <- function(xml){
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

# get a list of songs on the album
get_tracks <- function(album_id,type='data.frame'){

  type <- check_type(type)

  call = paste("http://api.musixmatch.com/ws/1.1/album.tracks.get?",
               "album_id=", album_id,
               "&apikey=", getOption('mmapikey'),
               "&page_size=100",
               "&f_has_lyrics=1",
               "&format=xml", sep = "")

  xml <- xmlParse(call)

  if( type %in% df.options)
    result <- get_tracks_df(xml)
  if ( type %in% list.options)
    result <- get_full_list(xml)
  result
}
get_tracks_df <- function(xml){

  track_id <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/track_id"),stringsAsFactors = FALSE)$text
  track_mbid <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/track_mbid"),stringsAsFactors = FALSE)$text
  track_spotify_id <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/track_spotify_id"),stringsAsFactors = FALSE)$text
  track_soundcloud_id <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/track_soundcloud_id"),stringsAsFactors = FALSE)$text
  track_xboxmusic_id <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/track_xboxmusic_id"),stringsAsFactors = FALSE)$text
  track_name <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/track_name"),stringsAsFactors = FALSE)$text
  track_length <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/track_length"),stringsAsFactors = FALSE)$text
  instrumental <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/instrumental"),stringsAsFactors = FALSE)$text
  has_lyrics <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/has_lyrics"),stringsAsFactors = FALSE)$text
  album_id <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/album_id"),stringsAsFactors = FALSE)$text
  artist_id <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/artist_id"),stringsAsFactors = FALSE)$text
  track_share_url <- xmlToDataFrame(nodes=getNodeSet(xml, "//track/track_share_url"),stringsAsFactors = FALSE)$text

  return(data.frame(track_id,
                    track_mbid,
                    track_spotify_id,
                    track_soundcloud_id,
                    track_xboxmusic_id,
                    track_name,
                    track_length,
                    instrumental,
                    has_lyrics,
                    album_id,
                    artist_id,
                    track_share_url))
}

# get lyrics from a song
get_lyrics <- function(track_id,type='data.frame',...){

  type <- check_type(type)

  call = paste("http://api.musixmatch.com/ws/1.1/track.lyrics.get?",
               "track_id=", track_id,
               "&apikey=", getOption('mmapikey'),
               "&format=xml", sep = "")

  xml <- xmlParse(call)

  if( type %in% df.options)
    result <- get_tracks_df(xml)
  if ( type %in% list.options)
    result <- get_full_list(xml)
  result
}

get_lyrics_df <- function(xml){

  lyrics <- tryCatch(xmlToDataFrame(nodes=getNodeSet(xml, "//lyrics_body"),stringsAsFactors = FALSE),
                     error = function(e) print("NA"))

  lyrics_clean <- lyrics %>%
    as.character() %>%
    gsub("This Lyrics is NOT for Commercial use","", .) %>%
    gsub("\\n", " ", .)

  return(lyrics_clean)
}
