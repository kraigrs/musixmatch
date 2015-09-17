

#' This returns a list containing all of the XML fields
#'
#' @param xml an XML document with nodes artist_id, artist_name and artist_country
#' @return list of full XML document
get_full_list<- function(xml) return(xmlToList(xml))


#' Returns XML parsed into easy to use data.frame
#'
#' @param xml an XML document with nodes artist_id, artist_name and artist_country
#'
get_artist_df <- function(xml){

  artist_ids <- xmlToDataFrame(nodes=getNodeSet(xml, "//artist/artist_id"),stringsAsFactors = FALSE)$text
  artist_names <- xmlToDataFrame(nodes=getNodeSet(xml, "//artist/artist_name"),stringsAsFactors = FALSE)$text
  artist_country <- xmlToDataFrame(nodes=getNodeSet(xml, "//artist/artist_country"),stringsAsFactors = FALSE)$text

  return(data.frame(id = artist_ids,
                    name = artist_names,
                    country = artist_country,
                    stringsAsFactors = FALSE))
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

