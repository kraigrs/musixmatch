
get_corpus <- function(artist){

  #get albums for particular artist
  albums <- get_albums(artist)

  #loop through albums
  tracks <- lapply(albums, get_tracks) %>% unlist()

  #loop through tracks
  lyrics <- lapply(tracks, get_lyrics) %>% unlist()

  obj <- list(lyrics = data.frame(lyrics),
              n_albums = length(albums),
              n_tracks = length(tracks))

  return(obj)
}
