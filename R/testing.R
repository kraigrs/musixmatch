# Test Methods


run_tests <- function(){
  q_artist <- 'slayer'
  artist_id <- 2683
  album_id <- 10392458
  track_id <- 13886643

  # search_artist
  tryCatch(expr=search_artist(q_artist),
           error=function(e) {
             print(e)
             return(1)
             })


  tryCatch(expr=search_artist(q_artist,page_size = 2),
           error=function(e) {
             print(e)
             return(1)
           })

  tryCatch(expr=search_artist(q_artist,page_size = 2,f_artist_id=artist_id),
           error=function(e) {
             print(e)
             return(1)
           })

  #
  # get_artist_albums
  #
  tryCatch(expr=get_artist_albums(artist_id),
           error=function(e) {
             print(e)
             return(1)
           })

  tryCatch(expr=get_artist_albums(artist_id,page_size=2),
           error=function(e) {
             print(e)
             return(1)
           })

  tryCatch(expr=get_artist_albums(artist_id,g_album_name=1),
           error=function(e) {
             print(e)
             return(1)
           })

  tryCatch(expr=get_artist_albums(artist_id,s_release_date='desc'),
           error=function(e) {
             print(e)
             return(1)
           })

  #
  # get_artist
  #
  tryCatch(expr=get_artist(artist_id),
           error=function(e) {
             print(e)
             return(1)
           })

  #
  # get_artist_related
  #
  tryCatch(expr=get_artist_related(artist_id),
           error=function(e) {
             print(e)
             return(1)
           })

   #
   # get_album
   #
   tryCatch(expr=get_album(album_id),
            error=function(e) {
              print(e)
              return(1)
            })

   #
   # get_album_tracks
   #
   tryCatch(expr=get_album_tracks(album_id),
            error=function(e) {
              print(e)
              return(1)
            })
   tryCatch(expr=get_album_tracks(album_id,f_has_lyrics=1,page=2),
            error=function(e) {
              print(e)
              return(1)
            })

 #
 # search_track
 #
 tryCatch(expr=search_track(q_track='Show No Mercy',q_artist='Slayer'),
          error=function(e) {
            print(e)
            return(1)
          })
 tryCatch(expr=search_track(q_track='Show No Mercy',f_has_lyrics=1),
          error=function(e) {
            print(e)
            return(1)
          })
 #
 # get_track
 #
 tryCatch(expr=get_track(track_id),
          error=function(e) {
            print(e)
            return(1)
          })

 #
 # get_track_subtitle - ERROR
 #
 tryCatch(expr=get_track_subtitle(track_id),
          error=function(e) {
            print(e)
            return(1)
          })

}
