# musiXmatch API wrapper in R

## Usage

```{r}
library(musixmatch)
set_apikey(YOUR_APIKEY)
```

### Get Artist IDs (**artist.search**)

```{r}
# Return data.frame of most useful fields to identify artist
get_artist('slayer')

# Return list of full XML result in a list
get_artist(artist = 'slayer',type = 'list')
```

### Get album discography of artist (**artist.albums.get**)

```{r}
# Return data.frame of most useful fields to identify albums of an artist
get_albums(2683)
```

### Get all tracks from an album (**album.tracks.get**)

```{r}
# Return data.frame of most useful fields related to tracks on an album
get_tracks(10324491)
```

### Get Lyrics from a song (****)
```{r}
# Returns lyrics of a song
get_lyrics(13809615)
```


