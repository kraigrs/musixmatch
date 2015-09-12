# musiXmatch API wrapper in R

## Installation

```
devtools::install_github('kraigrs/musixmatch')
devtools::install_github('rweyant/musixmatch')
```

## Usage

```
library(musixmatch)
set_apikey(YOUR_APIKEY)
```

### Get Artist IDs (**artist.search**)

```
# Return data.frame of most useful fields to identify artist
get_artist('slayer')

# Return list of full XML result in a list
get_artist(artist = 'slayer',type = 'list')
```

### Get album discography of artist (**artist.albums.get**)

```
# Return data.frame of most useful fields to identify albums of an artist
get_albums(2683)
```

### Get all tracks from an album (**album.tracks.get**)

```
# Return data.frame of most useful fields related to tracks on an album
get_tracks(10324491)
```
