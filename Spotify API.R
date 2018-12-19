rm(list=ls())
library(spotifyr)

Sys.setenv(SPOTIFY_CLIENT_ID = '9d362b6aba3e4218aa4e49e9292ba691') #le mie credenziali per usare spotify API
Sys.setenv(SPOTIFY_CLIENT_SECRET = '4a76a80629f74744b1bb8e9098fc79f2')
access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))

playlists <- get_user_playlists(1187363546) # Scarico le playlist del mio account spotify
playlist_tracks <- get_playlist_tracks(playlists[1,]) # Scarico la lista delle canzoni che mi interessano
audio_features <- get_track_audio_features(playlist_tracks) # Questi sono i dati che mi interessano
names(audio_features)
info <- playlist_tracks[,c(3,5,6)]
audio_features <- cbind(info, audio_features)
audio_features$Class[1:500,] <- 1 #se piace a me
audio_features$Class[501:1000,] <- 0 #se piace a flo
write.table(file="audio_features.txt", audio_features, row.names=F, col.names=T)
