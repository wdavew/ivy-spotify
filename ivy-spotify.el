;;; spotify.el --- Control Spotify through Ivy
;;; Commentary:

;;; Provides minimal set of functions to control spotify through an Ivy front
;;; end. Must set spotify-client-id and spotify-client-secret in your config.
;;; Create a personal app here first to connect with:
;;; https://developer.spotify.com/ Oauth2 will open a browser on the initial
;;; login. Provide your credentials and then take the code from the query
;;; parameters of the callback url it attempts to visit. This is a WIP
;;; currently, needs a few more features.
;;;
;;; Package-Requires: ((oauth2 "0.11") (swiper "20181212.1655") (json "1.4"))
;;; -*- lexical-binding: t; -*-

;;; Code:
(defconst spotify-auth-url "https://accounts.spotify.com/authorize")
(defconst spotify-token-url "https://accounts.spotify.com/api/token")
(defconst spotify-scopes "user-modify-playback-state user-read-playback-state")
(defconst spotify-search-url "https://api.spotify.com/v1/search")
(defconst spotify-play-url "https://api.spotify.com/v1/me/player/play")
(defconst spotify-devices-url "https://api.spotify.com/v1/me/player/devices")
(defconst spotify-transfer-playback-url "https://api.spotify.com/v1/me/player")
(defconst spotify-json-header '(("Content-Type" . "application/json")))
(defconst spotify-url-header '(("Content-Type" . "application/x-www-form-urlencoded")))
(defconst spotify-cl-header '(("Content-Length" . "0")))

(defgroup ivy-spotify nil
          "Ivy-spotify client")
(defcustom spotify-client-id nil
           "Spotify app id for oauth in ivy-spotify."
           :type 'string
           :group 'ivy-spotify)
(defcustom spotify-client-secret nil
           "Spotify app secret for oauth in ivy-spotify."
           :type 'string
           :group 'ivy-spotify)

(defun token ()
  "Retrieves personal access token using oauth2 flow."
    (oauth2-auth-and-store
                 spotify-auth-url
                 spotify-token-url
                 spotify-scopes
                 spotify-client-id
                 spotify-client-secret
                 "http://localhost:8080/callback"))

(defun make-req (url &optional method data headers)
  "Send request with DATA and HEADERS to the spotify API URL using METHOD and return JSON."
  (with-current-buffer (oauth2-url-retrieve-synchronously (token) url method data headers)
    (goto-char url-http-end-of-headers)
    (json-read)))

(defun spotify-search-tracks (str)
  "Execute spotify track search for STR."
  (let ((url (concat spotify-search-url "?type=track" "&" "q=" str)))
    (make-req url "GET")))

(defun spotify-search-playlists (str)
  "Execute spotify playlist search for STR."
  (let ((url (concat spotify-search-url "?type=playlist" "&" "q=" str)))
    (make-req url "GET")))

(defun spotify-search-albums (str)
  "Execute spotify playlist search for STR."
  (let ((url (concat spotify-search-url "?type=album" "&" "q=" str)))
    (make-req url "GET")))

(defun spotify-play-track (uri)
  "Play the given spotify track URI on currently active device."
  (let ((data (json-encode `(("uris" . ,(list uri))))))
    (make-req spotify-play-url "PUT" data spotify-json-header)))

(defun spotify-play-context (uri)
  "Play the given spotify context URI on currently active device."
  (let ((data (json-encode `(("context_uri" . ,uri)))))
    (make-req spotify-play-url "PUT" data spotify-json-header)))

(defun spotify-activate-device (id)
  "Activate device with ID."
  (let ((json-dev (json-encode `(("device_ids" . ,(list id))))))
    (make-req spotify-transfer-playback-url "PUT" json-dev spotify-json-header)))

(defun vec-head (vector)
  "Return the first element of VECTOR."
  (elt vector 0))

(defun format-spotify-track (track)
  "Generate a display name with uri property given raw TRACK data."
  (let ((name (cdr (assoc 'name track)))
        (artist
         (cdr
          (assoc 'name (vec-head (cdr (assoc 'artists track))))))
        (album (cdr (assoc 'name (assoc 'album track)))))
    (propertize (format "%s - %s - %s" name artist album) 'uri (cdr (assoc 'uri track)))))

(defun format-spotify-collection (collection)
  "Generate a display name with uri property given raw COLLECTION data."
  (let ((name (cdr (assoc 'name collection))))
    (propertize name 'uri (cdr (assoc 'uri collection)))))

(defun format-spotify-device (device)
  "Generate a display name with id property given raw DEVICE data."
  (let ((is_active (cdr (assoc 'is_active device)))
        (name (cdr (assoc 'name device))))
    (propertize (format "%s%s" name (if (eq is_active :json-false) "" " (active)")) 'id (cdr (assoc 'id device)))))

(defun spotify-track-list (str)
  "Retrieve a formatted track list after searching Spotify for STR."
  (when (< 3 (length str))
  (let ((tracks (spotify-search-tracks str)))
         (mapcar 'format-spotify-track (cdr (assoc 'items (assoc 'tracks tracks)))))))

(defun spotify-album-list (str)
  "Retrieve a formatted playlist list after searching Spotify for STR."
  (when (< 3 (length str))
  (let ((playlists (spotify-search-albums str)))
         (mapcar 'format-spotify-collection (cdr (assoc 'items (assoc 'albums playlists)))))))

(defun spotify-playlist-list (str)
  "Retrieve a formatted playlist list after searching Spotify for STR."
  (when (< 3 (length str))
  (let ((playlists (spotify-search-playlists str)))
         (mapcar 'format-spotify-collection (cdr (assoc 'items (assoc 'playlists playlists)))))))

(defun spotify-device-list ()
  "Retrieve a formatted device list after searching Spotify for STR."
  (let ((dev (make-req spotify-devices-url)))
    (mapcar 'format-spotify-device (cdar dev))))

(defun ivy-spotify-track-search ()
  "Prompt for input and display track search results in Ivy."
  (interactive)
  (let ((ivy-dynamic-exhibit-delay-ms 250))
      (ivy-read "Select a track:"
                #'spotify-track-list
                :dynamic-collection t
                :action (lambda (x) (let ((uri (get-text-property 0 'uri x)))
                          (spotify-play-track uri))))))

(defun ivy-spotify-album-search ()
  "Prompt for input and display album search results in Ivy."
  (interactive)
  (let ((ivy-dynamic-exhibit-delay-ms 250))
      (ivy-read "Select an album:"
                #'spotify-album-list
                :dynamic-collection t
                :action (lambda (x) (let ((uri (get-text-property 0 'uri x)))
                          (spotify-play-context uri))))))

(defun ivy-spotify-playlist-search ()
  "Prompt for input and display playlist search results in Ivy."
  (interactive)
  (let ((ivy-dynamic-exhibit-delay-ms 250))
      (ivy-read "Select a playlist:"
                #'spotify-playlist-list
                :dynamic-collection t
                :action (lambda (x) (let ((uri (get-text-property 0 'uri x)))
                          (spotify-play-context uri))))))

(defun ivy-spotify-device-search ()
  "Prompt for input and display device search results in Ivy."
  (interactive)
  (let ((ivy-dynamic-exhibit-delay-ms 250))
  (ivy-read "Select a device:"
            (spotify-device-list)
            :action (lambda (x) (let ((id (get-text-property 0 'id x)))
                                  (spotify-activate-device id))))))

(defun player-action (action)
  "Send ACTION to /player endpoint with correct method and headers."
  (let ((method (if
                  (or
                   (string= action "next")
                   (string= action "previous"))
                  "POST"
                  "PUT")))
  (make-req (format "https://api.spotify.com/v1/me/player/%s" action) method nil spotify-cl-header)))

(defun spotify-play ()
  "Play the currently active player."
  (interactive)
  (player-action "play"))
(defun spotify-pause ()
  "Pause the currently active player."
  (interactive)
  (player-action "pause"))
(defun spotify-next ()
  "Go to next track."
  (interactive)
  (player-action "next"))
(defun spotify-prev ()
  "Go to previous track."
  (interactive)
  (player-action "previous"))
(defun spotify-shuffle-on ()
  "Turn shuffle on in active spotify player."
  (interactive)
  (player-action "shuffle?state=true"))
(defun spotify-shuffle-off ()
  "Turn shuffle off in active spotify player."
  (interactive)
  (player-action "shuffle?state=false"))

(provide 'spotify.el)
;;; ivy-spotify.el ends here
