(in-package #:ems)

(defun get-contributors (tweet)
  (rest (assoc ':contributors tweet)))

(defun get-geo (tweet)
  (rest (assoc ':geo tweet)))

(defun get-coordinates (tweet)
  (rest (assoc ':coordinates tweet)))

(defun get-created-at (tweet)
  (rest (assoc ':created--at tweet)))

(defun get-current-user-retweet (tweet)
  (rest (assoc ':current--user--retweet tweet)))

(defun get-id (tweet)
  (rest (assoc ':id tweet)))

(defun get-id-str (tweet)
  (rest (assoc ':id--str tweet)))

(defun get-in-reply-to-screen-name (tweet)
  (rest (assoc ':in--reply--to--screen--name tweet)))

(defun get-in-reply-to-status-id (tweet)
  (rest (assoc ':in--reply--to--status--id tweet)))

(defun get-in-reply-to-status-id-str (tweet)
  (rest (assoc ':in--reply--to--status--id--str tweet)))

(defun get-in-reply-to-user-id (tweet)
  (rest (assoc ':in--reply--to--user--id tweet)))

(defun get-in-reply-to-user-id-str (tweet)
  (rest (assoc ':in--reply--to--user--id--str tweet)))

(defun get-entities (tweet)
  (rest (assoc ':entities tweet)))

(defun get-text (tweet)
  (rest (assoc ':text tweet)))

(defun get-favorited (tweet)
  (rest (assoc ':favorited tweet)))

(defun get-place (tweet)
  (rest (assoc ':place tweet)))

(defun get-possibly-sensitive (tweet)
  (rest (assoc ':possibly--sensitive tweet)))

(defun get-retweet-count (tweet)
  (rest (assoc ':retweet--count tweet)))

(defun get-retweeted (tweet)
  (rest (assoc ':retweeted tweet)))

(defun get-source (tweet)
  (rest (assoc ':source tweet)))

(defun get-truncated (tweet)
  (rest (assoc ':truncated tweet)))

(defun get-user (tweet)
  (rest (assoc ':user tweet)))

(defun get-withheld-copyright (tweet)
  (rest (assoc ':withheld--copyright tweet)))

(defun get-withheld-in-countries (tweet)
  (rest (assoc ':withheld--in--countries tweet)))

(defun get-withheld-scope (tweet)
  (rest (assoc ':withheld--scope tweet)))

(defun get-retweeted-status (tweet)
  (rest (assoc ':retweeted--status tweet)))

;; the first level of user

(defun get-user-profile-image-url-https (user)
  (rest (assoc ':profile--image--url--https user)))

(defun get-user-utc-offset (user)
  (rest (assoc ':utc--offset user)))

(defun get-user-profile-background-image-url (user)
  (rest (assoc ':profile--background--image--url user)))

(defun get-user-statuses-count (user)
  (rest (assoc ':statuses--count user)))

(defun get-user-status (user)
  (rest (assoc ':status user)))

(defun get-user-id (user)
  (rest (assoc ':id user)))

(defun get-user-location (user)
  (rest (assoc ':location user)))

(defun get-user-profile-text-color (user)
  (rest (assoc ':profile--text--color user)))

(defun get-user-show-all-inline-media (user)
  (rest (assoc ':show--all--inline--media user)))

(defun get-user-id-str (user)
  (rest (assoc ':id--str user)))

(defun get-user-name (user)
  (rest (assoc ':name user)))

(defun get-user-profile-use-background-image (user)
  (rest (assoc ':profile--use--background--image user)))

(defun get-user-protected (user)
  (rest (assoc ':protected user)))

(defun get-user-friends-count (user)
  (rest (assoc ':friends--count user)))

(defun get-user-profile-image-url (user)
  (rest (assoc ':profile--image--url user)))

(defun get-user-followers-count (user)
  (rest (assoc ':followers--count user)))

(defun get-user-profile-sidebar-fill-color (user)
  (rest (assoc ':profile--sidebar--fill--color user)))

(defun get-user-description (user)
  (rest (assoc ':description user)))

(defun get-user-time-zone (user)
  (rest (assoc ':time--zone user)))

(defun get-user-profile-background-tile (user)
  (rest (assoc ':profile--background--tile user)))

(defun get-user-following (user)
  (rest (assoc ':following user)))

(defun get-user-is-translator (user)
  (rest (assoc ':is--translator user)))

(defun get-user-profile-background-color (user)
  (rest (assoc ':profile--background--color user)))

(defun get-user-default-profile (user)
  (rest (assoc ':default--profile user)))

(defun get-user-profile-link-color (user)
  (rest (assoc ':profile--link--color user)))

(defun get-user-lang (user)
  (rest (assoc ':lang user)))

(defun get-user-listed-count (user)
  (rest (assoc ':listed--count user)))

(defun get-user-created-at (user)
  (rest (assoc ':created--at user)))

(defun get-user-default-profile-image (user)
  (rest (assoc ':default--profile--image user)))

(defun get-user-contributors-enabled (user)
  (rest (assoc ':contributors--enabled user)))

(defun get-user-verified (user)
  (rest (assoc ':verified user)))

(defun get-user-geo-enabled (user)
  (rest (assoc ':geo--enabled user)))

(defun get-user-profile-sidebar-border-color (user)
  (rest (assoc ':profile--sidebar--border--color user)))

(defun get-user-url (user)
  (rest (assoc ':url user)))

(defun get-user-screen-name (user)
  (rest (assoc ':screen--name user)))

(defun get-user-follow-request-sent (user)
  (rest (assoc ':follow--request--sent user)))

(defun get-user-notifications (user)
  (rest (assoc ':notifications user)))

(defun get-user-profile-background-image-url-https (user)
  (rest (assoc ':profile--background--image--url--https user)))

(defun get-user-favourites-count (user)
  (rest (assoc ':favourites--count user)))

;; the first level of retweeted--status that is tweet but not documented

(defun get-retweeted-status-truncated (retweeted-status)
  (rest (assoc ':truncated retweeted-status)))

(defun get-retweeted-status-in-reply-to-status-id-str (retweeted-status)
  (rest (assoc ':in--reply--to--status--id--str retweeted-status)))

(defun get-retweeted-status-id (retweeted-status)
  (rest (assoc ':id retweeted-status)))

(defun get-retweeted-status-user (retweeted-status)
  (rest (assoc ':user retweeted-status)))

(defun get-retweeted-status-id-str (retweeted-status)
  (rest (assoc ':id--str retweeted-status)))

(defun get-retweeted-status-in-reply-to-status-id (retweeted-status)
  (rest (assoc ':in--reply--to--status--id retweeted-status)))

(defun get-retweeted-status-possibly-sensitive (retweeted-status)
  (rest (assoc ':possibly--sensitive retweeted-status)))

(defun get-retweeted-status-retweeted (retweeted-status)
  (rest (assoc ':retweeted retweeted-status)))

(defun get-retweeted-status-possibly-sensitive-editable (retweeted-status)
  (rest (assoc ':possibly--sensitive--editable retweeted-status)))

(defun get-retweeted-status-entities (retweeted-status)
  (rest (assoc ':entities retweeted-status)))

(defun get-retweeted-status-favorited (retweeted-status)
  (rest (assoc ':favorited retweeted-status)))

(defun get-retweeted-status-text (retweeted-status)
  (rest (assoc ':text retweeted-status)))

(defun get-retweeted-status-in-reply-to-user-id-str (retweeted-status)
  (rest (assoc ':in--reply--to--user--id--str retweeted-status)))

(defun get-retweeted-status-place (retweeted-status)
  (rest (assoc ':place retweeted-status)))

(defun get-retweeted-status-coordinates (retweeted-status)
  (rest (assoc ':coordinates retweeted-status)))

(defun get-retweeted-status-geo (retweeted-status)
  (rest (assoc ':geo retweeted-status)))

(defun get-retweeted-status-in-reply-to-screen-name (retweeted-status)
  (rest (assoc ':in--reply--to--screen--name retweeted-status)))

(defun get-retweeted-status-contributors (retweeted-status)
  (rest (assoc ':contributors retweeted-status)))

(defun get-retweeted-status-source (retweeted-status)
  (rest (assoc ':source retweeted-status)))

(defun get-retweeted-status-retweet-count (retweeted-status)
  (rest (assoc ':retweet--count retweeted-status)))

(defun get-retweeted-status-in-reply-to-user-id (retweeted-status)
  (rest (assoc ':in--reply--to--user--id retweeted-status)))

(defun get-retweeted-status-created-at (retweeted-status)
  (rest (assoc ':created--at retweeted-status)))

;; the first level of entities

(defun get-entities-urls (entities)
  (rest (assoc ':urls entities)))

(defun get-entities-media (entities)
  (rest (assoc ':media entities)))

(defun get-entities-user-mentions (entities)
  (rest (assoc ':user--mentions entities)))

(defun get-entities-hashtags (entities)
  (rest (assoc ':hashtags entities)))

;; the first level of urls

(defun get-urls-display-url (urls)
  (rest (assoc ':display--url urls)))

(defun get-urls-url (urls)
  (rest (assoc ':url urls)))

(defun get-urls-expanded-url (urls)
  (rest (assoc ':expanded--url urls)))

(defun get-urls-indices (urls)
  (rest (assoc ':indices urls)))

;; the first level of media

(defun get-media-display-url (media)
  (rest (assoc ':display--url media)))

(defun get-media-media-url-https (media)
  (rest (assoc ':media--url--https media)))

(defun get-media-id-str (media)
  (rest (assoc ':id--str media)))

(defun get-media-id (media)
  (rest (assoc ':id media)))

(defun get-media-sizes (media)
  (rest (assoc ':sizes media)))

(defun get-media-url (media)
  (rest (assoc ':url media)))

(defun get-media-media-url (media)
  (rest (assoc ':media--url media)))

(defun get-media-expanded-url (media)
  (rest (assoc ':expanded--url media)))

(defun get-media-indices (media)
  (rest (assoc ':indices media)))

(defun get-media-type (media)
  (rest (assoc ':type media)))

;; the first level of user--mentions

(defun get-user-mentions-id (user-mentions)
  (rest (assoc ':id user-mentions)))

(defun get-user-mentions-id-str (user-mentions)
  (rest (assoc ':id--str user-mentions)))

(defun get-user-mentions-name (user-mentions)
  (rest (assoc ':name user-mentions)))

(defun get-user-mentions-screen-name (user-mentions)
  (rest (assoc ':screen--name user-mentions)))

(defun get-user-mentions-indices (user-mentions)
  (rest (assoc ':indices user-mentions)))

;; the first level of hashtags

(defun get-hashtags-indices (hashtags)
  (rest (assoc ':indices hashtags)))

(defun get-hashtags-text (hashtags)
  (rest (assoc ':text hashtags)))

;; the first level of sizes

(defun get-sizes-large (sizes)
  (rest (assoc ':large sizes)))

(defun get-sizes-medium (sizes)
  (rest (assoc ':medium sizes)))

(defun get-sizes-thumb (sizes)
  (rest (assoc ':thumb sizes)))

(defun get-sizes-small (sizes)
  (rest (assoc ':small sizes)))

;; the first level of large, medium, thumb, small

(defun get-subsizes-w (subsizes)
  (rest (assoc ':w subsizes)))

(defun get-subsizes-resize (subsizes)
  (rest (assoc ':resize subsizes)))

(defun get-subsizes-h (subsizes)
  (rest (assoc ':h subsizes)))

;; the first level of attributes

(defun get-attributes-street-address (attributes)
  (rest (assoc ':street--address attributes)))

(defun get-attributes-locality (attributes)
  (rest (assoc ':locality attributes)))

(defun get-attributes-region (attributes)
  (rest (assoc ':region attributes)))

(defun get-attributes-iso3 (attributes)
  (rest (assoc ':iso3 attributes)))

(defun get-attributes-postal-code (attributes)
  (rest (assoc ':postal--code attributes)))

(defun get-attributes-phone (attributes)
  (rest (assoc ':phone attributes)))

(defun get-attributes-twitter (attributes)
  (rest (assoc ':twitter attributes)))

(defun get-attributes-url (attributes)
  (rest (assoc ':url attributes)))

(defun get-attributes-|app:id| (attributes)
  (rest (assoc ':|app:id| attributes)))

;; the first level of bounding--box

(defun get-bounding-box-coordinates (bounding-box)
  (rest (assoc ':coordinates bounding-box)))

(defun get-bounding-box-type (bounding-box)
  (rest (assoc ':type bounding-box)))

;; the first level of places

(defun get-places-name (places)
  (rest (assoc ':name places)))

(defun get-places-attributes (places)
  (rest (assoc ':attributes places)))

(defun get-places-full-name (places)
  (rest (assoc ':full--name places)))

(defun get-places-place-type (places)
  (rest (assoc ':place--type places)))

(defun get-places-url (places)
  (rest (assoc ':url places)))

(defun get-places-bounding-box (places)
  (rest (assoc ':bounding--box places)))

(defun get-places-geometry (places)
  (rest (assoc ':geometry places)))

(defun get-places-polylines (places)
  (rest (assoc ':polylines places)))

(defun get-places-country (places)
  (rest (assoc ':country places)))

(defun get-places-id (places)
  (rest (assoc ':id places)))

(defun get-places-contained-within (places)
  (rest (assoc ':contained--within places)))

(defun get-places-country-code (places)
  (rest (assoc ':country--code places)))

;; the first level of contributors

(defun get-contributors-id (contributors)
  (rest (assoc ':id contributors)))

(defun get-contributors-id-str (contributors)
  (rest (assoc ':id--str contributors)))

(defun get-contributors-screen-name (contributors)
  (rest (assoc ':screen--name contributors)))

;; the first level of coordinates

(defun get-coordinates-coordinates (coordinates)
  (rest (assoc ':coordinates coordinates)))

(defun get-coordinates-type (coordinates)
  (rest (assoc ':type coordinates)))

;; the first level of current--user--retweet

(defun get-current-user-retweet-id (current-user-retweet)
  (rest (assoc ':id current-user-retweet)))
(defun get-current-user-retweet-id-str (current-user-retweet)
  (rest (assoc ':id--str current-user-retweet)))

;; builders
;; building entities

(defun make-small (media)
  (let (
        (w (get-subsizes-w (get-sizes-small (get-media-sizes media))))
        (h (get-subsizes-h (get-sizes-small (get-media-sizes media))))
        (r (get-subsizes-resize (get-sizes-small (get-media-sizes media))))
       )
    (if (or w h r) (make-instance 'small :w w :h h :resize r) ())))

(defun make-thumb (media)
  (let (
        (w (get-subsizes-w (get-sizes-thumb (get-media-sizes media))))
        (h (get-subsizes-h (get-sizes-thumb (get-media-sizes media))))
        (r (get-subsizes-resize (get-sizes-thumb (get-media-sizes media))))
       )
    (if (or w h r) (make-instance 'thumb :w w :h h :resize r) ())))

(defun make-medium (media)
  (let (
        (w (get-subsizes-w (get-sizes-medium (get-media-sizes media))))
        (h (get-subsizes-h (get-sizes-medium (get-media-sizes media))))
        (r (get-subsizes-resize (get-sizes-medium (get-media-sizes media))))
       )
    (if (or w h r) (make-instance 'medium :w w :h h :resize r) ())))

(defun make-large (media)
  (let (
        (w (get-subsizes-w (get-sizes-large (get-media-sizes media))))
        (h (get-subsizes-h (get-sizes-large (get-media-sizes media))))
        (r (get-subsizes-resize (get-sizes-large (get-media-sizes media))))
       )
    (if (or w h r) (make-instance 'large :w w :h h :resize r) ())))

(defun make-media-sizes (media)
   (let (
	 (large (make-large media))
	 (medium (make-medium media))
	 (thumb (make-thumb media))
	 (small (make-small media))
	 )
     (if (or large medium thumb small) (make-instance 'sizes :large large
				                    :medium medium
				                    :thumb thumb
				                    :small small) ())))


(defun make-media (media)
   (let (
	 (display-url (get-media-display-url media))
	 (media-url-https (get-media-media-url-https media))
	 (id-str (get-media-id-str media))
	 (id (get-media-id media))
	 (sizes (make-media-sizes media))
	 (url (get-media-url media))
	 (media-url (get-media-media-url media))
	 (expanded-url (get-media-expanded-url media))
	 (indices (get-media-indices media))
	 (type (get-media-type media))
	 )
     (if (or display-url media-url-https id-str id sizes url media-url expanded-url indices type) 
	 (make-instance 'media :display-url display-url
			:media-url-https media-url-https
			:id-str id-str
			:id id
			:sizes sizes
			:url url
			:media-url media-url
			:expanded-url expanded-url
			:indices indices
			:type type) ())))

(defun make-media-list (medias)
   (let ((out ()))
     (dolist (media medias) (setf out (append out (list (make-media media))))) out ))

(defun make-urls (urls)
  (let (
	(display-url (get-urls-display-url urls))
	(url (get-urls-url urls))
	(expanded-url (get-urls-expanded-url urls))
	(indices (get-urls-indices urls))
	)
    (if (or display-url url expanded-url indices) (make-instance 'urls
								   :display-url display-url
								   :url url
								   :expanded-url expanded-url
								   :indices indices) ())))

(defun make-urls-list (urls)
   (let ((out ()))
     (dolist (url urls) (setf out (append out (list (make-urls url))))) out ))

(defun make-user-mentions (user-mentions)
   (let (
	 (id (get-user-mentions-id user-mentions))
	 (id-str (get-user-mentions-id-str user-mentions))
	 (name (get-user-mentions-name user-mentions))
	 (screen-name (get-user-mentions-screen-name user-mentions))
	 (indices (get-user-mentions-indices user-mentions))
	 )
     (if (or id id-str name screen-name indices) (make-instance 'user-mentions
								  :id id
								  :id-str id-str
								  :name name
								  :screen-name screen-name
								  :indices indices) ())))

(defun make-user-mentions-list (user-mentions)
   (let ((out ()))
     (dolist (um user-mentions) (setf out (append out (list (make-user-mentions um))))) out ))

(defun make-hashtags (hashtags)
   (let (
	 (indices (get-hashtags-indices hashtags))
	 (text (get-hashtags-text hashtags))
	 )
     (if (or indices text) (make-instance 'hashtags 
					  :indices indices
					  :text text) ())))

(defun make-hashtags-list (hashtags)
   (let ((out ()))
     (dolist (hashtag hashtags) (setf out (append out (list (make-hashtags hashtag))))) out ))

(defun make-entities (entities)
   (let (
	 (urls (make-urls-list (get-entities-urls entities)))
	 (media (make-media-list (get-entities-media entities)))
	 (user-mentions (make-user-mentions-list (get-entities-user-mentions entities)))
	 (hashtags (make-hashtags-list (get-entities-hashtags entities)))
	 )
     (if (or urls media user-mentions hashtags) (make-instance 'entities
								:urls urls
								:media media
								:user-mentions user-mentions
								:hashtags hashtags) ())))
;; final helpers

(defun make-coordinates (tweet)
   (let* (
	 (temp (get-coordinates tweet))
	 (coordinates (get-coordinates-coordinates temp))
	 (type (get-coordinates-type temp))
	 )
     (if (or coordinates type) (make-instance 'coordinates 
			                    :coordinates coordinates
			                    :type type) ())))

(defun make-contributors (contributors)
   (let (
	 (id (get-contributors-id contributors))
	 (id-str (get-contributors-id-str contributors))
	 (screen-name (get-contributors-screen-name contributors))
	 )
     (if (or id id-str screen-name) (make-instance 'contributors
				                   :id id
				                   :id-str id-str
				                   :screen-name screen-name) ())))

(defun make-contributors-list (contributors)
   (let ((out ()))
   (dolist (cont contributors) (setf out (append out (list (make-contributors cont))))) out ))

(defun make-current-user-retweet (current-user-retweet)
   (let (
	 (id (get-current-user-retweet-id current-user-retweet))
	 (id-str (get-current-user-retweet-id-str current-user-retweet))
	 )
     (if (or id id-str) (make-instance 'current-user-retweet
					:id id
					:id-str id-str) ())))

(defun make-attributes (attributes)
   (let (
	 (street-address (get-attributes-street-address attributes))
	 (locality (get-attributes-locality attributes))
	 (region (get-attributes-region attributes))
	 (iso3 (get-attributes-iso3 attributes))
	 (postal-code (get-attributes-postal-code attributes))
	 (phone (get-attributes-phone attributes))
	 (twitter (get-attributes-twitter attributes))
	 (url (get-attributes-url attributes))
	 (|app:id| (get-attributes-|app:id| attributes))
	 )
     (if (or street-address locality region iso3 postal-code phone twitter url |app:id| ) 
     (make-instance 'attributes
		   :street-address street-address
		   :locality locality
		   :region region
		   :iso3 iso3
		   :postal-code postal-code
		   :phone phone
		   :twitter twitter
		   :url url
		   :|app:id| |app:id|) ())))

(defun make-bounding-box (bounding-box)
   (let (
	 (coordinates (get-bounding-box-coordinates bounding-box))
	 (type (get-bounding-box-type bounding-box))
	 )
     (if (or coordinates type) (make-instance 'bounding-box
			                    :coordinates coordinates
			                    :type type) ())))

(defun make-places (places)
   (let (
	 (name (get-places-name places))
	 (attributes (make-attributes (get-places-attributes places)))
	 (full-name (get-places-full-name places))
	 (place-type (get-places-place-type places))
	 (url (get-places-url places))
	 (bounding-box (make-bounding-box (get-places-bounding-box places)))
	 (geometry (get-places-geometry places))
	 (polylines (get-places-polylines places))
	 (country (get-places-country places))
	 (id (get-places-id places))
	 (contained-within (get-places-contained-within places))
	 (country-code (get-places-country-code places))
	 )
     (if (or name attributes full-name place-type url bounding-box geometry polylines country id contained-within country-code)
	 (make-instance 'places
			:name name
			:attributes attributes
			:full-name full-name
			:place-type place-type
			:url url
			:bounding-box bounding-box
			:geometry geometry
			:polylines polylines
			:country country
			:id id
			:contained-within contained-within
			:country-code country-code) ())))

;; version 1 needs status builder and maybe more

(defun make-user (user)
    (let (
            (profile-image-url-https (get-user-profile-image-url-https user))
            (utc-offset (get-user-utc-offset user))
            (profile-background-image-url (get-user-profile-background-image-url user))
            (statuses-count (get-user-statuses-count user))
	    (status (get-user-status user))
            (id (get-user-id user))
            (location (get-user-location user))
            (profile-text-color (get-user-profile-text-color user))
            (show-all-inline-media (get-user-show-all-inline-media user))
            (id-str (get-user-id-str user))
            (name (get-user-name user))
            (profile-use-background-image (get-user-profile-use-background-image user))
            (protected (get-user-protected user))
            (friends-count (get-user-friends-count user))
            (profile-image-url (get-user-profile-image-url user))
            (followers-count (get-user-followers-count user))
            (profile-sidebar-fill-color (get-user-profile-sidebar-fill-color user))
            (description (get-user-description user))
            (time-zone (get-user-time-zone user))
            (profile-background-tile (get-user-profile-background-tile user))
            (following (get-user-following user))
            (is-translator (get-user-is-translator user))
            (profile-background-color (get-user-profile-background-color user))
            (default-profile (get-user-default-profile user))
            (profile-link-color (get-user-profile-link-color user))
            (lang (get-user-lang user))
            (listed-count (get-user-listed-count user))
            (created-at (get-user-created-at user))
            (default-profile-image (get-user-default-profile-image user))
            (contributors-enabled (get-user-contributors-enabled user))
            (verified (get-user-verified user))
            (geo-enabled (get-user-geo-enabled user))
            (profile-sidebar-border-color (get-user-profile-sidebar-border-color user))
            (url (get-user-url user))
            (screen-name (get-user-screen-name user))
            (follow-request-sent (get-user-follow-request-sent user))
            (notifications (get-user-notifications user))
            (profile-background-image-url-https (get-user-profile-background-image-url-https user))
            (favourites-count (get-user-id user)))
      (if status (setf status (make-tweet status)))
      (if (or profile-image-url-https utc-offset profile-background-image-url statuses-count status id location profile-text-color show-all-inline-media id-str name profile-use-background-image protected friends-count profile-image-url followers-count profile-sidebar-fill-color description time-zone profile-background-tile following is-translator profile-background-color default-profile profile-link-color lang listed-count created-at default-profile-image contributors-enabled verified geo-enabled profile-sidebar-border-color url screen-name follow-request-sent notifications profile-background-image-url-https favourites-count) (make-instance 'user 
								:profile-image-url-https profile-image-url-https
								:utc-offset utc-offset
								:profile-background-image-url profile-background-image-url
								:statuses-count statuses-count
								:status status
								:id id
								:location location
								:profile-text-color profile-text-color
								:show-all-inline-media show-all-inline-media
								:id-str id-str
								:name name
								:profile-use-background-image profile-use-background-image
								:protected protected
								:friends-count friends-count
								:profile-image-url profile-image-url
								:followers-count followers-count
								:profile-sidebar-fill-color profile-sidebar-fill-color
								:description description
								:time-zone time-zone
								:profile-background-tile profile-background-tile
								:following following
								:is-translator is-translator
								:profile-background-color profile-background-color
								:default-profile default-profile
								:profile-link-color profile-link-color
								:lang lang
								:listed-count listed-count
								:created-at created-at
								:default-profile-image default-profile-image
								:contributors-enabled contributors-enabled
								:verified verified
								:geo-enabled geo-enabled
								:profile-sidebar-border-color profile-sidebar-border-color
								:url url
								:screen-name screen-name
								:follow-request-sent follow-request-sent
								:notifications notifications
								:profile-background-image-url-https profile-background-image-url-https
								:favourites-count favourites-count) ())))

;; status is tweet - now need geo stuff to finish

(defun make-tweet (tweet)
  (let (
   (contributors (make-contributors-list (get-contributors tweet)))
   (geo (get-geo tweet))
   (coordinates (make-coordinates (get-coordinates tweet)))
   (created-at (get-created-at tweet))
   (current-user-retweet (make-current-user-retweet (get-current-user-retweet tweet)))
   (entities (make-entities (get-entities tweet)))
   (favorited (get-favorited tweet))
   (id (get-id tweet))
   (id-str (get-id-str tweet))
   (in-reply-to-screen-name (get-in-reply-to-screen-name tweet))
   (in-reply-to-status-id (get-in-reply-to-status-id tweet))
   (in-reply-to-status-id-str (get-in-reply-to-status-id-str tweet))
   (in-reply-to-user-id (get-in-reply-to-user-id tweet))
   (in-reply-to-user-id-str (get-in-reply-to-user-id-str tweet))
   (place (make-places (get-place tweet)))
   (possibly-sensitive (get-possibly-sensitive tweet))
   (retweet-count (get-retweet-count tweet))
   (retweeted (get-retweeted tweet))
   (source (get-source tweet))
   (text (get-text tweet))
   (truncated (get-truncated tweet))
   (user (get-user tweet))
   (retweeted-status (get-retweeted-status tweet))
   (withheld-copyright (get-withheld-copyright tweet))
   (withheld-in-countries (get-withheld-in-countries tweet))
   (withheld-scope (get-withheld-scope tweet))
   )
      (if retweeted-status (setf retweeted-status (make-tweet retweeted-status)))
      (if user (setf user (make-user user)))
      (if (or contributors geo coordinates created-at current-user-retweet entities favorited id id-str in-reply-to-screen-name in-reply-to-status-id in-reply-to-status-id-str in-reply-to-user-id in-reply-to-user-id-str place possibly-sensitive retweet-count retweeted source text truncated user retweeted-status withheld-copyright withheld-in-countries withheld-scope) 
   (make-instance 'tweet 
                  :contributors contributors
                  :geo geo
                  :coordinates coordinates
                  :created-at created-at
                  :current-user-retweet current-user-retweet
                  :entities entities
                  :favorited favorited
                  :id id
                  :id-str id-str
                  :in-reply-to-screen-name in-reply-to-screen-name
                  :in-reply-to-status-id in-reply-to-status-id
                  :in-reply-to-status-id-str in-reply-to-status-id-str
                  :in-reply-to-user-id in-reply-to-user-id
                  :in-reply-to-user-id-str in-reply-to-user-id-str
                  :place place
                  :possibly-sensitive possibly-sensitive
                  :retweet-count retweet-count
                  :retweeted retweeted
                  :source source
                  :text text
                  :truncated truncated
                  :user user
                  :retweeted-status retweeted-status
                  :withheld-copyright withheld-copyright
                  :withheld-in-countries withheld-in-countries
                  :withheld-scope withheld-scope) ())))
