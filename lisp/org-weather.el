;;; org-weather.el --- Set weather tag for Org mode entries -*- lexical-binding: t; -*-

;; Author: Slava Barinov <rayslava@gmail.com>
;; Maintainer: Slava Barinov <rayslava@gmail.com>
;; Created: 03 July 2024
;; Version: 1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: weather, convenience
;; URL: https://gist.github.com/rayslava/aa13a24deb66e9f05c7c2b0b1af3629c
;; License: GPL-3+

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides a function to update the `:WEATHER:` property of an Org
;; mode entry with the current weather information for a specified location. The
;; location is taken from the `:LOCATION:` property of the entry. If the property
;; does not exist, the user is prompted to enter a location.

;; To use this package, load it and call `org-weather-update-property` in an
;; Org mode buffer.

;;; Code:

(require 'org)
(require 'json)
(require 'request)

(defcustom openweathermap-api-key "YOUR_API_KEY"
  "API key for OpenWeatherMap.")

(defun org-weather--get-coordinates (city callback)
  "Get the latitude and longitude for CITY using OpenWeatherMap Geocoding API.
Calls CALLBACK with the coordinates."
  (let ((url (format "https://api.openweathermap.org/geo/1.0/direct?q=%s&limit=1&appid=%s"
                     (url-hexify-string city) openweathermap-api-key)))
    (request
      url
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (if (and data (vectorp data) (> (length data) 0))
                      (let* ((location (elt data 0))
                             (lat (alist-get 'lat location))
                             (lon (alist-get 'lon location)))
                        (funcall callback (cons lat lon)))
                    (error "Location not found"))))
      :error (cl-function
              (lambda (&rest _)
                (error "Failed to retrieve coordinates"))))))

(defun org-weather--get-weather (lat lon callback)
  "Get the current weather for LAT and LON using OpenWeatherMap API.
Calls CALLBACK with the weather summary."
  (let ((url (format "https://api.openweathermap.org/data/2.5/weather?lat=%s&lon=%s&units=metric&appid=%s"
                     lat lon openweathermap-api-key)))
    (request
      url
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((weather-summary (format "%s, %s°C"
                                                 (alist-get 'description (elt (alist-get 'weather data) 0))
                                                 (alist-get 'temp (alist-get 'main data)))))
                    (funcall callback weather-summary))))
      :error (cl-function
              (lambda (&rest _)
                (error "Failed to retrieve weather"))))))

(defun org-weather--get-city-name ()
  "Get the value of the :LOCATION: property of the current header.
If the property does not exist, prompt the user for the location and update the property."
  (let ((location (org-entry-get nil "LOCATION")))
    (unless location
      (setq location (read-string "Enter location: "))
      (org-entry-put nil "LOCATION" location))
    location))

;;;###autoload
(defun org-weather-update-property ()
  "Update the :WEATHER: property with the current weather at :LOCATION:."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let ((city (org-weather--get-city-name)))
      (if city
          (org-weather--get-coordinates city
                           (lambda (coordinates)
                             (let ((lat (car coordinates))
                                   (lon (cdr coordinates)))
                               (org-weather--get-weather lat lon
                                            (lambda (weather)
                                              (org-entry-put nil "WEATHER" weather)
                                              (message "Weather updated: %s" weather))))))
        (message "No :LOCATION: property found")))))

(provide 'org-weather)

;;; org-weather.el ends here
