;;; init-osm.el --- OSM. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require-package 'osm)

(with-eval-after-load 'org
  (require 'osm-ol))

(provide 'init-osm)
;;; init-osm.el ends here
