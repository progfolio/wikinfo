;; -*- lexical-binding: t; -*-
(require 'wikinfo)

(let ((helm-candidate-separator (nth 1 '(" " "🞜"))))
  (wikinfo-infobox (wikinfo-search "")))

