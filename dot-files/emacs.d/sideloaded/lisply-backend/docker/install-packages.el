;; Package installation script for Lisply Backend
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; Install simple-httpd
(package-install 'simple-httpd)

;; Make sure we can require it
(require 'simple-httpd)
