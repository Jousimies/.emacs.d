;;; init-dict.el --- Dictionary -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2022 Duan Ning

;; Author: Duan Ning <duan_n@outlook.com>
;; Version: 1.0.0
;; Keywords:

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Dict source
;; https://github.com/skywind3000/ECDICT
;; http://download.huzheng.org/dict.org/

;;

;;; Code:

(when (maybe-require-package 'langtool)
  (setq langtool-http-server-host "localhost")
  (setq langtool-http-server-port 8081))

(when (maybe-require-package 'go-translate)
  (with-eval-after-load 'go-translate
    (setq gts-buffer-follow-p t)
    (setq gts-translate-list '(("en" "zh")))
    (setq gts-default-translator (gts-translator
                                  :picker (gts-noprompt-picker)
                                  :engines (list
                                            (gts-google-engine :parser (gts-google-summary-parser)))
                                  :render (gts-buffer-render)))))


;; google-translate 没有 go-translate 好使。
;; (when (maybe-require-package 'google-translate)
;;   (setq google-translate-translation-directions-alist '(("en" . "zh-CN")))
;;   (setq google-translate-pop-up-buffer-set-focus t))

;; lingva
;; go-translate 的备用。
(when (maybe-require-package 'lingva)
  (setq lingva-target "zh"))

;; sdcv
;; 其反回的结果有点乱糟糟，我更喜欢使用 osx-dictionary。
;; (require 'sdcv)
(setq sdcv-dictionary-data-dir (expand-file-name "sdcv-dict" user-emacs-directory))
(setq sdcv-program "/opt/homebrew/bin/sdcv")
(setq sdcv-dictionary-simple-list    ;星际译王屏幕取词词典, 简单, 快速
      '("懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "KDic11万英汉词典"))
(setq sdcv-dictionary-complete-list     ;星际译王的词典, 完全, 详细
      '("牛津英汉双解美化版"
        "懒虫简明英汉词典"
        "英汉汉英专业词典"
        "XDICT英汉辞典"
        "stardict1.3英汉辞典"
        "WordNet"
        "XDICT汉英辞典"
        "Jargon"
        "懒虫简明汉英词典"
        "FOLDOC"
        "新世纪英汉科技大词典"
        "KDic11万英汉词典"
        "朗道汉英字典5.0"
        "CDICT5英汉辞典"
        "新世纪汉英科技大词典"
        "21世纪双语科技词典"
        "quick_eng-zh_CN"))

(with-eval-after-load 'evil-collection
  (evil-collection-define-key 'normal 'sdcv-mode-map
   "q" 'quit-window))

;; osx-dictionary
(require-package 'osx-dictionary)

;; websocket-bridge and dictionary-overlay can be used to learn english words.
(run-with-idle-timer 10 nil (lambda ()
                             (require 'websocket-bridge)
                             (require 'dictionary-overlay)
                             (dictionary-overlay-start)))

(with-eval-after-load 'dictionary-overlay
  (setq dictionary-overlay-user-data-directory (expand-file-name "var/dictionary-overlay" user-emacs-directory))
  (with-eval-after-load 'osx-dictionary
    (advice-add 'osx-dictionary-search-pointer :after 'dictionary-overlay-mark-word-unknown)))


(provide 'init-dict)
;;; init-dict.el ends here
