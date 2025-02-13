;;; immersive-translate-ollama.el --- Ollama backend for immersive-translation -*- lexical-binding: t; -*-

;; Based on work by
;; Copyright (C) 2023  stardiviner
;; Original Author: stardiviner <numbchild@gmail.com>
;; Keywords: convenience

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:

(require 'immersive-translate-curl)

(defgroup immersive-translate-ollama nil
  "Immersive translate ollama backend."
  :group 'immersive-translate)

(defcustom immersive-translate-ollama-host "localhost:11434"
  "The Ollama API host queried by immersive-translate."
  :group 'immersive-translate-ollama
  :type 'string)

(defcustom immersive-translate-ollama-model "llama3.2"
  "GPT Model for chat."
  :group 'immersive-translate-ollama
  :type '(choice
          (const :tag "DeepSeek R1 7b" "deepseek-r1:7b")
          (const :tag "llama 3.2" "llama3.2:latest")
          (const :tag "llama 3.3" "llama3.3:latest")))

(defcustom immersive-translate-ollama-temperature 0.7
  "\"Temperature\" of Ollama response.

This is a number between 0.0 and 2.0 that controls the randomness
of the response, with 2.0 being the most random."
  :group 'immersive-translate-ollama
  :type 'number)

(defcustom immersive-translate-ollama-system-prompt "You are a professional translator."
  "System prompt used by Ollama."
  :group 'immersive-translate-ollama
  :type 'string)

(defcustom immersive-translate-ollama-user-prompt "You will be provided with text delimited by triple backticks, your task is to translate the wrapped text into Chinese. You should only output the translated text. \n```%s```"
  "User prompt used by Ollama."
  :group 'immersive-translate-ollama
  :type 'string)


(defun immersive-translate-ollama--request-data (prompts)
  "Encode data into JSON format..

Argument PROMPTS are for sending to Ollama."
  (let ((prompts-plist
         `( :model ,immersive-translate-ollama-model
            :messages [,@prompts]
            ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-JSON.html
            :stream :false ; :false, :null
            :options (:temperature ,immersive-translate-ollama-temperature))))
    ;; (plist-put prompts-plist :temperature immersive-translate-ollama-temperature)
    prompts-plist))

(defun immersive-translate-ollama-get-args (prompts token)
  "Produce list of arguments for calling cURL.

PROMPTS is the data to send, TOKEN is a unique identifier."
  (let* ((url (format "https://%s/api/chat" immersive-translate-ollama-host))
         (data-json (encode-coding-string
                     (json-serialize (immersive-translate-ollama--request-data prompts))
                     'utf-8))
         (headers `(("Content-Type" . "application/json"))))
    (append (list url)
            (cl-loop for (key . val) in headers
                     append (list "--header" (format "%s: %s" key val)))
            (list "--disable" "--location" "--silent" "--compressed"
                  "-X" "POST"
                  "--write-out" (format "(%s . %%{size_header})" token) ; "-w"
                  "--max-time" (number-to-string 60) ; "-m"
                  "--dump-header" "-" ; "-D-"
                  "--data" data-json))))

(defun immersive-translate-ollama-create-prompt (content)
  "Create a full prompt suitable for sending to Ollama.

CONTENT is the text to be translated."
  (let ((user-prompt (format immersive-translate-ollama-user-prompt content)))
    `((:role "system" :content ,immersive-translate-ollama-system-prompt)
      (:role "user"   :content ,user-prompt))))

(defun immersive-translate-curl-ollama-get-translation (response)
  "Get the translated text in RESPONSE returned by Ollama."
  (map-nested-elt response '(:choices 0 :message :content)))

(add-to-list 'immersive-translate-curl-get-translation-alist
             '(ollama . immersive-translate-curl-ollama-get-translation))

(add-to-list 'immersive-translate-curl-get-args-alist
             '(ollama . immersive-translate-ollama-get-args))

(defun immersive-translate-ollama-translate (info &optional callback)
  "Translate the content in INFO using Ollama.

INFO is a plist with the following keys:
- :content (the text needed to be translated)
- :buffer (the current buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (immersive-translate-curl-do 'ollama info callback))

(require 'request)

(defun immersive-translate-ollama-translate (info &optional callback)
  "Translate the content in INFO using Ollama.

INFO is a plist with the following keys:
- :content (the text needed to be translated)
- :buffer (the current buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (let* ((url (format "https://%s/api/chat" immersive-translate-ollama-host))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (prompts (immersive-translate-ollama-create-prompt (plist-get info :content)))
         (url-request-data (encode-coding-string
                            (json-serialize (immersive-translate-ollama--request-data prompts))
                            'utf-8))
         (url-request-callback (lambda (status _args)
                                 ;; TODO:
                                 (with-current-buffer (current-buffer)
                                   (set-buffer-multibyte t) ; support Chinese
                                   (let* ((json (or (json-read-from-string
                                                     (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max)))
                                                    (json-read))))
                                     (pp json))))))
    ;; check out URL buffer " *http localhost:11434*"
    ;; FAIL:
    ;; (url-retrieve url url-request-callback)
    ;; FAIL:
    (with-current-buffer (url-retrieve url url-request-callback)
      (set-buffer-multibyte t) ; support Chinese
      (let* ((json (or (json-read-from-string
                        (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max)))
                       (json-read))))
        (pp json)))
    ;; FAIL:
    (request url
      :type url-request-method
      :params url-request-extra-headers
      :data url-request-data
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message "I sent: %S" (assoc-default 'args data)))))
    ))

;;; TEST: A simple HTTP API request to ollama serve
(let* ((url (format "https://%s/api/chat" immersive-translate-ollama-host))
       (url-request-method "POST")
       (url-request-extra-headers '(("Content-Type" . "application/json")
                                    ;; ("Authorization" . "Bearer no-key")
                                    ))
       (prompts "hello")
       (url-request-data (encode-coding-string
                          (json-serialize (immersive-translate-ollama--request-data prompts))
                          'utf-8))
       (url-request-callback (lambda (status _args)
                               ;; TODO:
                               (with-current-buffer (current-buffer)
                                 (set-buffer-multibyte t) ; support Chinese
                                 (let* ((json (or (json-read-from-string
                                                   (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max)))
                                                  (json-read))))
                                   (pp json))))))

  ;; (url-retrieve-synchronously url)
  ;; (url-retrieve url url-request-callback)

  (request url
    :type url-request-method
    ;; :params url-request-extra-headers
    :data url-request-data
    ;; :parser 'json-read

    :parser 'buffer-string
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (when data
                  (with-current-buffer (get-buffer-create "*request demo*")
                    (erase-buffer)
                    (insert data)
                    (pop-to-buffer (current-buffer))))))
    :error (cl-function
            (lambda (&rest args &key error-thrown &allow-other-keys)
              (message "Got error: %S" error-thrown)))
    ;; :complete (lambda (&rest _) (message "Finished!"))
    :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                   (418 . (lambda (&rest _) (message "Got 418.")))))
  )


(provide 'immersive-translate-ollama)
;;; immersive-translate-ollama.el ends here
