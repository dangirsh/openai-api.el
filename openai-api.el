;;; openai-api.el ---  An interface to OpenAI's language models API

;;; COMMENTARY:
;;; This is currently only includes the "completion" POST request.
;;; Other endpoints should be easy to add.
;;;
;; Requires an OpenAI API Key
;; Codex models (for generating code) currently require private beta access.
;;
;; This API tries to prevent unnecessarily wasteful requests. Right now, that means
;; preventing requests that have n>1 and temperature=0.0 (multiple identical results).
;;
;;
;; Configuration:
;;
;; See: https://beta.openai.com/account/api-keys
;; (setq openai-api-secret-key <key>)  ; required
;;
;; (Optional) Engine selection
;; See: https://beta.openai.com/docs/engines
;; (setq openai-api-engine "davinci") ; defaults to "davinci"
;;
;; (Optional) Set custom completion parameters
;; See: https://beta.openai.com/docs/api-reference/completions/create
;;(setq openai-api-completion-params '(("max_tokens" . 128)
;;                                     ("temperature" . 0)
;;                                     ("frequency_penalty" . 0)
;;                                     ("presence_penalty" . 0)))
;;                                     ("n" . 1)))

(require 'json)
(require 'request)

;;; Code:
(defvar openai-api-secret-key nil)

(defvar openai-api--version "v1")
(defvar openai-api--base-url (format "https://api.openai.com/%s" openai-api--version))

(defvar openai-api-engine "davinci")


(defvar openai-api-completion-params '((max_tokens . 128)
                                       (temperature . 0)
                                       (frequency_penalty . 0)
                                       (presence_penalty . 0)
                                       (n . 1)))


(defun openai-api--post-request (url body-params)
  "Submit a POST request to the OpenAI API with given URL and BODY-PARAMS."
  (unless openai-api-secret-key
    (error "You must configure your OpenAI secret key with (setq openai-api-secret-key <key>). See https://beta.openai.com/account/api-keys"))
  (let ((response-body))
    (request
      url
      :type "POST"
      :headers `(("Content-Type" . "application/json")
                 ("Authorization" . ,(format "Bearer %s" openai-api-secret-key)))
      :data (json-encode-alist body-params)
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (setq response-body (request-response-data response)))))
    response-body))

(defun openai-api--sanitize-completion-params ()
  "Fixup wasteful or meanless completion parameter settings, and return better ones."
  (let ((sanitized-completion-params openai-api-completion-params))
    (if (and (equal (alist-get 'temperature openai-api-completion-params) 0.0)
             (> (alist-get 'n openai-api-completion-params) 1))
        (display-warning 'openai-api
                         "OpenAI API: Requesting multiple responses (n>1) with temperature=0.0 is useless, setting n=1.")
      (setf (alist-get 'n sanitized-completion-params) 1))
    sanitized-completion-params))


(defun openai-api--completion-request (prompt)
  "Submit a completion request for PROMPT. See: https://beta.openai.com/docs/api-reference/completions."
  (let ((openai-api-completion-params (openai-api--sanitize-completion-params)))
    (openai-api--post-request
     (format "%s/engines/%s/completions" openai-api--base-url openai-api-engine)
     `(("prompt" . ,prompt) ,@openai-api-completion-params))))


(defun openai-api--parse-completion-response (completion-response)
  "Parse the 'text' values from the 'choices' array in the COMPLETION-RESPONSE."
  (let* ((completions (alist-get 'choices completion-response))
         (extract-result #'(lambda (choice) (string-trim (alist-get 'text choice))))
         (results (seq-map extract-result completions)))
    results))


(defun openai-api-get-completions (prompt)
  "Return a list of completions for PROMPT, with length given by the request body param 'n'."
  (openai-api--parse-completion-response
   (openai-api--completion-request prompt)))

(provide 'openai-api)

;;; openai-api.el ends here
