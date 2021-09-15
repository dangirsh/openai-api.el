;;; openai-api.el ---  An interface to OpenAI's language models API

;;; COMMENTARY:
;;; This is currently only includes the "completion" POST request.
;;; Other endpoints should be easy to add.
;;;
;; Requires an OpenAI API Key
;; Codex models (for generating code) currently require private beta access.
;;
;; Configuration: See README.org

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


(defun openai-api--validate-secret-key ()
  (unless openai-api-secret-key
    (error
     (concat "You must configure your OpenAI secret key with (setq openai-api-secret-key <key>).\n"
             "See https://beta.openai.com/account/api-keys"))))

(defun openai-api--validate-engine ()
  (unless (member openai-api-engine (openai-api-get-engines))
    (error
     (format "Invalid engine %s. Must be one of: %s" openai-api-engine (openai-api-get-engines)))))

(defun openai-api--request-header ()
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(format "Bearer %s" openai-api-secret-key))))


(defun openai-api--get-request (url)
  "Submit a POST request to the OpenAI API with given URL and BODY-PARAMS."
  (openai-api--validate-secret-key)
  (let ((response-body))
    (request
      url
      :type "GET"
      :headers (openai-api--request-header)
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (setq response-body (request-response-data response)))))
    response-body))

(defun openai-api--get-engines-request ()
  (openai-api--get-request (format "%s/engines" openai-api--base-url)))

(defun openai-api--parse-engines-response (engines-response)
  (let* ((engines-alist (alist-get 'data engines-response))
         (extract-result #'(lambda (engine) (alist-get 'id engine)))
         (engine-names (seq-map extract-result engines-alist)))
    engine-names))


(defvar openai-engines-cache nil)

(defun openai-api-get-engines (&optional no-cache)
  (if (and openai-engines-cache (not no-cache))
      openai-engines-cache
    (setq openai-engines-cache
          (openai-api--parse-engines-response
           (openai-api--get-engines-request)))))


(defun openai-api--post-request (url body-params)
  "Submit a POST request to the OpenAI API with given URL and BODY-PARAMS."
  (openai-api--validate-secret-key)
  (openai-api--validate-engine)
  (let ((response-body))
    (request
      url
      :type "POST"
      :headers (openai-api--request-header)
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

(defun openai-api-complete-region (&optional b e)
  "Get a completion for the current region."
  (interactive "r")
  (let* ((prompt (buffer-substring-no-properties (region-beginning) (region-end)))
         (completions (openai-api-get-completions prompt)))
    (deactivate-mark)
    (goto-char e)
    (newline)
    (insert (completing-read "Choose completion:" completions))))

(provide 'openai-api)

;;; openai-api.el ends here
