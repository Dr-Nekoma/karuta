(setq *mailbox* nil)

(defun karuta-handler (process response)
  ""
  ;; (setq *mailbox* (json-read-from-string response))
  (setq *mailbox* response)
  (let ((response (process-get process :response)))
    (message "Received: %s" response)    
    (delete-process process)))

(setq karuta-address "localhost")
(setq karuta-port 7632)

(defun karuta-client (content)
  ""
  (let ((connection (open-network-stream "karuta" "*karuta-socket*" karuta-address karuta-port)))
    (process-put connection :response nil)
    (set-process-filter connection 'karuta-handler)
    (process-send-string connection content)))

(setq msg "test")

(karuta-client msg)

(with-current-buffer "sexp"
  (erase-buffer)
  (goto-char (point-min))
  (insert *mailbox*)
  (emacs-lisp-mode)
  (pp-buffer))
