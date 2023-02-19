;; an attempt at making an HTML canvas where org-mode notes can be laid out, like Obsidian Canvas
;; copyright by Charl P. Botha <http://charlotha.com/> under the BSD 3-clause

;; notes
;; - ielm-change-working-buffer
;; - namespace with prefix hypen or double hyphen: https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html#Coding-Conventions
;; - org-id-open e.g. (org-id-open "a7e901be-1325-433e-a397-449e32842a86" nil)

;; https://orgmode.org/worg/dev/org-element-api.html
;; "org-element-contents returns an ordered (by buffer position) list of all
;; elements or objects within a given element or object. Since local parsing
;; ignores contents, it only makes sense to use this function on a part of an
;; AST."

(require 'simple-httpd)

(defun omc--to-html (s)
  "Convert string S from orgmode to HTML."
  (org-export-string-as s 'html 't)
  )

;; thanks GPT-enabled bing!
(defun omc--extract-body (html)
  "Extract the part between <body> and </body> tags from HTML."
  (let ((start (search "<body>" html))
        (end (search "</body>" html)))
    (substring html (+ start 6) end)))

;; given an id, return that node (could be a heading = title, or a file with #+TITLE) as HTML
;; example of headline inside of other file (2020-11-Nov)
;; (omc--org-id-to-html "7b2de849-2a77-4e5e-bff2-6857fc9091f1")
;; TODO: extract title
(defun omc--org-id-to-html (id)
  "Find an org file or heading with ID and return its HTML representation."
  (let ((fnpos (org-id-find id)))
    (unless fnpos
      (error "Cannot find entry with ID \"%s\"" id))
    (with-temp-buffer
      (insert-file-contents (car fnpos))
      (goto-char (cdr fnpos))
      ;; if heading then export subtree only (SUBTREEP), else export buffer
      ;; NOTE: body only unfortunately excludes rendering title in either mode, durnit!
      ;;       (note that without body only <body> does contain rendering of the title)
      ;;       so we just do the whole thing, then take everything between the two tags
      (omc--extract-body (org-export-as 'html (org-at-heading-p) nil nil)))))

;; example paragraph element with only text, following (TYPE PROPERTIES CONTENTS):
;; (paragraph
;;   (:begin 297 :end 325 :contents-begin 297 :contents-end 325 :post-blank 0 :post-affiliated 297 :mode nil :granularity nil :parent #48)
;;  #("I can't make an empty title" 0 28 (:parent #53)))

(defun omc-parse-buffer ()
  "parse the canvas buffer and spit out json"
  (let ((parsed (org-element-parse-buffer)))
    ;; for each headline
    (org-element-map parsed 'headline
      (lambda (hl)
        ;; if headline has a canvas_coords, get out title and section (body)
        (when-let* ((coords (org-element-property :CANVAS_COORDS hl))
                    ;; :title gives you the #("bleh" M N ....) propertized string
                    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Text-Props-and-Strings.html
                    ;; you could also do:
                    ;; (title (org-element-interpret-data
     		    ;;   (org-element-property :title headline)))
                    (title (org-element-property :raw-value hl))
                    ;; each hl by def has a single section up to the next HL
                    (section (org-element-map hl 'section #'identity nil t)))
  `((title . ,title)
    (coords . ,(mapcar #'string-to-number (split-string coords)))
    (body .
          (
           ;; for each section we want something like:
           ;; {"title": "bleh", "coords": [200, 100], "body": [<html>, <link>]}
           ;; within section, get out paragraphs
           ;; each paragraph can have multiple children: link, bold,
           ;; straight text (looks like special object type is 'plain-text)
           ;; -- search org-element.el for more
           ;; first pass of section to get paragraphs -- we convert the whole list to one chunk of HTML
           ;; you can't just interpret the list of paragraphs: only top-level org-data or element
           ;;,(omc--to-html (org-element-interpret-data (org-element-map section 'paragraph #'identity)))
           ,(omc--to-html (org-element-interpret-data section))

          ;; second pass of section
          ;; to get out the first link of type ID for transclusion
          ,(org-element-map section 'link
             (lambda (lnk)
                   (and (string= (org-element-property :type lnk) "id")
                        (org-element-property :path lnk)))
             nil t)
          ) ;; end of body list
          )) ;; end of object
          ) ;; end of when-let
        ))))


;; can't use defservlet here because we need current-buffer
(defun httpd/canvas (proc path &rest args)
  ;; serve current canvas.org file

  (message "==> my buffer %s" canvas-buffer)


  ;; MATCH t -- all headlines (tags/property/todo match)
  ;; SCOPE nil -- current buffer, respecting restriction
  (with-current-buffer canvas-buffer
    ;; (with-httpd-buffer proc "text/json"
    ;;   (insert "BLEH BLEH"))
    )
  


  )

;;;###autoload
(define-minor-mode
  org-canvas-mode
  "Enable org-mode-canvas.
This serves the web-build and API over HTTP."
  :lighter " org-canvas "
  ;; buffer-local thanks
  :global nil
  :group 'org-canvas
  :init-value nil
  (cond
   (org-canvas-mode
    ;(setq-local httpd-port org-roam-ui-port)
    ;(setq httpd-root org-roam-ui-app-build-dir)
    (httpd-start)
    (message "===> activate org-canvas-mode on %s" (current-buffer))
    ;; no idea how this will work for httpd servlets
    (setq-local canvas-buffer (current-buffer))
    
    )
   (t
    (progn
      
      (httpd-stop)
      
      ))))



;; default servlet httpd/ will serve files from httpd-root -- we would like to keep that


;; SCRATCH AREA below this line ---------------------------------------

(defun get-headline-with-text ()
  (save-excursion
    (save-restriction
      (widen)
      (ignore-errors (outline-up-heading 1))
      (let* ((elt (org-element-at-point-no-context))
             (title (org-element-property :title elt))
             (beg (progn (org-end-of-meta-data t) (point)))
             (end (progn (outline-next-visible-heading 1) (point))))
        (list title (buffer-substring-no-properties beg end))))))

;; this json string
(json-read-from-string "{\"title\": \"hello there\", \"coords\": [200,100]   }")
;; gives this object:
;; ((title . "hello there") (coords . [200 100]))

