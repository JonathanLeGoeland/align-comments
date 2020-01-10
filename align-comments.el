;; This define the pattern identifying a comment start.
;; This is a minor mode that helps to keep comment tidy.
;; It is targeted at C and C++ programming.
;; To enable the mode : M-x ac-mode
;; To automatically enable the mode :
;; Add the folder where you downloaded align-comments.el to the load-path:
;; (add-to-list 'load-path "<folder>")
;; Load the module :
;; (load "align-comments")
;; Add ac-mode to the hook for C/C++ mode to
;; (add-hook 'c-mode-common-hook 'ac-mode)
;;
;;
;; Example 1:
;; // Write your comment
;; C-c l
;; Example 2:
;; /*|Write your comment|*/
;; C-c f
;; Example 3:
;; int a; // A's variable.
;; unsigned short b; // B.s variable.
;; Select the two lines and C-c a
;;
;; The nice thing is that you can reformat. For instance in a framed
;; comment, you can add a new line: |New line| and strike C-c f.  And
;; the framed comment will be updated.
;;
;; Happy coding.
;;

(defvar ac-comment-start "//"
  "End of line comment (C++ style).")
(defvar ac-os-comment-start "/*"
  "Start of block comment (old style, c style).")
(defvar ac-os-comment-end "*/"
  "End of block comment (old style, c style).")

(defvar ac-test nil
  "t if we want to perform the tests while evaluating the file.")

;;; ----------------------------------------------------------------
;;; Align comments

;; This function founds the outer most comment start/end.
;; That is to say, it founds the right most comment start/end.
(defun ac-give-outer-most-ac-comment (beginning end comment-sign)
  (goto-char beginning)
  (let ((max-col 0))
    (while (and (< (point) end)
                (search-forward comment-sign end t))
      (message "Current column is: %d." (current-column))
      (if (> (current-column) max-col)
          (setq max-col (current-column)))
      )
    (- max-col (length comment-sign))))

;; This function aligns all comment starts to the right most side.
(defun ac-move-comments-to-col (beginning end max-col comment-sign)
  (goto-char beginning)
  (while (and (< (point) end)
              (search-forward comment-sign end t))
    ;; The end if comment start is found
    (goto-char (match-beginning 0))
    (dotimes (i (- max-col (current-column)))
      (insert " "))
    (end-of-line)))

(defun ac-move-comments-to-col-no-limits (beginning end max-col comment-sign)
  (goto-char beginning)
  (while (<  (progn (beginning-of-line)  (point)) end )
    ;; Do something
    (if (search-forward comment-sign (save-excursion (end-of-line)))
        (progn
          (backward-char 2)
          (if (< (current-column) max-col)
              (insert (make-string (- max-col (current-column)) ?\s)))
          ;; Try to remove white characters
          (while (and (> (current-column) max-col)
                      (search-backward " "))
            (delete-char 1))
          ))
    (forward-line)
    ))

(defun ac-align-start-comment (beginning end)
  "Allign C++ style and C style comments in selected region."
  (interactive "r")
  (save-excursion
    (goto-char beginning)
    (let ((outer-col-1 0)
          (outer-col-2 0)
          (outer-col 0))
      ;; 1 -- Found max column lenght of the outer most comment.
      (setq outer-col-1 (ac-give-outer-most-ac-comment beginning end ac-comment-start))
      (setq outer-col-2 (ac-give-outer-most-ac-comment beginning end ac-os-comment-start))
      (setq outer-col (max outer-col-1 outer-col-2))
      (message "Max col is %d." outer-col)
      ;; 2 -- Allign all the foound start comment on this value.
      (ac-move-comments-to-col beginning end outer-col ac-comment-start)
      (ac-move-comments-to-col beginning end outer-col ac-os-comment-start)
      ;; 3 -- In necessary aligne the closing c-style comment.
      (ac-move-comments-to-col-no-limits
       beginning
       end
       (- fill-column 2)
       ac-os-comment-end))))

;;; ----------------------------------------------------------------
;;; framed text
;;; These functions allow to create a frame arround comment

;; Build framed text-property-default-nonsticky
(defun ac-write-one-line ()
  (move-beginning-of-line nil)
  (insert "/*")
  (dotimes (i (- fill-column 4)) (insert "-"))
  (insert "*/")
  )

 (defun frame-text ()
  (interactive)
  "This function does:
  - Create a frame arround the comment
  - Center the comment.
Constraints:
  - The comment must be surronded by || (pipes).
  - The function must be called when the point is between the pipes.
"
  (save-excursion
    ;; Find limits of box
    (let ((min (search-backward ac-os-comment-start nil t))
          (min-col (current-column))
          (max (search-forward ac-os-comment-end nil t)))
      ;; insert the upper line
      (goto-char (+ 2 min))
      (insert (concat
               (make-string  (- fill-column (current-column) (length ac-os-comment-start)) ?-))
              "+\n ")

      ;; Center the line
      (let ((min-txt (search-forward "|" nil))
            (max-txt (search-forward "|" nil))
            (mid-space ))
        (setq mid-space (/ (- fill-column (- max-txt min-txt) 3 min-col) 2))
        (goto-char min-txt)
        (backward-char 1)
        (dotimes (i min-col) (insert " "))
        (forward-char 1)
        (dotimes (i mid-space) (insert " "))
        (search-forward "|" nil t)
        (backward-char 1)
        (dotimes (i (- fill-column (current-column) 2)) (insert " "))
        )
      ;; insert the lower line
      (forward-char 1)
      (insert "\n")
      (insert (concat
               (make-string (+ min-col 1) ?\s)
               "+"
               (make-string (- fill-column (current-column)
                                  2
                                  (length ac-os-comment-end)
                                  min-col)
                            ?-)
               ))
      )))

(defun ac-make-frame-top (start-col end-col)
  (let ((width (- end-col start-col)))
    (concat
     (make-string start-col ?\s)
     "/*"
     (make-string (- width 3) ?-)
     "+\n")))

(defun ac-make-frame-bottom (start-col end-col)
  (let ((width (- end-col start-col)))
    (concat
     (make-string start-col ?\s)
     " +"
     (make-string (- width 3) ?-)
     "*/")))

(defun make-sub-framed-line (comment start-col end-col)
  (let ((width (- end-col start-col )))
    ;; Since we will devide
    (if (evenp (- width (length comment)) )
        (setq comment (concat comment " ")))
    (concat
     ;; Spaces * start-col
     (make-string start-col ?\s)
     ;; |
     " |"
     ;; Spaces * (width - comment-length - 2)/2
     (make-string (/ (- width (length comment) 2) 2) ?\s)
     ;; comment
     comment
     ;; Spaces * (width - comment-length - 2)/2
     (make-string (/ (- width (length comment) 2) 2) ?\s)
     ;; | + new line
     "|\n"
     )))

(defun ac-frame-text ()
  (interactive)
  (let ((origin-pos (point)) )
    (save-excursion
      (let (comment
            one-line-comment
            (end-pos (search-forward ac-os-comment-end nil t))
            (end-col (current-column))
            (start-pos (search-backward ac-os-comment-start nil t))
            (start-col (current-column))
            )
        ;; Get the text inside /* and */
        (setq comment (buffer-substring (+ start-pos (length ac-os-comment-start))
                                        end-pos))
        ;; Erase the comment
        (kill-region start-pos end-pos)

        ;; Prepare top line /*--- (width - 3)  ---+
        (beginning-of-line)
        (insert (ac-make-frame-top start-col fill-column))
        (while (string-match "\|\s*" comment)
          ;; Get the text after |  .
          (if (string-match "\|\s*" comment)
              (setq comment (substring comment (match-end 0)))
            error "Framed comment shall start by |.")
          ;; Get the text before    |.
          (if (string-match "\s*\|" comment)
              (progn
                (setq one-line-comment (substring comment 0 (match-beginning 0)))
                (setq comment (substring comment (match-end 0) (length comment))))
            error "Framed comment shall end by |.")
          ;; Prepare comment line
          (insert (make-sub-framed-line one-line-comment start-col fill-column))

          )
        ;; Prepare bottom line +-- (width - 3) --*/
        (insert (ac-make-frame-bottom start-col fill-column))))
    (goto-char origin-pos)))


;;; ----------------------------------------------------------------
;;; Line separator:
;;; Create a line separator comment and maintain it.

(defun ac-build-nice-comment-line (comment start-column)
  (let ((start "//")
        (column ":")
        (trailer))
    (setq trailer (make-string
                   (- fill-column
                      (+ (length comment) (length start) (length column))
                      start-column)
                   ?-))
    (concat start
            comment
            trailer
            column))
  )

(defun ac-comment-line-separator ()
  "Creates a line separator comment and maintain it.
You first need to write a comment like this: // <comment>
Then call `ac-comment-line-separator` when the point is on the line.
The comment becomes // <comment>------ : (It reaches `fill-column`)

The function maintains the comment, means that if you modified the comment
and call `ac-comment-line-separator` again the number of filling characters is
adjusted.
"
  (interactive)
  (let ((cur-col (current-column)))
    (save-excursion
      ;; Extract text from the comment
      (let ((max-point (progn (end-of-line) (point)) )
            (min-point (search-backward ac-comment-start)))
        (if (search-forward-regexp "//\\([^-]+\\)\\(\\-+:$\\)?" max-point t)
            (progn ;; Format nicely
              (let ((comment (match-string 1)))
                (kill-region min-point max-point)
                (insert (ac-build-nice-comment-line comment (current-column))))))))
    ;; Move back to previous position (if possible)
    (move-to-column cur-col)))

;;; //\\(.+\\)\\(-+:\\)?$
;;; Tests
(if ac-test
    (progn
      (switch-to-buffer-other-window "*test*")
      (erase-buffer)
      ;; Test aligne commtne
      (progn
        ;; Write test text:
        (insert "int short; // Coment one.\n")
        (insert "unsigned int long_declaration; // With another comment.\n")
        (insert "char a; /* C style comment. */")
        (insert "\n")
        ;; set region and call align-comment
        (ac-align-start-comment (point-min) (point))
        (goto-char (point-max))
        )
      ;; Test frame text
      (progn
        ;; First test :
        (insert "/*|This text will be framed.|*/")
        (backward-char 3)
        (ac-frame-text)
        ;; Second test :
        (goto-char (point-max))
        (insert "\n/*|This text will be framed_.|*/")
        (backward-char 3)
        (ac-frame-text)
        ;; Second test :
        (goto-char (point-max))
        (insert "\n    /*|This text will be framed .|*/")
        (backward-char 3)
        (ac-frame-text)
        ;; Third test :
        (goto-char (point-max))
        (insert "\n    /*|First line.||Second line.|*/")
        (backward-char 3)
        (ac-frame-text)
        )

      ;; Test ac-comment-line-separator
      (progn
        (goto-char (point-max))
        (insert "\n//test none.")
        (ac-comment-line-separator)
        (goto-char (point-max))
        (insert "\n    //test one.")
        (ac-comment-line-separator)
        (goto-char (point-max))

        (insert "\n// 1 ** test one.")
        (ac-comment-line-separator)
        (goto-char (point-max))

        (insert "\n    // 2 ** It shall be indented! --:")
        (ac-comment-line-separator)
        (goto-char (point-max))

        )
      ;; Go back to the original buffer
      (other-window -1)))

;;; ------------------------------------------------------------------
;;; Roead map (All done for the moment).
;;; Some interesting things to do:

;;; Align comment :

;;; Framed-commend:
;; - Centering optional

;;; Comment line separator:

;;; All:

;; Make it a minor mode. See https://nullprogram.com/blog/2013/02/06/
(define-minor-mode ac-mode
  "Align comments mode."
  :lighter " ac"
  :keymap (let ((map (make-sparse-keymap)))
            ;; Align comment
            (define-key map (kbd "C-c a") 'ac-align-start-comment)
            ;; Ligne separator
            (define-key map (kbd "C-c l") 'ac-comment-line-separator)
            ;; Framed comment
            (define-key map (kbd "C-c f") 'ac-frame-text)
            map))

(provide 'align-comments)
