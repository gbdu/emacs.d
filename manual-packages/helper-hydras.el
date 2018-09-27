;;; helper-hydras.el --- helpful hydras for many things
;;; outline
(defhydra hydra-outline (:color pink :columns 3 )
"
^Hide                                  ^Show              ^Move

_
"
    ;; Hide
    ("7" hide-sublevels "Hide everything but the top-level headings")
    ("4" hide-body     "Hide everything but headings (all body lines)")
    ("E" hide-entry    "Hide this entry's body")
    ("C" hide-other    "Hide other branches")
    ("B" hide-leaves   "Hide body lines in this entry and sub-entries")
    ("T" hide-subtree  "Hide everything in this entry and sub-entries")
    ;; Show
    ("a" show-all          " Show (expand) everything")
    ("b" show-branches     "Show all sub-headings under this heading")
    ("t" show-subtree      "Show (expand) everything in this heading & below")
    ("e" show-entry        "Show this heading's body")
    ("c" show-children     "Show this heading's immediate child sub-headings")

    ;; Move
    ("w" outline-backward-same-level "backwards")       ; Backward - same level
    ("W" outline-up-heading "Up")                ; Up
    ("d" outline-forward-same-level "forward")        ; Forward - same level
    ("D" outline-next-heading "next")
    ("q" nil "leave") )
  ;;  ("s" outline-next-visible-heading)      ; Next
  ;;  ("w" outline-previous-visible-heading)  ; Previous



;;; org utils 
(defhydra helper-hydra-org-utils (:color blue)
  ("1" my/copy-id-to-clipboard "Copy headline ID")
  ("c" org-capture "Capture note")
  ("I" my/org-add-ids-to-headlines-in-file "ID all headlines")
  ("T" org-agenda-show-tags-in-columns  "Agenda tags")
  ("j" org-adjust-region "Adjust list in region"))
;;; Org/agenda
(defhydra helper-hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

;;; Org/agenda 2
;; (defhydra hydra-org-agenda-view (:hint none) ;
;;     "
;;     _d_: ?d? day        _g_: time grid=?g? _a_: arch-trees
;;     _w_: ?w? week       _[_: inactive      _A_: arch-files
;;     _t_: ?t? fortnight  _f_: follow=?f?    _r_: report=?r?
;;     _m_: ?m? month      _e_: entry =?e?    _D_: diary=?D?
;;     _y_: ?y? year       _q_: quit          _L__l__c_: ?l?"
;;     ("SPC" org-agenda-reset-view)
;;     ("d" org-agenda-day-view
;;     (if (eq 'day (org-agenda-cts))
;;     "[x]" "[ ]"))
;;     ("w" org-agenda-week-view
;;     (if (eq 'week (org-agenda-cts))
;;     "[x]" "[ ]"))
;;     ("t" org-agenda-fortnight-view
;;     (if (eq 'fortnight (org-agenda-cts))
;;     "[x]" "[ ]"))
;;     ("m" org-agenda-month-view
;;     (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
;;     ("y" org-agenda-year-view
;;     (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
;;     ("l" org-agenda-log-mode
;;     (format "% -3S" org-agenda-show-log))
;;     ("L" (org-agenda-log-mode '(4)))
;;     ("c" (org-agenda-log-mode 'clockcheck))
;;     ("f" org-agenda-follow-mode
;;     (format "% -3S" org-agenda-follow-mode))
;;     ("a" org-agenda-archives-mode)
;;     ("A" (org-agenda-archives-mode 'files))
;;     ("r" org-agenda-clockreport-mode
;;     (format "% -3S" org-agenda-clockreport-mode))
;;     ("e" org-agenda-entry-text-mode
;;     (format "% -3S" org-agenda-entry-text-mode))
;;     ("g" org-agenda-toggle-time-grid
;;     (format "% -3S" org-agenda-use-time-grid))
;;     ("D" org-agenda-toggle-diary
;;     (format "% -3S" org-agenda-include-diary))
;;     ("!" org-agenda-toggle-deadlines)
;;     ("["
;;     (let ((org-agenda-include-inactive-timestamps t))
;;     (org-agenda-check-type t 'timeline 'agenda)
;;     (org-agenda-redo)))
;;     ("q" (message "Abort") :exit t))

;;; Transpose
(defhydra helper-hydra-transpose (:color red)
    "Transpose"
     ("c" transpose-chars "characters")
     ("w" transpose-words "words")
     ("o" org-transpose-words "Org mode words")
     ("l" transpose-lines "lines")
     ("s" transpose-sentences "sentences")
     ("e" org-transpose-elements "Org mode elements")
     ("p" transpose-paragraphs "paragraphs")
     ("t" org-table-transpose-table-at-point "Org mode table")
     ("q" nil "cancel" :color blue))
;;; Ediff
(defhydra helper-hydra-ediff (:color blue :hint nil)
  "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
                  _c_urrent file
"
  ("b" ediff-buffers)
  ("B" ediff-buffers3)
  ("=" ediff-files)
  ("f" ediff-files)
  ("F" ediff-files3)
  ("c" ediff-current-file)
  ("r" ediff-revision)
  ("l" ediff-regions-linewise)
  ("w" ediff-regions-wordwise))
;;; Dired
(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))


;;; Rectangle
(defhydra helper-hydra-rectangle (:hint nil
                                        :body-pre (rectangle-mark-mode 1)
                                        :color pink
                                        :post (deactivate-mark))
  "
  ^_k_^     _d_elete            _s_tring
_h_   _l_   _q_uit              _y_ank
  ^_j_^     _n_ew-copy          _r_eset
^^^^        _e_xchange          _u_ndo
^^^^        _o_pen              _p_aste
^^^^        _w_hitespaces
^^^^        _N_umber lines
^^^^        ^ ^
"
  ("h" rectangle-backward-char nil)
  ("l" rectangle-forward-char nil)
  ("o" open-rectangle)
  ("w" clear-rectangle)
  ("N" rectangle-number-lines)
  ("k" rectangle-previous-line nil)
  ("j" rectangle-next-line nil)
  ("e" hydra-ex-point-mark nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
("q" nil nil))
;;; Window
(defhydra helper-hydra-window (:color red
                        :hint nil)
  "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"

  ("left"  windmove-left)
  ("down" windmove-down)
  ("up" windmove-up)
  ("right" windmove-right)


  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("v" split-window-right)
  ("x" split-window-below)
  ;("t" transpose-frame "'")
  ;; winner-mode must be enabled
  ("u" winner-undo)
  ("r" winner-redo) ;;Fixme, not working?
  ("o" delete-other-windows :exit t)
  ("a" ace-window :exit t)
  ("f" new-frame :exit t)
  ("s" ace-swap-window)
  ("da" ace-delete-window)
  ("dw" delete-window)
  ("db" kill-this-buffer)
  ("df" delete-frame :exit t)
  ("q" nil)
  ;("i" ace-maximize-window "ace-one" :color blue)
  ;("b" ido-switch-buffer "buf")
  ("m" headlong-bookmark-jump))
;;; Yasnippet
(defhydra helper-hydra-yasnippet (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all))



;;; Macro
(defhydra helper-hydra-macro (:hint nil :color pink :pre 
				    (when defining-kbd-macro
                                      (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_w_^           [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_D_] delete     [_t_] set       [_k_] key bind  [_,_] last
 _a_ ←   → _d_       [_o_] edit       [_A_] add       [_x_] register
     ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
     ^_s_^           [_m_] step
    ^^   ^^          [_S_] swap
"
  ("a" kmacro-start-macro :color blue)
  ("d" kmacro-end-or-call-macro-repeat)
  ("w" kmacro-cycle-ring-previous)
  ("s" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("D" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("S" kmacro-swap-ring)
  ("n" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("A" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("k" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil :color blue))


;;; Fonts 
(defhydra helper-hydra-fonts (:hint none)
  "
1: Neep-10
2: Terminus-9
3: Lime-8
4: Fira Mono-9
5: Iosevka-10
6: Monaco-9 
7: Ubuntu Mono-11
8: Space Mono-10
"
  ("<right>" text-scale-increase "in")
  ("<left>" text-scale-decrease "out")
  ("1" (set-default-font "Neep-10")  "Neep")
  ("2" (set-default-font "Terminus-9") "terminus-9")
  ("3" (set-default-font "lime-8")  "lime")
  ("4" (set-default-font "Fira Mono Medium-9")  "Fira-9")
  ("5" (set-default-font "Iosevka-10:weight=Semibold") "Ioveska-10")
  ("6" (set-default-font "Monaco-9") "Monaco-9")
  ("7" (set-default-font "Ubuntu Mono-11") "Ubuntu Mono-10")
  ("8" (set-default-font "Space Mono-10") "Space Mono-10"))

;;; Provide
(provide 'helper-hydras) ;; helper-hydras.ends
