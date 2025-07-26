;;; vala-mode.el --- Major mode for Vala programming language

(defvar my-vala-mode-hook nil)

(defun my-vala-mode-newline()
  "my-vala-modeで<C-j>を押した時の挙動。
単純に改行コードを挿入するだけ。インデントしない。"
  (interactive)
  (insert "\n"))

(defun my-vala-mode-newline-and-indent()
  "my-vala-modeで<C-m>を押した時の挙動。
改行コードを挿入し、前の行のインデントに揃える。"
  (interactive)
  (let ((prev-indent (current-indentation)))
    (insert "\n")
    (insert (make-string prev-indent ?\s))))

(defun my-vala-indent-right ()
  "字下げする。(インデントをタブ一個分加える)"
  (interactive)
  (let ((indent-str (make-string tab-width ?\s)))
    (if (use-region-p)
        ;;選択範囲がある場合
        (let* ((start (region-beginning))
               (end (region-end)))
          (save-mark-and-excursion
            ;; カーソルの位置を正規化する
            (goto-char start)
            (beginning-of-line)
            (setq start (point))
            (goto-char end)
            (beginning-of-line)
            (forward-line 1)
            (setq end (point))
            ;; 選択範囲の最初の行から
            (goto-char start)
            (beginning-of-line)
            (while (< (point) end)
              (insert indent-str)
              (setq end (+ end tab-width))
              (forward-line 1)))
          (setq deactivate-mark nil))
      ;; 選択範囲がない場合
      (if (not (bolp))
          ;; カーソルが行頭にある場合
          (insert (make-string tab-width ?\s))
        ;; カーソルが行頭以外にある場合
        (save-excursion
          (beginning-of-line)
          (insert (make-string tab-width ?\s)))))))

(defun my-vala-indent-left ()
  "字上げする。(インデントをタブ一個分減らす)"
  (interactive)
  (let ((spaces tab-width))
    (if (use-region-p)
        ;; 選択範囲がある場合
        (let ((start (region-beginning))
              (end (region-end)))
          (save-mark-and-excursion
            ;; カーソルの位置を正規化する
            (goto-char start)
            (beginning-of-line)
            (setq start (point))
            (goto-char end)
            (beginning-of-line)
            (forward-line 1)
            (setq end (point))
            ;; 選択範囲の最初の行から
            (goto-char start)
            ;; 1行ずつ字上げをしていく
            (while (< (point) end)
              ;; 空白以外の文字を削除しないように1文字ずつ削除していく
              (dotimes (i spaces)
                (when (looking-at " ")
                  (delete-char 1)
                  (setq end (- end 1))))
              (forward-line 1)))
          (setq deactivate-mark nil))
      (save-excursion
        (beginning-of-line)
        (dotimes (i spaces)
          (when (looking-at " ")
            (delete-char 1)))))))

(defvar my-vala-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-i") 'my-vala-indent-right)
    (define-key map (kbd "M-i") 'my-vala-indent-left)
    map)
  "Keymap for `my-vala-mode'.")

;; キーワード（C言語＋GObject風味）
(defconst vala-font-lock-keywords
  (list
  ;; キーワード
   (cons (regexp-opt
          '("if" "else" "switch" "case" "default" "do" "while" "for" "foreach" "in" "break"
            "continue" "return" "try" "catch" "finally" "throw" "lock" "class" "interface"
            "struct" "enum" "delegate" "errordomain" "const" "weak" "unowned" "dynamic"
            "callback" "begin" "end" "abstract" "virtual" "override" "signal" "extern"
            "static" "async" "inline" "new" "public" "private" "protected" "internal" "out"
            "ref" "throws" "requires" "ensures" "namespace" "using" "as" "is" "in" "new"
            "delete" "sizeof" "typeof" "this" "base" "get" "set" "construct" "default"
            "value" "connect" "construct" "static construct" "class construct" "var" "yield"
            "global" "owned" "with") 'words)
         font-lock-keyword-face)
   '("\\<[@A-Z_][A-Z0-9_]+\\>" . font-lock-constant-face)
   '("\\<[0-9]+\\>" . font-lock-constant-face)
   `(,(regexp-opt '("true" "false" "null" "void" "int" "uint"
                   "long" "string" "double" "int8" "uint8"
                   "int16" "uint16" "int32" "uint32" "int64"
                   "uint64" "float" "bool" "char" "uchar")
                 'symbols)
     . font-lock-builtin-face)
   '("\".*\"" . font-lock-string-face)
   '("'[^']*'" . font-lock-string-face)
   '("[][+-/*%=^~|{}()!&><;:,.?/@$]" . font-lock-builtin-face)
   ;; 型名（PascalCaseっぽい）
   '("\\<[A-Z][a-zA-Z0-9_]*\\>" . font-lock-type-face)
   ;; 関数定義
   '("\\_<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\_>[ \t]*(" 1 font-lock-function-name-face)))
   ;;'("\\<\\(\\w+\\)\\s-*(" 1 font-lock-function-name-face)

(defvar my-vala-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; 単語構成要素など最低限
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\" "\"" st)  ; 文字列
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defun my-vala-mode ()
  "Major mode for editing Vala code"
  (interactive)
  (kill-all-local-variables)
  (use-local-map my-vala-mode-map)
  (set-syntax-table my-vala-mode-syntax-table)
  (setq-local font-lock-defaults '(vala-font-lock-keywords))
  (setq major-mode 'my-vala-mode)
  (setq-local tab-width 4)
  (setq mode-name "Vala")
  (font-lock-mode 1)
  (local-set-key (kbd "C-m") 'my-vala-mode-newline-and-indent)
  (local-set-key (kbd "C-j") 'my-vala-mode-newline)
  (run-hooks 'my-vala-mode-hook))

;; 拡張子 .vala をこのモードに関連付け
(add-to-list 'auto-mode-alist '("\\.vala\\'" . my-vala-mode))

(provide 'my-vala-mode)
;;; my-vala-mode.el ends here
