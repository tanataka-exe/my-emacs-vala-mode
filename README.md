# my-emacs-vala-mode
EmacsでValaを使うメジャーモードを自分で作ってみた（ChatGPTベース）

## 特徴
特に何の機能もないまっさらな編集モード。

* キーワード強調。
* `C-i`で字下げ。(タブ1文字分。ただし空白に変換する。)
* `M-i`で字上げ。(タブ1文字分。ただし空白に変換する。)
* 改行字は前の行のインデントに揃える。勝手に字上げ、字下げしない。

それだけ。

## インストール方法
1. Gitリポジトリをクローンする。
2. `my-vala-mode.el`を`~/.emacs.d/lisp`にコピーする。
3. `init.el`に`(add-to-list 'load-path "~/.emacs.d/lisp")`を書き込む。
4. `init.el`に`(require 'my-vala-mode)`を書き込む。
5. Emacsを再起動、または`init.el`をリロード (`eval-buffer`)


