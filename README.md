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

## 作ってみた感想
いやぁー良い時代というか凄い時代になったものですなぁ。  
今やAIが大抵のコードは書いてくれるんだから。  
Emacsのメジャーモードの作り方は、公式ドキュメントを見ても全然わからなかったから、AIなしでは書けなかったよ。  
そりゃコーダーなんか要らなくなるよなと思う。失業待ったなし。  
でも使う分にはめちゃめちゃ良いな。  
特に書いていて面白くない言語、VBAとかPerlとかPowerShellとかPythonみたいな言語は1行ごとにググってた手間を省略できるから、ほんと楽になる。  
可能性の幅が広がり素晴しいことです。
