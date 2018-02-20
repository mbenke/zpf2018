## Materiały do wykładu "Zaawansowane Programowanie Funkcyjne" Wydział MIM UW 2017/18

## "Advanced Functional Programming" course materials  (in Polish)

* Gotowe notatki/slajdy w katalogu www
* Kod w katalogu Code
* Do wygenerowania notatek i slajdów ze źródeł potrzebny program pandoc

### Szybki start

~~~~~
$ cabal update
$ cabal install pandoc
$ PATH=~/.cabal/bin:$PATH            # Linux
$ PATH=~/Library/Haskell/bin:$PATH   # OS X
$ git clone git://github.com/mbenke/zpf2017.git
$ cd zpf2018/Slides
$ make
~~~~~

albo, przy użyciu stack

~~~~
stack setup
stack install pandoc
export PATH=$(stack path --local-bin):$PATH
...
~~~~