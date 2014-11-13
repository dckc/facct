# facct - personal accounting tools using fp

I use a combination of GnuCash and a google docs spreadsheet for budgeting,
with a script, [budget_sync.py](https://bitbucket.org/DanC/quacken/src/tip/budget_sync.py?at=default),
to keep them in sync.

I'm interested to put a web interface on it.

I'm also interested in more reliable alternatives to python (cf.
[py2scala](https://bitbucket.org/DanC/py2scala)).

The haskell scotty stuff looks pretty straightforward. I'm now somewhat proficient in reading
haskell code, but whenever I sit down to write any, I'm defeated by tools or lack of libraries
or version skew or something.

Tonight's experiment was to see if I could get access to my GnuCash mysql database
using credentials from the gnome keyring as I do in `budget_sync.py`.

It works!


## dev notes ##

https://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program

cabal: The program 'c2hs' version >=0.16 is required but it could not be
found.

"you should add the following line to your Cabal file:

Build-tools: c2hs"

no joy

`cabal install c2hs` seems to be working... but shouldn't that be triggered by the .cabal file somehow?

<mietek_> DanC_: you've ran into a limitation of Cabal which has been outstanding since at least 2008
<mietek_> DanC_: https://github.com/haskell/cabal/issues/220

Configuring gnome-keyring-0.3.1...
cabal: The pkg-config package 'gnome-keyring-1' is required but it could not
be found.

$ sudo apt-get install libgnome-keyring-dev

TODO:

<merijn> DanC_: Look into ExceptT
<merijn> From transformers

<DanC_> whimper. no examples. http://hackage.haskell.org/package/transformers-0.4.1.0/docs/Control-Monad-Trans-Except.html

### IntelliJ IDEA haskell plugin works pretty well ###

intellij lost its marbles when I tried to rename a module

intellij takes 8 seconds to compile this little bit of code. `cabal build` is more like 3 seconds.
