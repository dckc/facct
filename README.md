dev notes

-- https://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program

 cabal: The program 'c2hs' version >=0.16 is required but it could not be
 found.
 "you should add the following line to your Cabal file:
 Build-tools: c2hs"
 no joy
<Welkin> hsc2hs?
<DanC_> what about it?
<Welkin> hsc2hs or c2hs?
 are they different?
<DanC_> you're asking me?
 `cabal install c2hs` seems to be working... but shouldn't that be triggered by the .cabal file somehow?
<mietek_> DanC_: ha
* DanC_ didn't realize he was making a joke
<mietek_> DanC_: you've ran into a limitation of Cabal which has been outstanding since at least 2008

<mietek_> DanC_: https://github.com/haskell/cabal/issues/220

Configuring gnome-keyring-0.3.1...
cabal: The pkg-config package 'gnome-keyring-1' is required but it could not
be found.

$ sudo apt-get install libgnome-keyring-dev
