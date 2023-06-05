a3-vertical

Vertical scrolling games for the Apple ///

Paul Hagstrom, July 2023

A couple of work in progress games to be described at KFest 2023.

Lazy River

boat on a river

Logdrive

a lot of logs on a river

spyhunter?

parallax?


relics

old stuff that hasn't been cleaned up yet

## Compilation ##

I use ca65 to compile this, and AppleCommander to get it onto a disk image.
My steps are:

```
ca65 a3wheel.s
ld65 -o a3wheel.bin -C apple3big.cfg a3wheel.o
ac -d a3wheel.po SOS.INTERP
ac -p a3wheel.po SOS.INTERP bin < a3wheel.bin
```

I use Applecommander, requires java, but brew install might be sufficient

See https://github.com/lifepillar/homebrew-appleii
and https://applecommander.github.io/install/


```
brew tap lifepillar/appleii
brew install applecommander-ac
alias ac='/opt/homebrew/Cellar/applecommander-ac/1.8.0/bin/ac'
```

test install with:

```
ac -h
```



## Play ##

Whee

## Keys ##

Whee

## Development notes ##



## Bugs ##

Bugs I am presently aware of.

- Nothing is written. Works perfectly so far.

## Enhancements ##

Things I have on the list of things to try to do:

- Write the thing
