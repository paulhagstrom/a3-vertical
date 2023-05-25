a3wheel

A game for the Apple ///

Paul Hagstrom, July 2023

Another Apple /// game, kind of a tech demo, but focusing specifically on the thing
that lets you shift the vertical thingiewhatsit.

## Compilation ##

I use ca65 to compile this, and AppleCommander to get it onto a disk image.
My steps are:

```
ca65 a3wheel.s
ld65 -o a3wheel.bin -C apple3big.cfg a3wheel.o
ac -d a3wheel.po SOS.INTERP
ac -p a3wheel.po SOS.INTERP bin < a3wheel.bin
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
