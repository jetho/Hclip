HClip
=====

A small cross-platform library for reading and modifying the system clipboard.
HClip works on Windows, Mac OS X, and Linux (but see the requirements below!).


Requirements
------------

- Windows: No additional requirements.
- Mac OS X: Requires the pbcopy and pbpaste commands, which ship with Mac OS X.
- Linux: Requires xclip or xsel installed.


Example Usage
-------------

*System.HClip> setClipboard "Haskell"
Right "Haskell"
*System.HClip> getClipboard
Right "Haskell"
*System.HClip> modifyClipboard (reverse . map toUpper)
Right "LLEKSAH"

