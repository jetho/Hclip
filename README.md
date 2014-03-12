## Hclip

A small cross-platform library for reading and modifying the system clipboard.

Hclip works on Windows, Mac OS X and Linux (but see the requirements below!).


#### Requirements

* Windows: No additional requirements.
* Mac OS X: Requires the pbcopy and pbpaste commands, which ship with Mac OS X.
* Linux: Requires xclip or xsel installed.


#### Example Usage

 ```haskell
λ: setClipboard "Haskell"
"Haskell"
λ: getClipboard
"Haskell"
λ: modifyClipboard (reverse . map toUpper)
"LLEKSAH"
λ: :m Control.Exception
λ: try (modifyClipboard $ map toUpper. reverse) :: IO (Either ClipboardException String)
Right "HASKELL"
 ```


