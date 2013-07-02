
--------------------------------------------------------------------
-- |
-- Module : System.HClip
-- Copyright : (c) Jens Thomas
-- License : BSD3
--
-- Maintainer: Jens Thomas <jetho@gmx.de>
-- Stability : experimental
-- Portability: 
--
-- A small cross-platform library for reading and modifying the 
-- system clipboard. 
-- 
--------------------------------------------------------------------

module System.HClip (
        getClipboard, 
        setClipboard, 
        modifyClipboard
  ) where

import System.Process (runInteractiveCommand, readProcessWithExitCode) 
import System.Info (os)
import System.IO (Handle, hPutStr, hClose)
import Data.Monoid 
import Control.Exception (bracket)
import System.IO.Strict (hGetContents) -- see http://hackage.haskell.org/package/strict
import System.Exit 
import Control.Monad.Error
import Data.List (intercalate)


type ErrorWithIO = ErrorT String IO

-- Clipboard Actions
data Command = GetClipboard | SetClipboard String 

-- Supported Operating Systems
data Linux = Linux deriving (Show)
data Darwin = Darwin deriving (Show)
data Windows = Windows deriving (Show)


-- type class for supported operating systems
class SupportedOS a where
  clipboard :: a -> Command -> IO (Either String String)

-- read clipboard contents
getClipboard :: IO (Either String String)
getClipboard = dispatchCommand GetClipboard

-- set clipboard contents
setClipboard :: String -> IO (Either String String)
setClipboard = dispatchCommand . SetClipboard

-- apply function to clipboard and return the new contents
modifyClipboard :: (String -> String) -> IO (Either String String)
modifyClipboard = flip (liftM . liftM) getClipboard >=> either (return . Left) setClipboard

-- select the supported operating system
dispatchCommand :: Command -> IO (Either String String)
dispatchCommand = case os of
  "mingw32" -> clipboard Windows 
  "linux" -> clipboard Linux
  "darwin" -> clipboard Darwin
  unknownOS -> const $ return . Left $ "Unsupported OS: " ++ unknownOS

-- Mac OS    
instance SupportedOS Darwin where
  clipboard Darwin GetClipboard = runErrorT $ withExternalCommand "pbcopy" GetClipboard    
  clipboard Darwin c@(SetClipboard s) = runErrorT $ withExternalCommand "pbpaste" c

-- Linux support
instance SupportedOS Linux where
  clipboard Linux command = runErrorT $ do
    prog <- chooseFirstCommand ["xsel", "xclip"]
    withExternalCommand (decode prog command) command
    where
      decode "xsel" GetClipboard = "xsel -o"
      decode "xsel" (SetClipboard _) = "xsel -i"
      decode "xclip" GetClipboard = "xclip -selection c -o"
      decode "xclip" (SetClipboard _) = "xclip -selection c"
    
-- TODO: Windows support
instance SupportedOS Windows where
  clipboard Windows GetClipboard = undefined
  clipboard Windows (SetClipboard s) = undefined


-- run external command for accessing the system clipboard
withExternalCommand :: String -> Command -> ErrorWithIO String
withExternalCommand prog command = 
  liftIO $ bracket (runInteractiveCommand prog)
                   (\(inp,outp,stderr,_) -> mapM_ hClose [inp,outp,stderr])
                   (\(inp,outp,_,_) -> (action command) (inp, outp))
  where
    action GetClipboard = hGetContents . stdout
    action (SetClipboard text) = (flip hPutStr text >=> const (return text)) . stdin
    stdin = fst
    stdout = snd


-- search for installed programs and return the first match
chooseFirstCommand :: [String] -> ErrorWithIO String
chooseFirstCommand cmds = do
  results <- liftIO $ mapM whichCommand cmds
  maybe (throwError $ "HClip requires " ++ apps ++ " installed.")
        return
        (getFirst (mconcat $ map First results))
  where apps = intercalate " or " cmds


-- use the which-command to check if cmd is installed
whichCommand :: String -> IO (Maybe String)
whichCommand cmd = do
  (exitCode,_,_) <- readProcessWithExitCode "which" [cmd] ""
  case exitCode of
    ExitSuccess -> return $ Just cmd
    ExitFailure _ -> return Nothing

