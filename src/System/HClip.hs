
module System.HClip (getClipboard, setClipboard, modifyClipboard) where

import System.Process
import System.Info (os)
import System.IO (Handle, hPutStr, hClose)
import Control.Monad
import Data.Monoid
import Control.Exception
import System.IO.Strict (hGetContents) -- see http://hackage.haskell.org/package/strict
import System.Exit
import Control.Monad.Error
import Data.List (intercalate)


type ErrorWithIO = ErrorT String IO

data Command = GetClipboard | SetClipboard String 


data Linux = Linux deriving (Show)
data Darwin = Darwin deriving (Show)
data Windows = Windows deriving (Show)


class SupportedOS a where
  clipboard :: a -> Command -> IO (Either String String)
    

instance SupportedOS Darwin where
  clipboard Darwin GetClipboard = runErrorT $ withExternalCommand "pbcopy" GetClipboard    
  clipboard Darwin c@(SetClipboard s) = runErrorT $ withExternalCommand "pbpaste" c


instance SupportedOS Linux where
  clipboard Linux command = runErrorT $ do
    prog <- chooseFirstCommand ["xsel", "xclip"]
    withExternalCommand (decode prog command) command
    where
      decode "xsel" GetClipboard = "xsel -o"
      decode "xsel" (SetClipboard _) = "xsel -i"
      decode "xclip" GetClipboard = "xclip -selection c -o"
      decode "xclip" (SetClipboard _) = "xclip -selection c"
    

instance SupportedOS Windows where
  clipboard Windows GetClipboard = undefined
  clipboard Windows (SetClipboard s) = undefined


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


chooseFirstCommand :: [String] -> ErrorWithIO String
chooseFirstCommand cmds = do
  results <- liftIO $ mapM whichCommand cmds
  maybe (throwError $ "HClip requires " ++ apps ++ " installed.")
        return
        (getFirst (mconcat $ map First results))
  where apps = intercalate " or " cmds


whichCommand :: String -> IO (Maybe String)
whichCommand cmd = do
  (exitCode,_,_) <- readProcessWithExitCode "which" [cmd] ""
  case exitCode of
    ExitSuccess -> return $ Just cmd
    ExitFailure _ -> return Nothing


dispatchCommand = case os of
  "mingw32" -> clipboard Windows 
  "linux" -> clipboard Linux
  "darwin" -> clipboard Darwin


getClipboard :: IO (Either String String)
getClipboard = dispatchCommand GetClipboard

setClipboard :: String -> IO (Either String String)
setClipboard = dispatchCommand . SetClipboard

modifyClipboard :: (String -> String) -> IO (Either String String)
modifyClipboard = flip (liftM . liftM) getClipboard >=> either (return . Left) setClipboard

