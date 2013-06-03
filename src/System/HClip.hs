
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


type ErrorWithIO = ErrorT String IO
type IOHandles = (Handle, Handle)
type OSType = String

data CommandType = GetClipboard | SetClipboard String


stdin, stdout :: IOHandles -> Handle
stdin = fst
stdout = snd


externalCommand "darwin" GetClipboard = "pbcopy"
externalCommand "darwin" (SetClipboard _) = "pbpaste"

externalCommand "xsel" GetClipboard = "xsel -o"
externalCommand "xsel" (SetClipboard _) = "xsel -i"

externalCommand "xclip" GetClipboard = "xclip -selection c -o"
externalCommand "xclip" (SetClipboard _) = "xclip -selection c"


whichCommand :: String -> IO (Maybe String)
whichCommand cmd = do 
  (exitCode,_,_) <- readProcessWithExitCode "which" [cmd] "" 
  case exitCode of
    ExitSuccess -> return $ Just cmd
    ExitFailure _ -> return Nothing


getLinuxCommand :: ErrorWithIO String
getLinuxCommand = do
  results <- liftIO $ mapM whichCommand ["xsel", "xclip"]
  maybe (throwError "HClip requires xclip or xsel installed.")
        return
        (getFirst (mconcat $ map First results))


getExternalCommand :: OSType -> CommandType -> ErrorWithIO String
getExternalCommand "linux" commandType = getLinuxCommand >>= return . flip externalCommand commandType
getExternalCommand "darwin" commandType = return $ externalCommand "darwin" commandType
getExternalCommand unknownOS _ = throwError ("Unsupported OS: " ++ unknownOS)


withExternalCommand :: OSType -> CommandType -> IO (Either String String)
withExternalCommand osType commandType = runErrorT $ do
  cmd <- getExternalCommand osType commandType
  liftIO $ bracket (runInteractiveCommand cmd)
           (\(inp,outp,stderr,_) -> mapM_ hClose [inp,outp,stderr])
           (\(inp,outp,_,_) -> (action commandType) (inp, outp))
  where
    action GetClipboard = hGetContents . stdout
    action (SetClipboard text) = (flip hPutStr text >=> const (return text)) . stdin


winCommand GetClipboard = undefined
winCommand (SetClipboard _) = undefined


execCommand :: CommandType -> IO (Either String String)
execCommand command = 
  case os of
    "windows" -> winCommand command
    osType -> withExternalCommand osType command


getClipboard :: IO (Either String String)
getClipboard = execCommand GetClipboard


setClipboard :: String -> IO (Either String String)
setClipboard = execCommand . SetClipboard 


modifyClipboard :: (String -> String) -> IO (Either String String) 
modifyClipboard = flip (liftM . liftM) getClipboard >=> either (return . Left) setClipboard

