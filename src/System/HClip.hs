
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

data CommandType = GetClipboard | SetClipboard


stdin, stdout :: IOHandles -> Handle
stdin = fst
stdout = snd


command "darwin" GetClipboard = "pbcopy"
command "darwin" SetClipboard = "pbpaste"

command "xsel" GetClipboard = "xsel -o"
command "xsel" SetClipboard = "xsel -i"

command "xclip" GetClipboard = "xclip -selection c -o"
command "xclip" SetClipboard = "xclip -selection c"


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


chooseOSCommand :: CommandType -> ErrorWithIO String
chooseOSCommand commandType = 
  case os of 
    "linux" -> getLinuxCommand >>= return . (flip command commandType)
    "darwin" -> return (command "darwin" commandType)
    unknownOS -> throwError ("Unsupported OS: " ++ unknownOS)


withCommand :: CommandType -> (IOHandles -> IO a) -> IO (Either String a)
withCommand commandType action = runErrorT $ do
  cmd <- chooseOSCommand commandType
  liftIO $ bracket (runInteractiveCommand cmd)
           (\(stdin,stdout,stderr,_) -> mapM_ hClose [stdin,stdout,stderr])
           (\(stdin,stdout,_,_) -> action (stdin, stdout))


getClipboard :: IO (Either String String)
getClipboard = withCommand GetClipboard $ hGetContents . stdout
  --where stdout = snd


setClipboard :: String -> IO (Either String ())
setClipboard text = withCommand SetClipboard $ flip hPutStr text . stdin
  --where stdin = fst


modifyClipboard :: (String -> String) -> IO (Either String ()) 
modifyClipboard = flip (liftM . liftM) getClipboard >=> either (return . Left) setClipboard

