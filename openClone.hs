-------------------------------------------------------------------------------
-- openClone.hs
-- Author: David Wells <drwells at default Virginia Tech email>
-- Description: A clone of the 'open' utility from NeXT
-------------------------------------------------------------------------------

import System (getArgs)
import System.Process (runCommand)
import System.Posix.Env (getEnv)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map (Map, lookup, fromList)

data FlagOption = Default | Application | Editor | Help

-- process a list of commands to run by mapping runCommand (creates a process)
-- over the list.
main = getArgs >>= mapM_ runCommand . attachCommands . findFlag

-- work with all the input values (except flags) and return a list of strings
-- to pass to /bin/sh
attachCommands :: Either String (FlagOption, [String]) -> [String]
attachCommands input = case input of
  Right (flagValue, inputArgs) -> attachApplications (flagValue, inputArgs)
-- if given an error message, return it to stderr.
  Left errorMessage -> error errorMessage

attachApplications :: (FlagOption, [String]) -> [String]
attachApplications (flagValue, inputArgs) = case flagValue of
-- in the default case, look up the application associated with the first file.
  Default     -> case Map.lookup (suffix (head inputArgs)) applicationTable of
    Just app  -> prependApplication app inputArgs
    Nothing   -> error "application not defined"
-- if the flag is for an application, that application is the first argument.
  Application -> prependApplication (head inputArgs ++ " ") (" " : tail inputArgs)
-- if the flag is for the editor, launch it with the editor variable.
  Editor      -> [editorName ++ " " ++ (unwords inputArgs)]
-- if the flag is for help, echo the help message.
  Help        -> ["echo " ++ "'" ++ usageMessage ++ "'"]
-- function to prefix files with the appropriate application.
  where prependApplication app files = map ((app ++ " ") ++) files
        suffix = reverse . takeWhile (/='.') . reverse

-- Given some list of command line arguments, return either an error or a tuple
-- of the correct action and arguements.
findFlag :: [String] -> Either String (FlagOption, [String])
findFlag commandLineArgs
  -- if the first arguement is a flag, parse it appropriately.
  | head firstArg == '-' = case (head commandLineArgs) of
    "-a" -> Right (Application, tail commandLineArgs)
    "-e" -> Right (Editor, tail commandLineArgs)
    "-H" -> Right (Help, tail commandLineArgs)
  | head firstArg == '-' = error "undefined flag"
  | otherwise = Right (Default, commandLineArgs)
    where firstArg = head commandLineArgs

-- This needs to be replaced by a proper parser with a dot file.
applicationTable :: Map.Map String String
applicationTable = Map.fromList [("pdf", "mupdf"), ("jpeg", "feh"),
                                 ("org", "emacs"),
                                 ("odt", "libreoffice"), ("ods", "libreoffice"),
                                 ("avi", "vlc"), ("flac", "vlc")]

-- Grab the editor name.
editorName :: String
editorName = case (unsafePerformIO $ (getEnv "EDITOR")) of
  Nothing  -> error "EDITOR variable not set"
  Just app -> app

-- print message for usage
usageMessage = unlines $ ["Usage: open [-e] [-H] [-a <application>]",
                          "Help: Open opens files from a shell."]
