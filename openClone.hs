-------------------------------------------------------------------------------
-- openClone.hs
-- Author: David Wells <drwells at default Virginia Tech email>
-- Description: A clone of the 'open' utility from NeXT
-------------------------------------------------------------------------------

import System
import System.Process
import qualified Data.Map as Map

main = getArgs >>= startApplications
-- process a list of commands to run by mapping runCommand (creates a process)
-- over the list.
       where startApplications = mapM_ runCommand . commandList
-- create a list of commands by attaching programs to files. In particular,
-- this creates a function that attaches the correct application to each
-- filetype.
             commandList       = map attachCommands

-- room for improvement - make it filter in blocks (block of flags, block of
-- files in the first unit, block of flags, block of files in the second, etc)

-- right now - no flags. Try using 'span isFlag args'

attachCommands :: String -> String
attachCommands file =
  case Map.lookup (suffix file) applicationTable of
      Just app -> app ++ " " ++ file
      Nothing  ->  error "application not defined"
  where suffix = tail . dropWhile (/='.')

-- This needs to be replaced by a proper parser with a dot file.
applicationTable :: Map.Map String String
applicationTable = Map.fromList [("pdf", "mupdf"), ("jpeg", "feh"),
                                 ("tex", "emacs"), ("odt", "libreoffice"),
                                 ("avi", "vlc"), ("flac", "vlc")]

-------------------------------------------------------------------------------
-- findFile - locate the file or application name passed in by command line.
-- These should be the only arguements that do not start with a '-'.
-------------------------------------------------------------------------------
findFiles :: [String] -> [String]
findFiles = filter (not . isFlag)

-- test to see if an arguement is a long or short flag.
isFlag :: String -> Bool
isFlag entry = neither [isLongFlag entry, isShortFlag entry]
               where neither = not . and

-- test to see if a single arguement is a long flag.
isLongFlag :: String -> Bool
isLongFlag entry = and [(entry !! 0 == '-'), (entry !! 1 == '-')]

-- test to see if a single arguement is a short flag.
isShortFlag :: String -> Bool
isShortFlag entry = and [(entry !! 0 == '-'), (entry !! 1 /= '-')]
