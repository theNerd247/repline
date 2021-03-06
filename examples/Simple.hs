{-# LANGUAGE LambdaCase #-}

module Main (main, repl) where

import Control.Applicative
import Control.Monad.Trans
import Data.List (isPrefixOf)
import System.Console.Repline
import System.Process (callCommand)
import qualified Options.Applicative as O

type Repl = HaskelineT IO 

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = ["kirk", "spock", "mccoy"]
  return $ filter (isPrefixOf n) names

-- Commands
data Cmds
  = Help String
  | Say String
  | Echo String
  | Quit

cmdsPInfo = O.info (cmdsP <**> O.helper) O.fullDesc

cmdsP :: O.Parser Cmds
cmdsP = 
    O.hsubparser (helpP <> sayP <> quitP)
 <|> echoP

echoP = Echo <$> restOfLine O.hidden

helpP = O.command ":help" 
  $ O.info 
    (fmap Help 
      $ O.strArgument 
      $ O.help "the command to show help for"
        <> O.metavar "CMD"
    ) 
  $ O.progDesc "Display help for a command"

sayP = O.command ":say" 
  $ O.info 
    (fmap Say 
      $ restOfLine
      $ O.help "Text to cowsay" <> O.metavar "TEXT"
    ) 
  $ O.progDesc "Display a cowsay"

quitP = O.command ":quit" 
  $ O.info (pure Quit) 
  $ O.progDesc "Quit the repl"

cmdsHandler (Help str) = help_ str
cmdsHandler (Say str)  = say_ str
cmdsHandler (Echo str) = echo_ str
cmdsHandler Quit       = abort

help_ = liftIO . print . ("Help: " ++) 

say_ = liftIO . callCommand . ("cowsay" ++) . (" " ++) 

echo_ = liftIO . putStrLn . ("echo: " ++)

-- opts :: [(String, [String] -> Repl ())]
-- opts =
--   [ ("help", help), -- :help
--     ("say", say) -- :say
--   ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome!"

repl :: IO ()
repl = evalReplOpts $ ReplOpts
  { prefix      = pure ">>> "
  , optsParser  = cmdsPInfo
  , replHandler = cmdsHandler
  , tabComplete = (Word0 completer)
  , initialiser = ini
  }

main :: IO ()
main = pure ()
