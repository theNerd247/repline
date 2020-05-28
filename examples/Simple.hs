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

opts :: Options Repl Cmds
opts = Options
  { optParser
       =  Help <$> optParser help_ 
      <|> Say <$> optParser say_
  , optHandler = \case 
      (Help str) -> optHandler help_ str
      (Say str)  -> optHandler say_ str
  }

help_ :: Options Repl String
help_ = Options
  { optParser  = O.subparser $ O.command "help" (O.info (O.strArgument mempty) mempty)
  , optHandler = liftIO . print . ("Help: " ++) 
  }

say_ :: Options Repl String
say_ = Options
  { optParser  = O.subparser $ O.command "say" (O.info (O.strArgument mempty) mempty)
  , optHandler = \args -> do
      liftIO $ callCommand $ "cowsay" ++ " " ++ args
      return ()
  }

-- opts :: [(String, [String] -> Repl ())]
-- opts =
--   [ ("help", help), -- :help
--     ("say", say) -- :say
--   ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome!"

repl_alt :: IO ()
repl_alt = evalReplOpts $ ReplOpts
  { banner      = pure ">>> "
  , command     = cmd
  , options     = opts
  , prefix      = Just ':'
  , tabComplete = (Word0 completer)
  , initialiser = ini
  }

repl :: IO ()
repl = evalRepl (pure ">>> ") cmd opts (Just ':') (Word0 completer) ini

main :: IO ()
main = pure ()
