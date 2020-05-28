{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
--
-- Repline exposes an additional monad transformer on top of Haskeline called 'HaskelineT'. It simplifies several
-- aspects of composing Haskeline with State and Exception monads in modern versions of mtl.
--
-- > type Repl a = HaskelineT IO a
--
-- The evaluator 'evalRepl' evaluates a 'HaskelineT' monad transformer by constructing a shell with several
-- custom functions and evaluating it inside of IO:
--
--   * Commands: Handled on ordinary input.
--
--   * Completions: Handled when tab key is pressed.
--
--   * Options: Handled when a command prefixed by a prefix character is entered.
--
--   * Command prefix character: Optional command prefix ( passing Nothing ignores the Options argument ).
--
--   * Banner: Text Displayed at initialization.
--
--   * Initializer: Run at initialization.
--
-- A simple evaluation function might simply echo the output back to the screen.
--
-- > -- Evaluation : handle each line user inputs
-- > cmd :: String -> Repl ()
-- > cmd input = liftIO $ print input
--
-- Several tab completion options are available, the most common is the 'WordCompleter' which completes on single
-- words separated by spaces from a list of matches. The internal logic can be whatever is required and can also
-- access a StateT instance to query application state.
--
-- > -- Tab Completion: return a completion for partial words entered
-- > completer :: Monad m => WordCompleter m
-- > completer n = do
-- >   let names = ["kirk", "spock", "mccoy"]
-- >   return $ filter (isPrefixOf n) names
--
-- Input which is prefixed by a colon (commands like \":type\" and \":help\") queries an association list of
-- functions which map to custom logic. The function takes a space-separated list of augments in it's first
-- argument. If the entire line is desired then the 'unwords' function can be used to concatenate.
--
-- > -- Commands
-- > help :: [String] -> Repl ()
-- > help args = liftIO $ print $ "Help: " ++ show args
-- >
-- > say :: [String] -> Repl ()
-- > say args = do
-- >   _ <- liftIO $ system $ "cowsay" ++ " " ++ (unwords args)
-- >   return ()
--
-- TODO: replace with optparse-applicative style
-- Now we need only map these functions to their commands.
--
-- > options :: [(String, [String] -> Repl ())]
-- > options = [
-- >     ("help", help)  -- :help
-- >   , ("say", say)    -- :say
-- >   ]
--
-- The prefix function is simply an IO action that is called at the start of the shell.
--
-- > ini :: Repl ()
-- > ini = liftIO $ putStrLn "Welcome!"
--
-- Putting it all together we have a little shell.
--
-- > main :: IO ()
-- > main = evalRepl (pure ">>> ") cmd options (Just ':') (Word completer) ini
--
-- Alternatively instead of initialising the repl from position arguments you
-- can pass the 'ReplOpts' record with explicitly named arguments.
--
-- > main_alt :: IO ()
-- > main_alt = evalReplOpts $ ReplOpts
-- >   { prefix      = pure ">>> "
-- >   , command     = cmd
-- >   , options     = opts
-- >   , prefix      = Just ':'
-- >   , tabComplete = (Word0 completer)
-- >   , initialiser = ini
-- >   }
--
-- Putting this in a file we can test out our cow-trek shell.
--
-- > $ runhaskell Main.hs
-- > Welcome!
-- > >>> <TAB>
-- > kirk spock mccoy
-- >
-- > >>> k<TAB>
-- > kirk
-- >
-- > >>> spam
-- > "spam"
-- >
-- > >>> :say Hello Haskell
-- >  _______________
-- > < Hello Haskell >
-- >  ---------------
-- >         \   ^__^
-- >          \  (oo)\_______
-- >             (__)\       )\/\
-- >                 ||----w |
-- >                 ||     ||
--
-- See <https://github.com/sdiehl/repline> for more examples.
module System.Console.Repline
  ( -- * Repline Monad
    HaskelineT,
    runHaskelineT,

    -- * Toplevel
    evalRepl,
    ReplOpts (..),
    evalReplOpts,

    -- * Repline Types
    ReplHandler,
    WordCompleter,
    LineCompleter,
    CompleterStyle (..),

    -- * Completers
    CompletionFunc, -- re-export
    fallbackCompletion,
    wordCompleter,
    listCompleter,
    fileCompleter,
    listWordCompleter,
    runMatcher,
    trimComplete,

    -- * Utilities
    abort,
    tryAction,
    dontCrash,
    restOfLine
  )
where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Fail as Fail
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List (isPrefixOf)
import qualified System.Console.Haskeline as H
import System.Console.Haskeline.Completion
import qualified Options.Applicative as O

-------------------------------------------------------------------------------
-- Haskeline Transformer
-------------------------------------------------------------------------------

-- | Monad transformer for readline input
newtype HaskelineT (m :: * -> *) a = HaskelineT {unHaskeline :: H.InputT m a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadIO,
      MonadFix,
      MonadTrans,
      MonadHaskeline,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

-- | Run HaskelineT monad
runHaskelineT :: (MonadMask m, MonadIO m) => H.Settings m -> HaskelineT m a -> m a
runHaskelineT s m = H.runInputT s (H.withInterrupt (unHaskeline m))

class MonadCatch m => MonadHaskeline m where
  getInputLine :: String -> m (Maybe String)
  getInputChar :: String -> m (Maybe Char)
  outputStr :: String -> m ()
  outputStrLn :: String -> m ()

instance (MonadMask m, MonadIO m) => MonadHaskeline (H.InputT m) where
  getInputLine = H.getInputLine
  getInputChar = H.getInputChar
  outputStr = H.outputStr
  outputStrLn = H.outputStrLn

instance Fail.MonadFail m => Fail.MonadFail (HaskelineT m) where
  fail = lift . Fail.fail

instance MonadState s m => MonadState s (HaskelineT m) where
  get = lift get
  put = lift . put

instance MonadReader r m => MonadReader r (HaskelineT m) where
  ask = lift ask
  local f (HaskelineT m) = HaskelineT $ H.mapInputT (local f) m

instance (MonadHaskeline m) => MonadHaskeline (StateT s m) where
  getInputLine = lift . getInputLine
  getInputChar = lift . getInputChar
  outputStr = lift . outputStr
  outputStrLn = lift . outputStrLn

-------------------------------------------------------------------------------
-- Repl
-------------------------------------------------------------------------------

-- | Options function synonym
-- TODO: add combinators to combine Options. As a note:
-- Parser is an Alternative 
-- a -> m () is a contravariant functor.
--
-- For manual combination:
--
-- For sum types <|>, and case can be used 
--
--  { optsParser = A <$> aOpt <|> B <$> bOpt
--  , optHandler = \case
--       (A (xs...)) -> optHandler aOpt xs...
--       (B (xs...)) -> optHandler bOpt xs...
--  }
--
-- For prd type (<$>, <*>), and >> can be used
--
--  { optsParser = AB <$> aOpt <*> bOpt
--  , optHandler = \(AB a b) -> optHandler aOpt a >> optHandler bOpt b
--  }
--
--  I've decided that I need to split up the parser and the cmd handler
--  as parsers are composable however it is not straight forward on how 
--  handlers should be parsed. E.g sum types could be handled as: C -> m ()
--  or (a -> m ()) >> (b -> m ()); either way could make sense depending 
--  on the user.

-- | Word completer
type WordCompleter m = (String -> m [String])

-- | Line completer
type LineCompleter m = (String -> String -> m [Completion])

-- | Wrap a HasklineT action so that if an interrupt is thrown the shell continues as normal.
tryAction :: (MonadMask m, MonadIO m) => HaskelineT m a -> HaskelineT m a
tryAction (HaskelineT f) = HaskelineT (H.withInterrupt loop)
  where
    loop = handle (\H.Interrupt -> loop) f

-- | Catch all toplevel failures.
dontCrash :: (MonadIO m, MonadCatch m) => m () -> m ()
dontCrash m = catch m (\e@SomeException {} -> liftIO (print e))

-- | Abort the current REPL loop, and continue.
abort :: MonadThrow m => HaskelineT m a
abort = throwM H.Interrupt

type ReplHandler m a = a -> HaskelineT m ()

-- TODO: I don't think this is correct but it'll be a place holder for now
-- | Parse the rest of the line as a single string. Use this as a catch all
-- if no commands are given
restOfLine :: O.Mod O.ArgumentFields String -> O.Parser String
restOfLine = O.argument (unwords <$> many O.str)

-- | Completion loop.
replLoop ::
  (MonadMask m, MonadIO m) 
  => HaskelineT m String 
  -- ^ prompt prefix
  -> O.ParserInfo a 
  -- ^ Parser for custom options
  -> ReplHandler m a
  -- ^ Handler for the top-level ReplCmd
  -> HaskelineT m ()
replLoop promptPrefix optsParser handler = loop
  where
    loop = do
      prefix <- promptPrefix
      minput <- H.handleInterrupt (return $ Just "") $ getInputLine prefix
      case minput of
        Nothing   -> outputStrLn "Goodbye."
        Just line -> handleLine (words line)

    handleLine [] = loop
    handleLine cmds = H.handleInterrupt (pure ()) $ (runOptsParser cmds >> loop)
      where

        runOptsParser = 
            handleParserResult
          . O.execParserPure parserPrefs optsParser

        parserPrefs = O.prefs
           $ O.showHelpOnError
          <> O.showHelpOnEmpty
          <> O.disambiguate

        handleParserResult (O.Success x)    = handler x
        handleParserResult (O.Failure help) = do
          liftIO . putStrLn $ "Parser failed with: " <> (unwords cmds)
          liftIO . putStrLn . fst $ O.renderFailure help ""
        -- | TODO: add support for tab completion from optparse-applicative (this
        -- may rid us of the Haskeline completion?)
        handleParserResult _                = pure ()

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

-- | REPL Options datatype
data ReplOpts m a
  = ReplOpts
      { prefix :: HaskelineT m String
        -- ^ Banner
      , optsParser :: O.ParserInfo a
        -- ^ repl options parser
      , replHandler :: ReplHandler m a  
        -- ^ repl handler
      , tabComplete :: CompleterStyle m
        -- ^ Tab completion function
      , initialiser :: HaskelineT m ()
        -- ^ Initialiser
      }

-- | Evaluate the REPL logic into a MonadCatch context from the ReplOpts
-- configuration.
evalReplOpts :: (MonadMask m, MonadIO m) => ReplOpts m a -> m ()
evalReplOpts ReplOpts {..} =
  evalRepl
    prefix
    optsParser
    replHandler
    tabComplete
    initialiser

-- | Evaluate the REPL logic into a MonadCatch context.
evalRepl ::
  (MonadMask m, MonadIO m) -- Terminal monad ( often IO ).
  => HaskelineT m String 
  -- ^ prompt prefix
  -> O.ParserInfo a
  -- ^ parser info describing how to parse input
  -> ReplHandler m a
  -- ^ Repl handler
  -> CompleterStyle m 
  -- ^ Tab completion function
  -> HaskelineT m b
  -- ^ Initialiser
  -> m ()
evalRepl prefix parser handler completer initz = 
  runHaskelineT _readline $ initz >> monad
  where
    monad = replLoop prefix parser handler
    _readline =
      H.Settings
        { H.complete       = mkCompleter completer
        , H.historyFile    = Just ".history"
        , H.autoAddHistory = True
        }

-------------------------------------------------------------------------------
-- Completions
-------------------------------------------------------------------------------

-- | Tab completer types
data CompleterStyle m
  = -- | Completion function takes single word.
    Word (WordCompleter m)
  | -- | Completion function takes single word ( no space ).
    Word0 (WordCompleter m)
  | -- | Completion function takes tuple of full line.
    Cursor (LineCompleter m)
  | -- | Completion function completes files in CWD.
    File
  | -- | Conditional tab completion based on prefix.
    Prefix
      (CompletionFunc m)
      [(String, CompletionFunc m)]
  | -- | Combine two completions
    Combine (CompleterStyle m) (CompleterStyle m)
  | -- | Custom completion
    Custom (CompletionFunc m)

-- | Make a completer function from a completion type
mkCompleter :: MonadIO m => CompleterStyle m -> CompletionFunc m
mkCompleter (Word f) = completeWord (Just '\\') " \t()[]" (_simpleComplete f)
mkCompleter (Word0 f) = completeWord (Just '\\') " \t()[]" (_simpleCompleteNoSpace f)
mkCompleter (Cursor f) = completeWordWithPrev (Just '\\') " \t()[]" (unRev0 f)
mkCompleter File = completeFilename
mkCompleter (Prefix def opts) = runMatcher opts def
mkCompleter (Combine a b) = fallbackCompletion (mkCompleter a) (mkCompleter b)
mkCompleter (Custom f) = f

-- haskeline takes the first argument as the reversed string, don't know why
unRev0 :: LineCompleter m -> LineCompleter m
unRev0 f x = f (reverse x)

-- | Trim completion
trimComplete :: String -> Completion -> Completion
trimComplete prefix (Completion a b c) = Completion (drop (length prefix) a) b c

_simpleComplete :: (Monad m) => (String -> m [String]) -> String -> m [Completion]
_simpleComplete f word = map simpleCompletion <$> f word

_simpleCompleteNoSpace :: (Monad m) => (String -> m [String]) -> String -> m [Completion]
_simpleCompleteNoSpace f word = map completionNoSpace <$> f word

completionNoSpace :: String -> Completion
completionNoSpace str = Completion str str False

-- | Word completer function
wordCompleter :: Monad m => WordCompleter m -> CompletionFunc m
wordCompleter f (start, n) = completeWord (Just '\\') " \t()[]" (_simpleComplete f) (start, n)

-- | List completer function
listCompleter :: Monad m => [String] -> CompletionFunc m
listCompleter names (start, n) = completeWord (Just '\\') " \t()[]" (_simpleComplete (completeAux names)) (start, n)

-- | List word completer
listWordCompleter :: Monad m => [String] -> WordCompleter m
listWordCompleter = completeAux

-- | File completer function
fileCompleter :: MonadIO m => CompletionFunc m
fileCompleter = completeFilename

completeAux :: Monad m => [String] -> WordCompleter m
completeAux names n = return $ filter (isPrefixOf n) names

completeMatcher ::
  (Monad m) =>
  CompletionFunc m ->
  String ->
  [(String, CompletionFunc m)] ->
  CompletionFunc m
completeMatcher def _ [] args = def args
completeMatcher def [] _ args = def args
completeMatcher def s ((x, f) : xs) args
  | x `isPrefixOf` s = f args
  | otherwise = completeMatcher def s xs args

-- | Return a completion function a line fragment
runMatcher ::
  Monad m =>
  [(String, CompletionFunc m)] ->
  CompletionFunc m ->
  CompletionFunc m
runMatcher opts def (start, n) =
  completeMatcher def (n ++ reverse start) opts (start, n)
