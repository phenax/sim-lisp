module Effects.ConsoleIO where

class Monad m => ConsoleIO m where
  putStrLn :: String -> m ()

instance ConsoleIO IO where
  putStrLn = Prelude.putStrLn
