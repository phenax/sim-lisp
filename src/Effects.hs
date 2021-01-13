module Effects where

--data Effects = PutString String | ReadString;

class Monad m => EffectsIO m where
  putStrLn :: String -> m ()

instance EffectsIO IO where
  putStrLn = Prelude.putStrLn
