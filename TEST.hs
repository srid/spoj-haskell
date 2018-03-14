module Main where

import Control.Monad (forM_)

takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p (x:xs) = do
  v <- x
  case p v of 
    True -> takeWhileM p xs >>= (\vs -> return (v:vs)) 
    False -> return []

readNumbers :: [IO Int]
readNumbers = f <$ [1..]
  where f = getLine >>= return . read :: IO Int

readUntil42 :: IO [Int]
readUntil42 = takeWhileM (/= 42) readNumbers

main :: IO ()
main = do
  inp <- readUntil42
  forM_ inp $ putStrLn . show 
