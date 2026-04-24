module Main (main) where

import GHC.IO.Handle (hClose, hGetContents', hPutStr)
import System.Environment (getArgs)
import System.IO (IOMode (ReadWriteMode), hGetContents, openFile)

main :: IO ()
main = do
  args <- getArgs
  if null args then repl else getArgs >>= execCmd >>= putStrLn
 where
  repl :: IO ()
  repl = do
    line <- getLine
    case words line of
      [] -> repl
      ["quit"] -> return ()
      cmd -> execCmd cmd >>= putStrLn >> repl

execCmd :: [String] -> IO String
execCmd ("add" : [item]) = do appendFile "./tasks" (" " ++ item); return $ "adding " ++ item
execCmd ("remove" : [item]) = do
  f <- openFile "./tasks" ReadWriteMode
  tasks <- fmap lines (hGetContents f)
  hPutStr f (unwords (filter (/= item) tasks))
  hClose f
  return $ "removing " ++ item
execCmd cmd = do return $ "command not recognized: " ++ unwords cmd
