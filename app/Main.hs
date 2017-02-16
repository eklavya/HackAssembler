module Main where

import           Addressing
import           Control.Monad.Except
import           Data.Set             as DS
import           Parser
import           System.Environment
import           System.IO
import           Text.Parsec
import           Translate



main :: IO ()
main = do
    args <- getArgs
    if length args > 1
      then print "Illegal arguments, please specify the file path to be assembled as the only argument"
      else do
        res <- runExceptT $ assemble $ head args
        case res of
          Left e -> print e
          Right _ -> print $ "successfully assembled file to " ++ head args ++ ".hack"



assemble :: String -> ExceptT String IO ()
assemble fname = do
  input <- liftIO $ readFile fname
  res <- runParserT (runPs fname) (DS.empty, DS.empty) fname input
  h <- liftIO $ openFile (fname ++ ".hack") WriteMode
  let mc = fmap (translate . addressify) res
  case mc of
    Right c -> liftIO $ sequence_ $ fmap (\s -> hPutStr h (s ++ "\n")) c
    Left e  -> throwError (show e)
  liftIO $ hFlush h
  liftIO $ hClose h


