module Lib
  ( start,
  )
where

import System.IO
import Data.List (isInfixOf)

start :: [String] -> IO ()
start args = do
  content <- getFileContent $ head args
  let allLines = lines content
  let filtered = filterLines allLines $ get2ndElement args
  let filteredMinus = map addLeadMinus filtered
  printElements $ showWhenNoLines filteredMinus

printElements :: [String] -> IO ()
printElements [] = return ()
printElements (x:xs) = do
  putStrLn x
  printElements xs

get2ndElement :: [String] -> String
get2ndElement (_ : x : _) = x
get2ndElement _ = []

getFileContent :: FilePath -> IO String
getFileContent path = do
  file <- openFile path ReadMode
  hGetContents file

filterLines :: [String] -> String -> [String]
filterLines [] _ = []
filterLines [x] query
  | query `isInfixOf` x = [x]
  | otherwise = []
filterLines (x : xs) query
  | query `isInfixOf` x = x : filterLines xs query
  | otherwise = filterLines xs query

addLeadMinus :: String -> String
addLeadMinus [] = []
addLeadMinus x = "- " ++ x

showWhenNoLines :: [String] -> [String]
showWhenNoLines [] = ["- No lines found"]
showWhenNoLines x = x