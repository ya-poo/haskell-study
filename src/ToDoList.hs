{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ToDoList where

import Control.Monad (forever)
import Data.Char (toUpper)
import Data.List (delete)
import System.IO (openTempFile, hClose, hPutStr)
import Control.Exception.Base (bracketOnError)
import System.Directory ( removeFile, renameFile )
import System.Environment (getArgs)

handleToDoList :: IO()
handleToDoList = do
    let fileName = "todo.txt"
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    numberString <- getLine 
    let number = read numberString
        newToDoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newToDoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)

add :: [String] -> IO()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    let number = read numberString
        newToDoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newToDoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)


dispatch :: String -> [String] -> IO()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove

commandToDoList :: IO ()
commandToDoList = do
    (command:argList) <- getArgs
    dispatch command argList
