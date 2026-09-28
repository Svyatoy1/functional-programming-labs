module Main (main) where

import Database.MySQL.Base (close)
import System.IO
import GHC.IO.Encoding (setLocaleEncoding)

import Database
import Models

main :: IO ()
main = do
    setLocaleEncoding utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    conn <- createConnection

    putStrLn "Connected to MySQL successfully!"

    students <- getStudents conn

    mapM_ (putStrLn . display) students

    close conn