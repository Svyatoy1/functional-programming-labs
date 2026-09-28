{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Configuration.Dotenv
import System.Environment
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    loadFile defaultConfig

    host <- getEnv "DB_HOST"
    port <- getEnv "DB_PORT"
    user <- getEnv "DB_USER"
    password <- getEnv "DB_PASSWORD"
    database <- getEnv "DB_NAME"

    conn <- connect defaultConnectInfo
        { ciHost = host
        , ciPort = read port
        , ciUser = BS.pack user
        , ciPassword = BS.pack password
        , ciDatabase = BS.pack database
        }

    putStrLn "Connected to MySQL successfully!"

    (_, is) <- query_ conn
        "SELECT id, first_name, last_name FROM students"

    rows <- Streams.toList is

    print rows

    close conn