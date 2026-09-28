{-# LANGUAGE OverloadedStrings #-}

module Database
    ( createConnection
    , getStudents
    ) where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Configuration.Dotenv
import System.Environment
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Models

createConnection :: IO MySQLConn
createConnection = do
    loadFile defaultConfig

    host <- getEnv "DB_HOST"
    port <- getEnv "DB_PORT"
    user <- getEnv "DB_USER"
    password <- getEnv "DB_PASSWORD"
    database <- getEnv "DB_NAME"

    connect defaultConnectInfo
        { ciHost = host
        , ciPort = read port
        , ciUser = BS.pack user
        , ciPassword = BS.pack password
        , ciDatabase = BS.pack database
        }

getStudents :: MySQLConn -> IO [Student]
getStudents conn = do
    (_, stream) <- query_ conn
        "SELECT id, first_name, last_name, group_name, course, phone FROM students"

    rows <- Streams.toList stream

    return (map rowToStudent rows)

rowToStudent :: [MySQLValue] -> Student
rowToStudent
    [ MySQLInt32 studentIdValue
    , MySQLText firstName
    , MySQLText lastName
    , MySQLText groupName
    , MySQLInt32 courseValue
    , phoneValue
    ] =
        Student
            { studentId = fromIntegral studentIdValue
            , studentFirstName = T.unpack firstName
            , studentLastName = T.unpack lastName
            , studentGroup = T.unpack groupName
            , studentCourse = fromIntegral courseValue
            , studentPhone = convertNullableText phoneValue
            }

rowToStudent _ =
    error "Unexpected student row format"

convertNullableText :: MySQLValue -> Maybe String
convertNullableText MySQLNull =
    Nothing

convertNullableText (MySQLText value) =
    Just (T.unpack value)

convertNullableText _ =
    Nothing