{-# LANGUAGE OverloadedStrings #-}

module Database
    ( createConnection
    , getStudents
    , addStudent
    , updateStudent
    , getStudentById
    , deleteStudent
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

    conn <- connect defaultConnectInfoMB4
        { ciHost = host
        , ciPort = read port
        , ciUser = BS.pack user
        , ciPassword = BS.pack password
        , ciDatabase = BS.pack database
        }

    _ <- execute_ conn "SET NAMES utf8mb4"

    return conn

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

getStudentById :: MySQLConn -> Int -> IO (Maybe Student)
getStudentById conn studentIdValue = do
    (_, stream) <- query conn
        "SELECT id, first_name, last_name, group_name, course, phone FROM students WHERE id = ?"
        [MySQLInt32 (fromIntegral studentIdValue)]

    rows <- Streams.toList stream

    case rows of
        [row] -> return (Just (rowToStudent row))
        _     -> return Nothing

convertNullableText :: MySQLValue -> Maybe String
convertNullableText MySQLNull =
    Nothing

convertNullableText (MySQLText value) =
    Just (T.unpack value)

convertNullableText _ =
    Nothing

addStudent :: MySQLConn -> Student -> IO ()
addStudent conn student = do
    _ <- execute conn
        "INSERT INTO students (first_name, last_name, group_name, course, phone) VALUES (?, ?, ?, ?, ?)"
        [ MySQLText (T.pack (studentFirstName student))
        , MySQLText (T.pack (studentLastName student))
        , MySQLText (T.pack (studentGroup student))
        , MySQLInt32 (fromIntegral (studentCourse student))
        , maybe MySQLNull (MySQLText . T.pack) (studentPhone student)
        ]

    putStrLn "Student added successfully."


updateStudent :: MySQLConn -> Student -> IO ()
updateStudent conn student = do
    _ <- execute conn
        "UPDATE students SET first_name = ?, last_name = ?, group_name = ?, course = ?, phone = ? WHERE id = ?"
        [ MySQLText (T.pack (studentFirstName student))
        , MySQLText (T.pack (studentLastName student))
        , MySQLText (T.pack (studentGroup student))
        , MySQLInt32 (fromIntegral (studentCourse student))
        , maybe MySQLNull (MySQLText . T.pack) (studentPhone student)
        , MySQLInt32 (fromIntegral (studentId student))
        ]

    putStrLn "Student updated successfully."


deleteStudent :: MySQLConn -> Int -> IO ()
deleteStudent conn studentIdValue = do
    _ <- execute conn
        "DELETE FROM students WHERE id = ?"
        [MySQLInt32 (fromIntegral studentIdValue)]

    putStrLn "Student deleted successfully."