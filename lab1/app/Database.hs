{-# LANGUAGE OverloadedStrings #-}

module Database
    ( createConnection
    , getStudents
    , getStudentById
    , addStudent
    , updateStudent
    , deleteStudent
    , getTeachers
    , getTeacherById
    , addTeacher
    , updateTeacher
    , deleteTeacher
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

getTeachers :: MySQLConn -> IO [Teacher]
getTeachers conn = do
    (_, stream) <- query_ conn
        "SELECT id, first_name, last_name, department, phone FROM teachers"

    rows <- Streams.toList stream
    return (map rowToTeacher rows)


getTeacherById :: MySQLConn -> Int -> IO (Maybe Teacher)
getTeacherById conn teacherIdValue = do
    (_, stream) <- query conn
        "SELECT id, first_name, last_name, department, phone FROM teachers WHERE id = ?"
        [MySQLInt32 (fromIntegral teacherIdValue)]

    rows <- Streams.toList stream

    case rows of
        [row] -> return (Just (rowToTeacher row))
        _     -> return Nothing


rowToTeacher :: [MySQLValue] -> Teacher
rowToTeacher
    [ MySQLInt32 teacherIdValue
    , MySQLText firstName
    , MySQLText lastName
    , MySQLText department
    , phoneValue
    ] =
        Teacher
            { teacherId = fromIntegral teacherIdValue
            , teacherFirstName = T.unpack firstName
            , teacherLastName = T.unpack lastName
            , teacherDepartment = T.unpack department
            , teacherPhone = convertNullableText phoneValue
            }

rowToTeacher _ =
    error "Unexpected teacher row format"


addTeacher :: MySQLConn -> Teacher -> IO ()
addTeacher conn teacher = do
    _ <- execute conn
        "INSERT INTO teachers (first_name, last_name, department, phone) VALUES (?, ?, ?, ?)"
        [ MySQLText (T.pack (teacherFirstName teacher))
        , MySQLText (T.pack (teacherLastName teacher))
        , MySQLText (T.pack (teacherDepartment teacher))
        , maybe MySQLNull (MySQLText . T.pack) (teacherPhone teacher)
        ]

    putStrLn "Teacher added successfully."


updateTeacher :: MySQLConn -> Teacher -> IO ()
updateTeacher conn teacher = do
    _ <- execute conn
        "UPDATE teachers SET first_name = ?, last_name = ?, department = ?, phone = ? WHERE id = ?"
        [ MySQLText (T.pack (teacherFirstName teacher))
        , MySQLText (T.pack (teacherLastName teacher))
        , MySQLText (T.pack (teacherDepartment teacher))
        , maybe MySQLNull (MySQLText . T.pack) (teacherPhone teacher)
        , MySQLInt32 (fromIntegral (teacherId teacher))
        ]

    putStrLn "Teacher updated successfully."


deleteTeacher :: MySQLConn -> Int -> IO ()
deleteTeacher conn teacherIdValue = do
    _ <- execute conn
        "DELETE FROM teachers WHERE id = ?"
        [MySQLInt32 (fromIntegral teacherIdValue)]

    putStrLn "Teacher deleted successfully."