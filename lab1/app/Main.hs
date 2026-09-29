module Main (main) where

import Database.MySQL.Base (MySQLConn, close)
import System.IO.CodePage (withCP65001)
import System.Console.Haskeline
    ( defaultSettings
    , getInputLine
    , runInputT
    )

import Database
import Models

main :: IO ()
main = withCP65001 runApp

runApp :: IO ()
runApp = do
    conn <- createConnection

    putStrLn "Connected to MySQL successfully!"

    menu conn

    close conn

prompt :: String -> IO String
prompt message = do
    result <- runInputT defaultSettings (getInputLine message)

    case result of
        Just value -> return value
        Nothing    -> return ""


menu :: MySQLConn -> IO ()
menu conn = do
    putStrLn ""
    putStrLn "===== SPORT ON FACULTY ====="
    putStrLn "1. Show students"
    putStrLn "2. Add student"
    putStrLn "3. Update student"
    putStrLn "4. Delete student"
    putStrLn "0. Exit"
    choice <- prompt "Choose option: "

    case choice of
        "1" -> do
            students <- getStudents conn
            mapM_ (putStrLn . display) students
            menu conn

        "2" -> do
            student <- readStudent 0
            addStudent conn student
            menu conn

        "3" -> do
            idText <- prompt "Student ID: "
            let studentIdValue = read idText

            maybeStudent <- getStudentById conn studentIdValue

            case maybeStudent of
                Nothing -> do
                    putStrLn "Student not found."
                    menu conn

                Just oldStudent -> do
                    updatedStudent <- editStudent oldStudent
                    updateStudent conn updatedStudent
                    menu conn

        "4" -> do
            idText <- prompt "Student ID: "
            let studentIdValue = read idText

            maybeStudent <- getStudentById conn studentIdValue

            case maybeStudent of
                Nothing -> do
                    putStrLn "Student not found."
                    menu conn

                Just student -> do
                    putStrLn ("Student: " ++ display student)
                    confirmation <- prompt "Delete this student? (y/n): "

                    if confirmation == "y" || confirmation == "Y"
                        then do
                            deleteStudent conn studentIdValue
                            menu conn
                        else do
                            putStrLn "Deletion cancelled."
                            menu conn

        "0" ->
            putStrLn "Goodbye!"

        _ -> do
            putStrLn "Invalid option."
            menu conn


readStudent :: Int -> IO Student
readStudent studentIdValue = do
    firstName <- prompt "First name: "
    lastName <- prompt "Last name: "
    groupName <- prompt "Group: "
    courseText <- prompt "Course: "
    phone <- prompt "Phone: "

    return Student
        { studentId = studentIdValue
        , studentFirstName = firstName
        , studentLastName = lastName
        , studentGroup = groupName
        , studentCourse = read courseText
        , studentPhone =
            if null phone
                then Nothing
                else Just phone
        }

editStudent :: Student -> IO Student
editStudent oldStudent = do
    putStrLn "Press Enter to keep the current value."

    firstName <- prompt $
        "First name [" ++ studentFirstName oldStudent ++ "]: "

    lastName <- prompt $
        "Last name [" ++ studentLastName oldStudent ++ "]: "

    groupName <- prompt $
        "Group [" ++ studentGroup oldStudent ++ "]: "

    courseText <- prompt $
        "Course [" ++ show (studentCourse oldStudent) ++ "]: "

    phone <- prompt $
        "Phone [" ++ maybe "" id (studentPhone oldStudent) ++ "]: "

    return Student
        { studentId = studentId oldStudent
        , studentFirstName =
            if null firstName
                then studentFirstName oldStudent
                else firstName
        , studentLastName =
            if null lastName
                then studentLastName oldStudent
                else lastName
        , studentGroup =
            if null groupName
                then studentGroup oldStudent
                else groupName
        , studentCourse =
            if null courseText
                then studentCourse oldStudent
                else read courseText
        , studentPhone =
            if null phone
                then studentPhone oldStudent
                else Just phone
        }