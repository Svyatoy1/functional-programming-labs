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
    putStrLn "1. Students"
    putStrLn "2. Teachers"
    putStrLn "0. Exit"

    choice <- prompt "Choose option: "

    case choice of
        "1" -> studentsMenu conn
        "2" -> teachersMenu conn
        "0" -> putStrLn "Goodbye!"
        _ -> do
            putStrLn "Invalid option."
            menu conn


studentsMenu :: MySQLConn -> IO ()
studentsMenu conn = do
    putStrLn ""
    putStrLn "===== STUDENTS ====="
    putStrLn "1. Show students"
    putStrLn "2. Add student"
    putStrLn "3. Update student"
    putStrLn "4. Delete student"
    putStrLn "0. Back"

    choice <- prompt "Choose option: "

    case choice of
        "1" -> do
            students <- getStudents conn
            mapM_ (putStrLn . display) students
            studentsMenu conn

        "2" -> do
            student <- readStudent 0
            addStudent conn student
            studentsMenu conn

        "3" -> do
            idText <- prompt "Student ID: "
            let studentIdValue = read idText

            maybeStudent <- getStudentById conn studentIdValue

            case maybeStudent of
                Nothing -> putStrLn "Student not found."
                Just oldStudent -> do
                    updatedStudent <- editStudent oldStudent
                    updateStudent conn updatedStudent

            studentsMenu conn

        "4" -> do
            idText <- prompt "Student ID: "
            let studentIdValue = read idText

            maybeStudent <- getStudentById conn studentIdValue

            case maybeStudent of
                Nothing ->
                    putStrLn "Student not found."

                Just student -> do
                    putStrLn ("Student: " ++ display student)
                    confirmation <- prompt "Delete this student? (y/n): "

                    if confirmation == "y" || confirmation == "Y"
                        then deleteStudent conn studentIdValue
                        else putStrLn "Deletion cancelled."

            studentsMenu conn

        "0" ->
            menu conn

        _ -> do
            putStrLn "Invalid option."
            studentsMenu conn


teachersMenu :: MySQLConn -> IO ()
teachersMenu conn = do
    putStrLn ""
    putStrLn "===== TEACHERS ====="
    putStrLn "1. Show teachers"
    putStrLn "2. Add teacher"
    putStrLn "3. Update teacher"
    putStrLn "4. Delete teacher"
    putStrLn "0. Back"

    choice <- prompt "Choose option: "

    case choice of
        "1" -> do
            teachers <- getTeachers conn
            mapM_ (putStrLn . display) teachers
            teachersMenu conn

        "2" -> do
            teacher <- readTeacher 0
            addTeacher conn teacher
            teachersMenu conn

        "3" -> do
            idText <- prompt "Teacher ID: "
            let teacherIdValue = read idText

            maybeTeacher <- getTeacherById conn teacherIdValue

            case maybeTeacher of
                Nothing ->
                    putStrLn "Teacher not found."

                Just oldTeacher -> do
                    updatedTeacher <- editTeacher oldTeacher
                    updateTeacher conn updatedTeacher

            teachersMenu conn

        "4" -> do
            idText <- prompt "Teacher ID: "
            let teacherIdValue = read idText

            maybeTeacher <- getTeacherById conn teacherIdValue

            case maybeTeacher of
                Nothing ->
                    putStrLn "Teacher not found."

                Just teacher -> do
                    putStrLn ("Teacher: " ++ display teacher)
                    confirmation <- prompt "Delete this teacher? (y/n): "

                    if confirmation == "y" || confirmation == "Y"
                        then deleteTeacher conn teacherIdValue
                        else putStrLn "Deletion cancelled."

            teachersMenu conn

        "0" ->
            menu conn

        _ -> do
            putStrLn "Invalid option."
            teachersMenu conn


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


readTeacher :: Int -> IO Teacher
readTeacher teacherIdValue = do
    firstName <- prompt "First name: "
    lastName <- prompt "Last name: "
    department <- prompt "Department: "
    phone <- prompt "Phone: "

    return Teacher
        { teacherId = teacherIdValue
        , teacherFirstName = firstName
        , teacherLastName = lastName
        , teacherDepartment = department
        , teacherPhone =
            if null phone
                then Nothing
                else Just phone
        }


editTeacher :: Teacher -> IO Teacher
editTeacher oldTeacher = do
    putStrLn "Press Enter to keep the current value."

    firstName <- prompt $
        "First name [" ++ teacherFirstName oldTeacher ++ "]: "

    lastName <- prompt $
        "Last name [" ++ teacherLastName oldTeacher ++ "]: "

    department <- prompt $
        "Department [" ++ teacherDepartment oldTeacher ++ "]: "

    phone <- prompt $
        "Phone [" ++ maybe "" id (teacherPhone oldTeacher) ++ "]: "

    return Teacher
        { teacherId = teacherId oldTeacher
        , teacherFirstName =
            if null firstName
                then teacherFirstName oldTeacher
                else firstName
        , teacherLastName =
            if null lastName
                then teacherLastName oldTeacher
                else lastName
        , teacherDepartment =
            if null department
                then teacherDepartment oldTeacher
                else department
        , teacherPhone =
            if null phone
                then teacherPhone oldTeacher
                else Just phone
        }