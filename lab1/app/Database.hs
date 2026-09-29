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
    , getSportSections
    , getSportSectionById
    , addSportSection
    , updateSportSection
    , deleteSportSection
    , getSchedules
    , getScheduleById
    , addSchedule
    , updateSchedule
    , deleteSchedule
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


getSportSections :: MySQLConn -> IO [SportSection]
getSportSections conn = do
    (_, stream) <- query_ conn
        "SELECT id, name, sport_type, teacher_id, location FROM sport_sections"

    rows <- Streams.toList stream
    return (map rowToSportSection rows)


getSportSectionById :: MySQLConn -> Int -> IO (Maybe SportSection)
getSportSectionById conn sectionIdValue = do
    (_, stream) <- query conn
        "SELECT id, name, sport_type, teacher_id, location FROM sport_sections WHERE id = ?"
        [MySQLInt32 (fromIntegral sectionIdValue)]

    rows <- Streams.toList stream

    case rows of
        [row] -> return (Just (rowToSportSection row))
        _     -> return Nothing


rowToSportSection :: [MySQLValue] -> SportSection
rowToSportSection
    [ MySQLInt32 sectionIdValue
    , MySQLText name
    , MySQLText sport
    , teacherValue
    , locationValue
    ] =
        SportSection
            { sectionId = fromIntegral sectionIdValue
            , sectionName = T.unpack name
            , sportType = T.unpack sport
            , sectionTeacherId = convertNullableInt teacherValue
            , sectionLocation =
                maybe "" id (convertNullableText locationValue)
            }

rowToSportSection _ =
    error "Unexpected sport section row format"


convertNullableInt :: MySQLValue -> Maybe Int
convertNullableInt MySQLNull =
    Nothing

convertNullableInt (MySQLInt32 value) =
    Just (fromIntegral value)

convertNullableInt _ =
    Nothing


addSportSection :: MySQLConn -> SportSection -> IO ()
addSportSection conn section = do
    _ <- execute conn
        "INSERT INTO sport_sections (name, sport_type, teacher_id, location) VALUES (?, ?, ?, ?)"
        [ MySQLText (T.pack (sectionName section))
        , MySQLText (T.pack (sportType section))
        , maybe MySQLNull
            (MySQLInt32 . fromIntegral)
            (sectionTeacherId section)
        , MySQLText (T.pack (sectionLocation section))
        ]

    putStrLn "Sport section added successfully."


updateSportSection :: MySQLConn -> SportSection -> IO ()
updateSportSection conn section = do
    _ <- execute conn
        "UPDATE sport_sections SET name = ?, sport_type = ?, teacher_id = ?, location = ? WHERE id = ?"
        [ MySQLText (T.pack (sectionName section))
        , MySQLText (T.pack (sportType section))
        , maybe MySQLNull
            (MySQLInt32 . fromIntegral)
            (sectionTeacherId section)
        , MySQLText (T.pack (sectionLocation section))
        , MySQLInt32 (fromIntegral (sectionId section))
        ]

    putStrLn "Sport section updated successfully."


deleteSportSection :: MySQLConn -> Int -> IO ()
deleteSportSection conn sectionIdValue = do
    _ <- execute conn
        "DELETE FROM sport_sections WHERE id = ?"
        [MySQLInt32 (fromIntegral sectionIdValue)]

    putStrLn "Sport section deleted successfully."


getSchedules :: MySQLConn -> IO [SectionSchedule]
getSchedules conn = do
    (_, stream) <- query_ conn
        "SELECT id, section_id, day_of_week, CAST(start_time AS CHAR), CAST(end_time AS CHAR), location FROM section_schedule"

    rows <- Streams.toList stream
    return (map rowToSchedule rows)


getScheduleById :: MySQLConn -> Int -> IO (Maybe SectionSchedule)
getScheduleById conn scheduleIdValue = do
    (_, stream) <- query conn
        "SELECT id, section_id, day_of_week, CAST(start_time AS CHAR), CAST(end_time AS CHAR), location FROM section_schedule WHERE id = ?"
        [MySQLInt32 (fromIntegral scheduleIdValue)]

    rows <- Streams.toList stream

    case rows of
        [row] -> return (Just (rowToSchedule row))
        _     -> return Nothing


rowToSchedule :: [MySQLValue] -> SectionSchedule
rowToSchedule
    [ MySQLInt32 scheduleIdValue
    , MySQLInt32 sectionIdValue
    , MySQLText dayOfWeek
    , MySQLText startTime
    , MySQLText endTime
    , locationValue
    ] =
        SectionSchedule
            { scheduleId = fromIntegral scheduleIdValue
            , scheduleSectionId = fromIntegral sectionIdValue
            , scheduleDayOfWeek = T.unpack dayOfWeek
            , scheduleStartTime = T.unpack startTime
            , scheduleEndTime = T.unpack endTime
            , scheduleLocation = convertNullableText locationValue
            }

rowToSchedule _ =
    error "Unexpected schedule row format"


addSchedule :: MySQLConn -> SectionSchedule -> IO ()
addSchedule conn schedule = do
    _ <- execute conn
        "INSERT INTO section_schedule (section_id, day_of_week, start_time, end_time, location) VALUES (?, ?, ?, ?, ?)"
        [ MySQLInt32 (fromIntegral (scheduleSectionId schedule))
        , MySQLText (T.pack (scheduleDayOfWeek schedule))
        , MySQLText (T.pack (scheduleStartTime schedule))
        , MySQLText (T.pack (scheduleEndTime schedule))
        , maybe MySQLNull
            (MySQLText . T.pack)
            (scheduleLocation schedule)
        ]

    putStrLn "Schedule added successfully."


updateSchedule :: MySQLConn -> SectionSchedule -> IO ()
updateSchedule conn schedule = do
    _ <- execute conn
        "UPDATE section_schedule SET section_id = ?, day_of_week = ?, start_time = ?, end_time = ?, location = ? WHERE id = ?"
        [ MySQLInt32 (fromIntegral (scheduleSectionId schedule))
        , MySQLText (T.pack (scheduleDayOfWeek schedule))
        , MySQLText (T.pack (scheduleStartTime schedule))
        , MySQLText (T.pack (scheduleEndTime schedule))
        , maybe MySQLNull
            (MySQLText . T.pack)
            (scheduleLocation schedule)
        , MySQLInt32 (fromIntegral (scheduleId schedule))
        ]

    putStrLn "Schedule updated successfully."


deleteSchedule :: MySQLConn -> Int -> IO ()
deleteSchedule conn scheduleIdValue = do
    _ <- execute conn
        "DELETE FROM section_schedule WHERE id = ?"
        [MySQLInt32 (fromIntegral scheduleIdValue)]

    putStrLn "Schedule deleted successfully."