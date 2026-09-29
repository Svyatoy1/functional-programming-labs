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
    putStrLn "3. Sport sections"
    putStrLn "4. Section schedule"
    putStrLn "5. Competitions"
    putStrLn "6. Section members"
    putStrLn "7. Competition participants"
    putStrLn "0. Exit"

    choice <- prompt "Choose option: "

    case choice of
        "1" -> studentsMenu conn
        "2" -> teachersMenu conn
        "3" -> sectionsMenu conn
        "4" -> scheduleMenu conn
        "5" -> competitionsMenu conn
        "6" -> sectionMembersMenu conn
        "7" -> competitionParticipantsMenu conn
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


scheduleMenu :: MySQLConn -> IO ()
scheduleMenu conn = do
    putStrLn ""
    putStrLn "===== SECTION SCHEDULE ====="
    putStrLn "1. Show schedule"
    putStrLn "2. Add schedule"
    putStrLn "3. Update schedule"
    putStrLn "4. Delete schedule"
    putStrLn "0. Back"

    choice <- prompt "Choose option: "

    case choice of
        "1" -> do
            schedules <- getSchedules conn
            mapM_ (putStrLn . display) schedules
            scheduleMenu conn

        "2" -> do
            schedule <- readSchedule 0
            addSchedule conn schedule
            scheduleMenu conn

        "3" -> do
            idText <- prompt "Schedule ID: "
            let scheduleIdValue = read idText

            maybeSchedule <- getScheduleById conn scheduleIdValue

            case maybeSchedule of
                Nothing ->
                    putStrLn "Schedule not found."

                Just oldSchedule -> do
                    updatedSchedule <- editSchedule oldSchedule
                    updateSchedule conn updatedSchedule

            scheduleMenu conn

        "4" -> do
            idText <- prompt "Schedule ID: "
            let scheduleIdValue = read idText

            maybeSchedule <- getScheduleById conn scheduleIdValue

            case maybeSchedule of
                Nothing ->
                    putStrLn "Schedule not found."

                Just schedule -> do
                    putStrLn ("Schedule: " ++ display schedule)
                    confirmation <- prompt "Delete this schedule? (y/n): "

                    if confirmation == "y" || confirmation == "Y"
                        then deleteSchedule conn scheduleIdValue
                        else putStrLn "Deletion cancelled."

            scheduleMenu conn

        "0" ->
            menu conn

        _ -> do
            putStrLn "Invalid option."
            scheduleMenu conn


sectionsMenu :: MySQLConn -> IO ()
sectionsMenu conn = do
    putStrLn ""
    putStrLn "===== SPORT SECTIONS ====="
    putStrLn "1. Show sections"
    putStrLn "2. Add section"
    putStrLn "3. Update section"
    putStrLn "4. Delete section"
    putStrLn "0. Back"

    choice <- prompt "Choose option: "

    case choice of
        "1" -> do
            sections <- getSportSections conn
            mapM_ (putStrLn . display) sections
            sectionsMenu conn

        "2" -> do
            section <- readSportSection 0
            addSportSection conn section
            sectionsMenu conn

        "3" -> do
            idText <- prompt "Section ID: "
            let sectionIdValue = read idText

            maybeSection <- getSportSectionById conn sectionIdValue

            case maybeSection of
                Nothing ->
                    putStrLn "Section not found."

                Just oldSection -> do
                    updatedSection <- editSportSection oldSection
                    updateSportSection conn updatedSection

            sectionsMenu conn

        "4" -> do
            idText <- prompt "Section ID: "
            let sectionIdValue = read idText

            maybeSection <- getSportSectionById conn sectionIdValue

            case maybeSection of
                Nothing ->
                    putStrLn "Section not found."

                Just section -> do
                    putStrLn ("Section: " ++ display section)
                    confirmation <- prompt "Delete this section? (y/n): "

                    if confirmation == "y" || confirmation == "Y"
                        then deleteSportSection conn sectionIdValue
                        else putStrLn "Deletion cancelled."

            sectionsMenu conn

        "0" ->
            menu conn

        _ -> do
            putStrLn "Invalid option."
            sectionsMenu conn


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

readSportSection :: Int -> IO SportSection
readSportSection sectionIdValue = do
    name <- prompt "Section name: "
    sport <- prompt "Sport type: "
    teacherText <- prompt "Teacher ID (empty if none): "
    location <- prompt "Location: "

    return SportSection
        { sectionId = sectionIdValue
        , sectionName = name
        , sportType = sport
        , sectionTeacherId =
            if null teacherText
                then Nothing
                else Just (read teacherText)
        , sectionLocation = location
        }


editSportSection :: SportSection -> IO SportSection
editSportSection oldSection = do
    putStrLn "Press Enter to keep the current value."

    name <- prompt $
        "Section name [" ++ sectionName oldSection ++ "]: "

    sport <- prompt $
        "Sport type [" ++ sportType oldSection ++ "]: "

    teacherText <- prompt $
        "Teacher ID [" ++ maybe "-" show (sectionTeacherId oldSection) ++ "]: "

    location <- prompt $
        "Location [" ++ sectionLocation oldSection ++ "]: "

    return SportSection
        { sectionId = sectionId oldSection
        , sectionName =
            if null name then sectionName oldSection else name
        , sportType =
            if null sport then sportType oldSection else sport
        , sectionTeacherId =
            if null teacherText
                then sectionTeacherId oldSection
                else Just (read teacherText)
        , sectionLocation =
            if null location then sectionLocation oldSection else location
        }


readSchedule :: Int -> IO SectionSchedule
readSchedule scheduleIdValue = do
    sectionText <- prompt "Section ID: "
    day <- prompt "Day of week: "
    startTime <- prompt "Start time (HH:MM:SS): "
    endTime <- prompt "End time (HH:MM:SS): "
    location <- prompt "Location: "

    return SectionSchedule
        { scheduleId = scheduleIdValue
        , scheduleSectionId = read sectionText
        , scheduleDayOfWeek = day
        , scheduleStartTime = startTime
        , scheduleEndTime = endTime
        , scheduleLocation =
            if null location then Nothing else Just location
        }


editSchedule :: SectionSchedule -> IO SectionSchedule
editSchedule oldSchedule = do
    putStrLn "Press Enter to keep the current value."

    sectionText <- prompt $
        "Section ID [" ++ show (scheduleSectionId oldSchedule) ++ "]: "

    day <- prompt $
        "Day [" ++ scheduleDayOfWeek oldSchedule ++ "]: "

    startTime <- prompt $
        "Start time [" ++ scheduleStartTime oldSchedule ++ "]: "

    endTime <- prompt $
        "End time [" ++ scheduleEndTime oldSchedule ++ "]: "

    location <- prompt $
        "Location [" ++ maybe "-" id (scheduleLocation oldSchedule) ++ "]: "

    return SectionSchedule
        { scheduleId = scheduleId oldSchedule
        , scheduleSectionId =
            if null sectionText
                then scheduleSectionId oldSchedule
                else read sectionText
        , scheduleDayOfWeek =
            if null day then scheduleDayOfWeek oldSchedule else day
        , scheduleStartTime =
            if null startTime then scheduleStartTime oldSchedule else startTime
        , scheduleEndTime =
            if null endTime then scheduleEndTime oldSchedule else endTime
        , scheduleLocation =
            if null location
                then scheduleLocation oldSchedule
                else Just location
        }


competitionsMenu :: MySQLConn -> IO ()
competitionsMenu conn = do
    putStrLn ""
    putStrLn "===== COMPETITIONS ====="
    putStrLn "1. Show competitions"
    putStrLn "2. Add competition"
    putStrLn "3. Update competition"
    putStrLn "4. Delete competition"
    putStrLn "0. Back"

    choice <- prompt "Choose option: "

    case choice of
        "1" -> do
            competitions <- getCompetitions conn
            mapM_ (putStrLn . display) competitions
            competitionsMenu conn

        "2" -> do
            competition <- readCompetition 0
            addCompetition conn competition
            competitionsMenu conn

        "3" -> do
            idText <- prompt "Competition ID: "
            maybeCompetition <-
                getCompetitionById conn (read idText)

            case maybeCompetition of
                Nothing ->
                    putStrLn "Competition not found."

                Just oldCompetition -> do
                    competition <- editCompetition oldCompetition
                    updateCompetition conn competition

            competitionsMenu conn

        "4" -> do
            idText <- prompt "Competition ID: "
            deleteCompetition conn (read idText)
            competitionsMenu conn

        "0" ->
            menu conn

        _ ->
            competitionsMenu conn


readCompetition :: Int -> IO Competition
readCompetition competitionIdValue = do
    name <- prompt "Name: "
    date <- prompt "Date (YYYY-MM-DD): "
    location <- prompt "Location: "
    description <- prompt "Description: "

    return Competition
        { competitionId = competitionIdValue
        , competitionName = name
        , competitionDate = date
        , competitionLocation = location
        , competitionDescription =
            if null description
                then Nothing
                else Just description
        }


editCompetition :: Competition -> IO Competition
editCompetition oldCompetition = do
    putStrLn "Press Enter to keep the current value."

    name <- prompt $
        "Name [" ++ competitionName oldCompetition ++ "]: "

    date <- prompt $
        "Date [" ++ competitionDate oldCompetition ++ "]: "

    location <- prompt $
        "Location [" ++ competitionLocation oldCompetition ++ "]: "

    description <- prompt $
        "Description ["
        ++ maybe "" id (competitionDescription oldCompetition)
        ++ "]: "

    return Competition
        { competitionId = competitionId oldCompetition
        , competitionName =
            if null name
                then competitionName oldCompetition
                else name
        , competitionDate =
            if null date
                then competitionDate oldCompetition
                else date
        , competitionLocation =
            if null location
                then competitionLocation oldCompetition
                else location
        , competitionDescription =
            if null description
                then competitionDescription oldCompetition
                else Just description
        }


sectionMembersMenu :: MySQLConn -> IO ()
sectionMembersMenu conn = do
    putStrLn ""
    putStrLn "===== SECTION MEMBERS ====="
    putStrLn "1. Show members"
    putStrLn "2. Add member"
    putStrLn "3. Update member"
    putStrLn "4. Delete member"
    putStrLn "0. Back"

    choice <- prompt "Choose option: "

    case choice of
        "1" -> do
            members <- getSectionMembers conn
            mapM_ (putStrLn . display) members
            sectionMembersMenu conn

        "2" -> do
            member <- readSectionMember 0
            addSectionMember conn member
            sectionMembersMenu conn

        "3" -> do
            idText <- prompt "Membership ID: "
            maybeMember <-
                getSectionMemberById conn (read idText)

            case maybeMember of
                Nothing ->
                    putStrLn "Membership not found."

                Just oldMember -> do
                    member <- editSectionMember oldMember
                    updateSectionMember conn member

            sectionMembersMenu conn

        "4" -> do
            idText <- prompt "Membership ID: "
            deleteSectionMember conn (read idText)
            sectionMembersMenu conn

        "0" ->
            menu conn

        _ ->
            sectionMembersMenu conn


readSectionMember :: Int -> IO SectionMember
readSectionMember membershipIdValue = do
    studentText <- prompt "Student ID: "
    sectionText <- prompt "Section ID: "
    joinDate <- prompt "Join date (YYYY-MM-DD): "

    return SectionMember
        { membershipId = membershipIdValue
        , memberStudentId = read studentText
        , memberSectionId = read sectionText
        , memberJoinDate =
            if null joinDate then Nothing else Just joinDate
        }


editSectionMember :: SectionMember -> IO SectionMember
editSectionMember oldMember = do
    putStrLn "Press Enter to keep the current value."

    studentText <- prompt $
        "Student ID [" ++ show (memberStudentId oldMember) ++ "]: "

    sectionText <- prompt $
        "Section ID [" ++ show (memberSectionId oldMember) ++ "]: "

    joinDate <- prompt $
        "Join date ["
        ++ maybe "" id (memberJoinDate oldMember)
        ++ "]: "

    return SectionMember
        { membershipId = membershipId oldMember
        , memberStudentId =
            if null studentText
                then memberStudentId oldMember
                else read studentText
        , memberSectionId =
            if null sectionText
                then memberSectionId oldMember
                else read sectionText
        , memberJoinDate =
            if null joinDate
                then memberJoinDate oldMember
                else Just joinDate
        }


competitionParticipantsMenu :: MySQLConn -> IO ()
competitionParticipantsMenu conn = do
    putStrLn ""
    putStrLn "===== COMPETITION PARTICIPANTS ====="
    putStrLn "1. Show participants"
    putStrLn "2. Add participant"
    putStrLn "3. Update participant"
    putStrLn "4. Delete participant"
    putStrLn "0. Back"

    choice <- prompt "Choose option: "

    case choice of
        "1" -> do
            participants <- getCompetitionParticipants conn
            mapM_ (putStrLn . display) participants
            competitionParticipantsMenu conn

        "2" -> do
            participant <- readCompetitionParticipant 0
            addCompetitionParticipant conn participant
            competitionParticipantsMenu conn

        "3" -> do
            idText <- prompt "Participant ID: "

            maybeParticipant <-
                getCompetitionParticipantById conn (read idText)

            case maybeParticipant of
                Nothing ->
                    putStrLn "Participant not found."

                Just oldParticipant -> do
                    participant <-
                        editCompetitionParticipant oldParticipant

                    updateCompetitionParticipant conn participant

            competitionParticipantsMenu conn

        "4" -> do
            idText <- prompt "Participant ID: "
            deleteCompetitionParticipant conn (read idText)
            competitionParticipantsMenu conn

        "0" ->
            menu conn

        _ ->
            competitionParticipantsMenu conn


readCompetitionParticipant :: Int -> IO CompetitionParticipant
readCompetitionParticipant participantIdValue = do
    competitionText <- prompt "Competition ID: "
    studentText <- prompt "Student ID: "
    sectionText <- prompt "Section ID (empty if none): "
    result <- prompt "Result (empty if none): "

    return CompetitionParticipant
        { participantId = participantIdValue
        , participantCompetitionId = read competitionText
        , participantStudentId = read studentText
        , participantSectionId =
            if null sectionText
                then Nothing
                else Just (read sectionText)
        , participantResult =
            if null result
                then Nothing
                else Just result
        }


editCompetitionParticipant
    :: CompetitionParticipant
    -> IO CompetitionParticipant

editCompetitionParticipant oldParticipant = do
    putStrLn "Press Enter to keep the current value."

    competitionText <- prompt $
        "Competition ID ["
        ++ show (participantCompetitionId oldParticipant)
        ++ "]: "

    studentText <- prompt $
        "Student ID ["
        ++ show (participantStudentId oldParticipant)
        ++ "]: "

    sectionText <- prompt $
        "Section ID ["
        ++ maybe "-" show (participantSectionId oldParticipant)
        ++ "]: "

    result <- prompt $
        "Result ["
        ++ maybe "" id (participantResult oldParticipant)
        ++ "]: "

    return CompetitionParticipant
        { participantId = participantId oldParticipant

        , participantCompetitionId =
            if null competitionText
                then participantCompetitionId oldParticipant
                else read competitionText

        , participantStudentId =
            if null studentText
                then participantStudentId oldParticipant
                else read studentText

        , participantSectionId =
            if null sectionText
                then participantSectionId oldParticipant
                else Just (read sectionText)

        , participantResult =
            if null result
                then participantResult oldParticipant
                else Just result
        }