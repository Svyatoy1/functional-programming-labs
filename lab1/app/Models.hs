module Models where

data Student = Student
    { studentId :: Int
    , studentFirstName :: String
    , studentLastName :: String
    , studentGroup :: String
    , studentCourse :: Int
    , studentPhone :: Maybe String
    } deriving (Show)

data Teacher = Teacher
    { teacherId :: Int
    , teacherFirstName :: String
    , teacherLastName :: String
    , teacherDepartment :: String
    , teacherPhone :: Maybe String
    } deriving (Show)

data SportSection = SportSection
    { sectionId :: Int
    , sectionName :: String
    , sportType :: String
    , sectionTeacherId :: Maybe Int
    , sectionLocation :: String
    } deriving (Show)

data Competition = Competition
    { competitionId :: Int
    , competitionName :: String
    , competitionDate :: String
    , competitionLocation :: String
    , competitionDescription :: Maybe String
    } deriving (Show)

data SectionSchedule = SectionSchedule
    { scheduleId :: Int
    , scheduleSectionId :: Int
    , scheduleDayOfWeek :: String
    , scheduleStartTime :: String
    , scheduleEndTime :: String
    , scheduleLocation :: Maybe String
    } deriving (Show)

class Displayable a where
    display :: a -> String

instance Displayable Student where
    display s =
        "Student #" ++ show (studentId s)
        ++ ": "
        ++ studentFirstName s
        ++ " "
        ++ studentLastName s
        ++ ", group: "
        ++ studentGroup s
        ++ ", course: "
        ++ show (studentCourse s)

instance Displayable Teacher where
    display t =
        "Teacher #" ++ show (teacherId t)
        ++ ": "
        ++ teacherFirstName t
        ++ " "
        ++ teacherLastName t
        ++ ", department: "
        ++ teacherDepartment t

instance Displayable SportSection where
    display s =
        "Section #" ++ show (sectionId s)
        ++ ": "
        ++ sectionName s
        ++ " ("
        ++ sportType s
        ++ "), location: "
        ++ sectionLocation s

instance Displayable Competition where
    display c =
        "Competition #" ++ show (competitionId c)
        ++ ": "
        ++ competitionName c
        ++ ", date: "
        ++ competitionDate c
        ++ ", location: "
        ++ competitionLocation c

instance Displayable SectionSchedule where
    display s =
        "Schedule #" ++ show (scheduleId s)
        ++ ": section #" ++ show (scheduleSectionId s)
        ++ ", " ++ scheduleDayOfWeek s
        ++ ", " ++ scheduleStartTime s
        ++ " - " ++ scheduleEndTime s
        ++ ", location: "
        ++ maybe "-" id (scheduleLocation s)