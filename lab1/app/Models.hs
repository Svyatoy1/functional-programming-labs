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

data SectionMember = SectionMember
    { membershipId :: Int
    , memberStudentId :: Int
    , memberSectionId :: Int
    , memberJoinDate :: Maybe String
    } deriving (Show)

data CompetitionParticipant = CompetitionParticipant
    { participantId :: Int
    , participantCompetitionId :: Int
    , participantStudentId :: Int
    , participantSectionId :: Maybe Int
    , participantResult :: Maybe String
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

instance Displayable SectionMember where
    display m =
        "Membership #" ++ show (membershipId m)
        ++ ": student #" ++ show (memberStudentId m)
        ++ ", section #" ++ show (memberSectionId m)
        ++ ", joined: " ++ maybe "-" id (memberJoinDate m)

instance Displayable CompetitionParticipant where
    display p =
        "Participant #" ++ show (participantId p)
        ++ ": competition #" ++ show (participantCompetitionId p)
        ++ ", student #" ++ show (participantStudentId p)
        ++ ", section #" ++ maybe "-" show (participantSectionId p)
        ++ ", result: " ++ maybe "-" id (participantResult p)