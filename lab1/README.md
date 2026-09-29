# Functional Programming Lab 1

Information system **"Sport on Faculty"** implemented in Haskell with a MySQL database.

The application allows users to manage information about students, teachers, sport sections, section schedules, competitions, section memberships, and competition participants.

## Features

The system supports:

- Student management
- Teacher management
- Sport section management
- Section schedule management
- Competition management
- Section membership management
- Competition participant management
- Create, Read, Update and Delete operations
- MySQL database integration
- Console-based user interface
- Unicode / UTF-8 input support

## Technologies

- Haskell
- GHC
- Cabal
- MySQL 8
- `mysql-haskell`
- `io-streams`
- `dotenv`
- `haskeline`
- `code-page`

## Functional Programming Requirements

The project uses custom Haskell data types, a type class, and multiple instances.

The common `Displayable` type class is used to define how different entities are displayed:

```haskell
class Displayable a where
    display :: a -> String
```

Instances are implemented for application entities such as:

- `Student`
- `Teacher`
- `SportSection`
- `SectionSchedule`
- `Competition`
- `SectionMember`
- `CompetitionParticipant`

Example:

```haskell
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
```

This allows different data types to use the same `display` interface.

## Database

Database name:

```text
faculty_sport
```

The database contains **7 tables**.

### `students`

Stores information about students.

Main fields:

- `id`
- `first_name`
- `last_name`
- `group_name`
- `course`
- `phone`

### `teachers`

Stores information about teachers and sport section supervisors.

Main fields:

- `id`
- `first_name`
- `last_name`
- `department`
- `phone`

### `sport_sections`

Stores information about sport sections.

Main fields:

- `id`
- `name`
- `sport_type`
- `teacher_id`
- `location`

`teacher_id` references the `teachers` table.

### `section_schedule`

Stores the work schedule of sport sections.

Main fields:

- `id`
- `section_id`
- `day_of_week`
- `start_time`
- `end_time`
- `location`

`section_id` references the `sport_sections` table.

### `section_members`

Stores information about students participating in sport sections.

Main fields:

- `id`
- `student_id`
- `section_id`
- `join_date`

The table connects `students` and `sport_sections`.

### `competitions`

Stores the competition plan.

Main fields:

- `id`
- `name`
- `competition_date`
- `location`
- `description`

### `competition_participants`

Stores information about students participating in competitions.

Main fields:

- `id`
- `competition_id`
- `student_id`
- `section_id`
- `result`

The table connects competitions, students, and sport sections.

## Database Relationships

The main relationships are:

```text
teachers
   |
   v
sport_sections
   |
   +-----------> section_schedule
   |
   +-----------> section_members <---------- students
   |
   +-----------> competition_participants
                     ^              ^
                     |              |
                  students     competitions
```

Foreign keys are used to maintain consistency between related records.

## Project Structure

```text
lab1/
├── app/
│   ├── Main.hs
│   ├── Database.hs
│   └── Models.hs
│
├── database.sql
├── .env.example
├── .gitignore
├── lab1.cabal
├── README.md
├── CHANGELOG.md
└── LICENSE
```

### `Main.hs`

Contains:

- application entry point
- main menu
- entity submenus
- console input
- CRUD interaction logic

### `Database.hs`

Contains:

- MySQL connection
- SQL queries
- conversion of database rows to Haskell values
- CRUD functions

### `Models.hs`

Contains:

- Haskell data types
- `Displayable` type class
- `Displayable` instances

### `database.sql`

Contains:

- database creation
- table creation
- foreign key definitions
- initial test data

## Configuration

Database credentials are stored in a local `.env` file.

Create `.env` in the project directory based on `.env.example`:

```env
DB_HOST=127.0.0.1
DB_PORT=3306
DB_USER=root
DB_PASSWORD=your_password
DB_NAME=faculty_sport
```

The real `.env` file is ignored by Git and should not be committed.

## Database Setup

Make sure MySQL Server is running.

Create the database using:

```bash
mysql -u root -p < database.sql
```

Alternatively, open `database.sql` in a MySQL client and execute the SQL statements manually.

## Build

From the `lab1` directory run:

```bash
cabal build
```

## Run

Run the application with:

```bash
cabal run
```

After connecting to MySQL, the main menu is displayed:

```text
===== SPORT ON FACULTY =====
1. Students
2. Teachers
3. Sport sections
4. Section schedule
5. Competitions
6. Section members
7. Competition participants
0. Exit
```

Each section contains operations for viewing and managing the corresponding data.

Example:

```text
===== STUDENTS =====
1. Show students
2. Add student
3. Update student
4. Delete student
0. Back
```

## CRUD Operations

The application implements the four basic database operations:

- **Create** — add new records
- **Read** — display existing records
- **Update** — edit existing records
- **Delete** — remove records

For updates, the current value is displayed and pressing Enter keeps the existing value.

Example:

```text
First name [Олена]:
Last name [Шевченко]:
Group [К-22]:
Course [2]:
```

## Unicode Support

The application supports Ukrainian text in the console.

`Haskeline` is used for Unicode-aware console input, while `code-page` is used to provide correct UTF-8 console behavior on Windows.

This allows values such as:

```text
Олексій Амов
Кафедра фізичного виховання
Футбольна секція
Кубок факультету з футболу
```

to be entered and stored correctly.

## Example

Example student output:

```text
Student #1: Андрій Коваленко, group: К-21, course: 2
Student #2: Олена Шевченко, group: К-22, course: 2
Student #3: Максим Бондар, group: К-11, course: 1
```

## Requirements

To run the project you need:

- GHC
- Cabal
- MySQL Server
- MySQL database created from `database.sql`

The project was developed and tested with:

```text
GHC 9.10.3
MySQL 8.0
```

## Lab Assignment

Laboratory work №1 for the Functional Programming course.

Task:

> Develop the information system "Sport on Faculty" using Haskell, classes and instances, and MySQL.  
> The system must contain information about students and teachers participating in sport sections, section schedules, and competition plans.  
> The program must support entering and editing information.  
> The database must contain more than five tables.