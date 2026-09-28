CREATE DATABASE faculty_sport;
USE faculty_sport;

CREATE TABLE students (
    id INT AUTO_INCREMENT PRIMARY KEY,
    first_name VARCHAR(50) NOT NULL,
    last_name VARCHAR(50) NOT NULL,
    group_name VARCHAR(20) NOT NULL,
    course INT NOT NULL,
    phone VARCHAR(20)
);

CREATE TABLE teachers (
    id INT AUTO_INCREMENT PRIMARY KEY,
    first_name VARCHAR(50) NOT NULL,
    last_name VARCHAR(50) NOT NULL,
    department VARCHAR(100),
    phone VARCHAR(20)
);

CREATE TABLE sport_sections (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    sport_type VARCHAR(100) NOT NULL,
    teacher_id INT,
    location VARCHAR(100),
    FOREIGN KEY (teacher_id) REFERENCES teachers(id)
        ON DELETE SET NULL
        ON UPDATE CASCADE
);

CREATE TABLE section_members (
    id INT AUTO_INCREMENT PRIMARY KEY,
    student_id INT NOT NULL,
    section_id INT NOT NULL,
    join_date DATE,
    FOREIGN KEY (student_id) REFERENCES students(id)
        ON DELETE CASCADE
        ON UPDATE CASCADE,
    FOREIGN KEY (section_id) REFERENCES sport_sections(id)
        ON DELETE CASCADE
        ON UPDATE CASCADE
);

CREATE TABLE section_schedule (
    id INT AUTO_INCREMENT PRIMARY KEY,
    section_id INT NOT NULL,
    day_of_week VARCHAR(20) NOT NULL,
    start_time TIME NOT NULL,
    end_time TIME NOT NULL,
    location VARCHAR(100),
    FOREIGN KEY (section_id) REFERENCES sport_sections(id)
        ON DELETE CASCADE
        ON UPDATE CASCADE
);

CREATE TABLE competitions (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(150) NOT NULL,
    competition_date DATE NOT NULL,
    location VARCHAR(100),
    description TEXT
);

CREATE TABLE competition_participants (
    id INT AUTO_INCREMENT PRIMARY KEY,
    competition_id INT NOT NULL,
    student_id INT NOT NULL,
    section_id INT,
    result VARCHAR(100),
    FOREIGN KEY (competition_id) REFERENCES competitions(id)
        ON DELETE CASCADE
        ON UPDATE CASCADE,
    FOREIGN KEY (student_id) REFERENCES students(id)
        ON DELETE CASCADE
        ON UPDATE CASCADE,
    FOREIGN KEY (section_id) REFERENCES sport_sections(id)
        ON DELETE SET NULL
        ON UPDATE CASCADE
);

USE faculty_sport;

SHOW TABLES;