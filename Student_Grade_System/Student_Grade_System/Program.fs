open System
open System.IO
open System.Text.Json
open System.Windows.Forms

// Record type for Student
type Student = { ID: int; Name: string; Grades: float list ; AverageGrade: float  ; HighestGrade:float  ;LowestGrade:float }

// Path to store the database file
let databaseFilePath = "studentDatabase.json"

// Mutable student database
let mutable studentDatabase: Student list = []

// Load the database from the file
let loadDatabaseFromFile () =
    if File.Exists(databaseFilePath) then
        let json = File.ReadAllText(databaseFilePath)
        studentDatabase <- JsonSerializer.Deserialize<Student list>(json)
    else
        printfn "No existing database file found. Starting fresh."

// Save the student database to a file
let saveDatabaseToFile () =
    let json = JsonSerializer.Serialize(studentDatabase)
    File.WriteAllText(databaseFilePath, json)

// Check if a student already exists by ID
let studentExists (id: int) =
    studentDatabase |> List.exists (fun student -> student.ID = id)


 