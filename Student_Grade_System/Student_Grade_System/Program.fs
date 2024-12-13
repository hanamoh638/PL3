open System
open System.IO
open System.Text.Json
open System.Windows.Forms

// Record type for Student
type Student =
    { ID: int
      Name: string
      Grades: float list
      AverageGrade: float
      HighestGrade: float
      LowestGrade: float }

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



////////////////////////////Grade Management1////////////////////////////////////////

// Calculate the average for a single student's grades
let calculateAverage grades =
    if List.isEmpty grades then
        0.0
    else
        let mutable total = 0.0
        let mutable count = 0

        for grade in grades do
            total <- total + grade
            count <- count + 1

        total / float count


// Calculate the class average for all students
let classAverage () =
    if List.isEmpty studentDatabase then
        0.0
    else
        let mutable totalSum = 0.0
        let mutable totalCount = 0

        for student in studentDatabase do
            let avg = calculateAverage student.Grades
            totalSum <- totalSum + avg
            totalCount <- totalCount + 1

        totalSum / float totalCount

// Calculate pass and fail rates
let passFailRate () =
    let mutable passCount = 0
    let mutable totalCount = 0

    for student in studentDatabase do
        let avg = calculateAverage student.Grades

        if avg >= 50.0 then
            passCount <- passCount + 1

        totalCount <- totalCount + 1

    if totalCount = 0 then
        (0.0, 0.0)
    else
        let passRate = float passCount / float totalCount * 100.0
        let failRate = 100.0 - passRate
        (passRate, failRate)


/////////////////////////////////////////////////////grade management 2///////////////////////
//highest grade in all class
let highestGradeInClass () =
    let allGrades = studentDatabase |> List.collect (fun student -> student.Grades)

    if List.isEmpty allGrades then
        None
    else
        Some(List.max allGrades)

//lowest grade in all class
let lowestGradeInClass () =
    let allGrades = studentDatabase |> List.collect (fun student -> student.Grades)

    if List.isEmpty allGrades then
        None
    else
        Some(List.min allGrades)

//////////////////////////////////////////grade management 3////////////////////////////////

// Get status of student Pass/Fail
let IfPass grades =
    let average = calculateAverage grades
    let status = if average >= 50.0 then "Pass" else "Fail"
    status


//highest grade for student
let highestGrade grades =
    match grades with
    | [] -> System.Double.MinValue
    | head :: tail ->
        let mutable highest = head

        for grade in tail do
            if grade > highest then
                highest <- grade

        highest


//lowest grade for student
let lowestGrade (grades: float list) =
    match grades with
    | [] -> System.Double.MaxValue
    | head :: tail ->
        let mutable lowest = head

        for grade in tail do
            if grade < lowest then
                lowest <- grade

        lowest
