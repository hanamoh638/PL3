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
////////////////////////////////Student Management//////////////////////////////////////////

// Add a student
let addStudent (id: int) (name: string) (grades: float list) =
    if studentExists id then
        MessageBox.Show("Student with this ID already exists!") |> ignore
    else
        let avg = calculateAverage grades
        let highest = highestGrade grades
        let lowest = lowestGrade grades

        let newStudent =
            { ID = id
              Name = name
              Grades = grades
              AverageGrade = avg
              HighestGrade = highest
              LowestGrade = lowest }

        studentDatabase <- newStudent :: studentDatabase
        saveDatabaseToFile ()
        MessageBox.Show("Student added successfully.") |> ignore

// update student
let updateStudent (id: int) (name: string) (grades: float list) =
    let studentIndex = studentDatabase |> List.tryFindIndex (fun s -> s.ID = id)

    match studentIndex with
    | Some index ->
        // Recalculate the grades
        let avg = calculateAverage grades
        let highest = List.max grades
        let lowest = List.min grades

        // Update the student record with new values
        studentDatabase <-
            studentDatabase
            |> List.mapi (fun i s ->
                if i = index then
                    { s with
                        Name = name
                        Grades = grades
                        AverageGrade = avg
                        HighestGrade = highest
                        LowestGrade = lowest }
                else
                    s)

        saveDatabaseToFile ()
        MessageBox.Show("Student updated successfully.") |> ignore
    | None -> MessageBox.Show("Student not found!") |> ignore


// remove a student by ID
let removeStudent (id: int) =
    studentDatabase <- studentDatabase |> List.filter (fun student -> student.ID <> id)
    saveDatabaseToFile ()
    MessageBox.Show("Student removed successfully.") |> ignore



// Remove all students
let removeAllStudents () =
    studentDatabase <- []
    saveDatabaseToFile ()
    MessageBox.Show("All students removed successfully.") |> ignore


// Display all students in a ListBox
let displayStudents (listBox: ListBox) =
    listBox.Items.Clear()

    studentDatabase
    |> List.iter (fun student ->
        let gradesString = String.Join(", ", student.Grades)

        listBox.Items.Add(
            sprintf
                "ID: %d, Name: %s , Grades: [%s ], StudentAverage:%.2f ,Highest:%.2f ,Lowest:%.2f"
                student.ID
                student.Name
                gradesString
                student.AverageGrade
                student.HighestGrade
                student.LowestGrade
        )
        |> ignore

        let avgrade = calculateAverage (student.Grades)
        let avg = classAverage ()
        let (passRate, failRate) = passFailRate ()
        let Status = IfPass(student.Grades)
        let highInClass = highestGradeInClass ()
        let lowInClass = lowestGradeInClass ()
        listBox.Items.Add(sprintf "Status: %s" Status) |> ignore
        listBox.Items.Add(sprintf "Class Average: %.2f" avg) |> ignore

        match highInClass with
        | Some high -> listBox.Items.Add(sprintf "The highest grade in class: %.2f" high) |> ignore
        | None -> listBox.Items.Add("No grades available for highest grade.") |> ignore

        match lowInClass with
        | Some low -> listBox.Items.Add(sprintf "The lowest grade in class: %.2f" low) |> ignore
        | None -> listBox.Items.Add("No grades available for lowest grade.") |> ignore

        listBox.Items.Add(sprintf "Pass Rate: %.2f%%" passRate) |> ignore
        listBox.Items.Add(sprintf "Fail Rate: %.2f%%" failRate) |> ignore

        listBox.Items.Add(
            sprintf
                "----------------------------------------------------------------------------------------------------------------"
        )
        |> ignore)


// Display student by ID in ListBox
let displayStudentById (id: int) (listBox: ListBox) =
    listBox.Items.Clear()

    match studentDatabase |> List.tryFind (fun student -> student.ID = id) with
    | Some student ->
        let gradesString = String.Join(", ", student.Grades)

        let avg = classAverage ()
        let (passRate, failRate) = passFailRate ()
        let Status = IfPass(student.Grades)
        //listBox.Items.Clear()
        listBox.Items.Add(
            sprintf
                "ID: %d, Name: %s , Grades: [%s ], YourAverage:%.2f ,Highest:%.2f ,Lowest:%.2f"
                student.ID
                student.Name
                gradesString
                student.AverageGrade
                student.HighestGrade
                student.LowestGrade
        )
        |> ignore

        listBox.Items.Add(sprintf "Status: %s" Status) |> ignore
        listBox.Items.Add(sprintf "Class Average: %.2f" avg) |> ignore

        listBox.Items.Add(sprintf "Pass Rate: %.2f%%" passRate) |> ignore
        listBox.Items.Add(sprintf "Fail Rate: %.2f%%" failRate) |> ignore

        MessageBox.Show(sprintf "Student found Successfully") |> ignore
    | None -> MessageBox.Show("Student not found!") |> ignore
