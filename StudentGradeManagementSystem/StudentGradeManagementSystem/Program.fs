open System
open System.IO
open System.Text.Json

type Student =
    { ID: int
      Name: string
      Grades: float list }



let databaseFilePath = "studentDatabase.json"
let mutable studentDatabase: Student list = []



///////////////////////////////////////////////////////////////////////

let saveDatabaseToFile () =
    let json = JsonSerializer.Serialize(studentDatabase)
    File.WriteAllText(databaseFilePath, json)




let loadDatabaseFromFile () =
    if File.Exists(databaseFilePath) then
        let json = File.ReadAllText(databaseFilePath)
        studentDatabase <- JsonSerializer.Deserialize<Student list>(json)
    else
        printfn "No existing database file found. Starting fresh."


////////////////////////////////////////////////////////////////////////


let studentExists (id: int) =
    studentDatabase |> List.exists (fun student -> student.ID = id)



let addStudent () =
    printfn "enter id"
    let id = Console.ReadLine() |> int
    printfn "enter name"
    let name = Console.ReadLine()
    printfn "enter grade"

    let grades =
        Console.ReadLine()
        |> fun input -> input.Split(',')
        |> Array.map float
        |> List.ofArray

    if studentExists id then
        printfn "Student with ID %d already exists. Cannot add a duplicate." id
    else

        let newStudent =
            { ID = id
              Name = name
              Grades = grades }

        studentDatabase <- newStudent :: studentDatabase
        saveDatabaseToFile ()

////////////////////////////////////////////////////////////////////////////////

let updateStudentInList () =
    printf "Enter the ID of the student to edit: "
    let id = Console.ReadLine() |> int

    let studentIndex = studentDatabase |> List.tryFindIndex (fun s -> s.ID = id)

    match studentIndex with
    | Some index ->

        printf "Enter new name for the student: "
        let name = Console.ReadLine()


        printf "Enter new grades (comma-separated, e.g., 85,90,95): "

        let grades =
            Console.ReadLine()
            |> fun input -> input.Split(',')
            |> Array.map float
            |> List.ofArray

        // Update the student in the list
        studentDatabase <-
            studentDatabase
            |> List.mapi (fun i s ->
                if i = index then
                    { s with Name = name; Grades = grades }
                else
                    s)

        printfn "Student updated successfully."
    | None -> printfn "Student with ID %d not found." id

    
    saveDatabaseToFile ()
    printfn "Student with ID %d has been updated." id



///////////////////////////////////////////////////////////////////////////////////////

// remove student
let removeStudent () =
    printf "Enter the ID of the student to edit: "
    let id = Console.ReadLine() |> int
    studentDatabase <- studentDatabase |> List.filter (fun student -> student.ID <> id)
    saveDatabaseToFile ()
    printfn "Removed student with ID %d" id




///////////////////////////////////

//display

let displayStudents () =
    printfn "Current Student Database:"
    studentDatabase |> List.iter (fun student -> printfn "%A" student)

/////////////////////////////////


loadDatabaseFromFile ()
displayStudents ()
updateStudentInList()
removeStudent ()
displayStudents ()
