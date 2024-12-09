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


    ////////////////////////////Grade Management////////////////////////////////////////

    // Calculate the average for a single student's grades
let calculateAverage grades =
    if List.isEmpty grades then 0.0
    else
        let mutable total = 0.0
        let mutable count = 0
        for grade in grades do
            total <- total + grade
            count <- count + 1
        total / float count

// Get status of student Pass/Fail 
let IfPass grades =
    let average = calculateAverage grades
    let status =
        if average >= 50.0 then
            "Pass"
        else
            "Fail"
    status

// Calculate the class average for all students
let classAverage () =
    if List.isEmpty studentDatabase then 0.0
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
    if totalCount = 0 then (0.0, 0.0)
    else
        let passRate = float passCount / float totalCount * 100.0
        let failRate = 100.0 - passRate
        (passRate, failRate)


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

 //highest grade in all class
let highestGradeInClass () =
    let allGrades = 
     studentDatabase
        |> List.collect (fun student -> student.Grades)  
    if List.isEmpty allGrades then
        None
    else 
        Some(List.max allGrades)  

 //lowest grade in all class
let lowestGradeInClass () =
    let allGrades = 
         studentDatabase 
        |> List.collect (fun student -> student.Grades)  
    if List.isEmpty allGrades then
        None
    else
        Some(List.min allGrades)  


        //////////////////////////////////////////////////////////////////////////////
 ////////////////////////////////Student Management//////////////////////////////////////////

// Add a student
let addStudent (id: int) (name: string) (grades: float list) =
    if studentExists id then
        MessageBox.Show("Student with this ID already exists!") |> ignore
    else
        let avg = calculateAverage grades
        let highest =highestGrade grades
        let lowest =lowestGrade grades
        let newStudent = { ID = id; Name = name; Grades = grades ;AverageGrade=avg;HighestGrade=highest;LowestGrade=lowest }
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
        studentDatabase <- studentDatabase |> List.mapi (fun i s ->
            if i = index then 
                { s with Name = name; Grades = grades; 
                         AverageGrade =  avg; HighestGrade =  highest; LowestGrade =  lowest }
            else s)
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
    studentDatabase |> List.iter (fun student ->
    let gradesString = String.Join(", ", student.Grades)
    listBox.Items.Add(sprintf "ID: %d, Name: %s , Grades: [%s ], StudentAverage:%.2f ,Highest:%.2f ,Lowest:%.2f" student.ID student.Name gradesString student.AverageGrade student.HighestGrade student.LowestGrade) |> ignore
    let avgrade=calculateAverage (student.Grades)
    let avg = classAverage ()
    let (passRate, failRate) = passFailRate ()
    let Status= IfPass(student.Grades)
    let highInClass = highestGradeInClass()
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
    listBox.Items.Add(sprintf "----------------------------------------------------------------------------------------------------------------" ) |> ignore
    )


    // Display student by ID in ListBox
let displayStudentById (id: int) (listBox: ListBox) =
    listBox.Items.Clear()  
    match studentDatabase |> List.tryFind (fun student -> student.ID = id) with
    | Some student ->
        let gradesString = String.Join(", ", student.Grades)
       
        let avg = classAverage ()
        let (passRate, failRate) = passFailRate ()
        let Status= IfPass(student.Grades)
        //listBox.Items.Clear()
        listBox.Items.Add(sprintf "ID: %d, Name: %s , Grades: [%s ], YourAverage:%.2f ,Highest:%.2f ,Lowest:%.2f" student.ID student.Name gradesString student.AverageGrade student.HighestGrade student.LowestGrade) |> ignore
        listBox.Items.Add(sprintf "Status: %s" Status) |> ignore
        listBox.Items.Add(sprintf "Class Average: %.2f" avg) |> ignore
        
        listBox.Items.Add(sprintf "Pass Rate: %.2f%%" passRate) |> ignore
        listBox.Items.Add(sprintf "Fail Rate: %.2f%%" failRate) |> ignore
        
        MessageBox.Show(sprintf "Student found Successfully") |> ignore
    | None -> 
        MessageBox.Show("Student not found!") |> ignore
    
   




 ///////////////////////////////////////////////////////////////////////////////////////////////
// admin Form creation
let createAdminForm () =
    let form = new Form(Text = "Admin", Width = 600, Height = 400)
    form.BackColor <- System.Drawing.Color.Beige
    

    // ListBox to show students
    let studentListBox = new ListBox(Dock = DockStyle.Top, Height = 150)

    //labels
    let idLabel = new Label(Text = "Student ID:", Top = 160, Left = 5, Width = 100, Height = 20)
    let nameLabel = new Label(Text = "Student Name:", Top = 190, Left = 5,Width = 100, Height = 20)
    let gradesLabel = new Label(Text = "Grades (comma):",Top = 220, Left = 5,Width = 100, Height = 20)

    // TextBoxes and Labels for input
    let idTextBox = new TextBox(Top = 160, Left = 100, Width = 130 )
    let nameTextBox = new TextBox(Top = 190, Left = 100, Width = 130)
    let gradesTextBox = new TextBox(Top = 220, Left = 100, Width = 130)

    // Buttons for operations
    let addButton = new Button(Text = "Add Student", Top = 160, Left = 270 , Width = 130, Height = 35)
    let updateButton = new Button(Text = "Update Student", Top = 160, Left = 430 , Width = 130, Height = 35)
    let removeButton = new Button(Text = "Remove Student", Top = 260, Left = 270 , Width = 130, Height = 35)
    let displayButton = new Button(Text = "Display All Students", Top = 210, Left = 430, Width = 130, Height = 35)
    let removeAllButton = new Button(Text = "Remove All Students", Top = 260, Left = 430, Width = 130, Height = 35)
    let displayStudent = new Button(Text = "Display Student", Top = 210, Left = 270, Width = 130, Height = 35)
    let clearButton = new Button(Text = "Clear ", Top = 310, Left = 270, Width = 130, Height = 35)


    // Button Click Events
    addButton.Click.Add(fun _ ->
        let id = int idTextBox.Text
        let name = nameTextBox.Text
        let grades = gradesTextBox.Text.Split(',') |> Array.map float |> List.ofArray
        addStudent id name grades
        displayStudentById id studentListBox
        idTextBox.Text <- ""
        nameTextBox.Text <- ""
        gradesTextBox.Text <- ""
    )

    updateButton.Click.Add(fun _ ->
        let id = int idTextBox.Text
        let name = nameTextBox.Text
        let grades = gradesTextBox.Text.Split(',') |> Array.map float |> List.ofArray
        updateStudent id name grades
        displayStudents studentListBox  
        idTextBox.Text <- ""
        nameTextBox.Text <- ""
        gradesTextBox.Text <- ""
    )

    removeButton.Click.Add(fun _ ->
        let id = int idTextBox.Text
        removeStudent id
        displayStudents studentListBox
        idTextBox.Text <- ""
        nameTextBox.Text <- ""
        gradesTextBox.Text <- ""
    )

    displayButton.Click.Add(fun _ ->
        displayStudents studentListBox
    )

    removeAllButton.Click.Add(fun _ ->
        removeAllStudents ()  
        displayStudents studentListBox  
    )

    displayStudent.Click.Add(fun _ ->
        let id = int idTextBox.Text
        displayStudentById id studentListBox  
        idTextBox.Text <- ""
        nameTextBox.Text <- ""
        gradesTextBox.Text <- ""
    )

     
    clearButton.Click.Add(fun _ ->
    studentListBox.Items.Clear() 
    MessageBox.Show("window cleared") |> ignore
      )


    addButton.BackColor <- System.Drawing.Color.BurlyWood
    updateButton.BackColor <- System.Drawing.Color.BurlyWood
    displayStudent.BackColor <- System.Drawing.Color.BurlyWood
    removeButton.BackColor <- System.Drawing.Color.BurlyWood
    displayButton.BackColor <- System.Drawing.Color.ForestGreen
    removeAllButton.BackColor <- System.Drawing.Color.Brown
    clearButton.BackColor <- System.Drawing.Color.BurlyWood




    //add controls to the form
    form.Controls.Add(studentListBox)
    form.Controls.Add(idTextBox)
    form.Controls.Add(nameTextBox)
    form.Controls.Add(gradesTextBox)
    form.Controls.Add(addButton)
    form.Controls.Add(updateButton)
    form.Controls.Add(removeButton)
    form.Controls.Add(displayButton)
    form.Controls.Add(idLabel)
    form.Controls.Add(nameLabel)
    form.Controls.Add(gradesLabel)
    form.Controls.Add(removeAllButton)
    form.Controls.Add(displayStudent)
    form.Controls.Add(clearButton)
    form




    //////////////////////////////////////////GUI//////////////////////////////////////////
let createViewerForm () =
    let form = new Form(Text = "Viewr", Width = 600, Height = 400)
    form.BackColor <- System.Drawing.Color.Beige
    // ListBox to display students in the viewer window
    let studentListBox = new ListBox(Dock = DockStyle.Top, Height = 150)
    form.Controls.Add(studentListBox)
    
    let idLabel = new Label(Text = "Student ID:", Top = 215, Left = 5, Width = 90, Height = 20)
    let idTextBox = new TextBox(Top = 215, Left = 100, Width = 140 )
    let displayStudent = new Button(Text = "Display Student", Top = 210, Left = 270, Width = 130, Height = 35)

    displayStudent.Font<- new System.Drawing.Font("Arial", 9.0f, System.Drawing.FontStyle.Bold)
    displayStudent.BackColor <- System.Drawing.Color.BurlyWood

    form.Controls.Add(idLabel)
    form.Controls.Add(idTextBox)
    form.Controls.Add(displayStudent)

    displayStudent.Click.Add(fun _ ->
        let id = int idTextBox.Text
        displayStudentById id studentListBox  
        idTextBox.Text <- ""
     
    )
    form
    
    //////////////////////////////////admin authentcation////////////////////////////////
    
// Admin authentication function
let createAdminAuthenticationForm () =

    let adminUsername = "admin"
    let adminPassword = "123"

    let form = new Form(Text = "Admin Authentication", Width = 600, Height = 400)
    form.BackColor <- System.Drawing.Color.Beige
    
    let usernameLabel = new Label(Text = "Username:", Top = 20, Left = 50)
    let passwordLabel = new Label(Text = "Password:", Top = 60, Left = 50)
    let usernameTextBox = new TextBox(Top = 20, Left = 160, Width = 150)
    let passwordTextBox = new TextBox(Top = 60, Left = 160, Width = 150, UseSystemPasswordChar = true)

    usernameLabel.Font<- new System.Drawing.Font("Arial", 12.0f, System.Drawing.FontStyle.Bold)
    passwordLabel.Font<- new System.Drawing.Font("Arial", 12.0f, System.Drawing.FontStyle.Bold)
    
    let loginButton = new Button(Text = "Login", Top = 100, Left = 180,  Width = 130, Height = 35)
    loginButton.Click.Add(fun _ ->
        if usernameTextBox.Text = adminUsername && passwordTextBox.Text = adminPassword then
            MessageBox.Show("Login successful!", "Success") |> ignore
            form.Close() // Close the authentication form

            // Show Admin Form
            let adminForm = createAdminForm()
            adminForm.ShowDialog() |> ignore
        else
            MessageBox.Show("Invalid admin.", "Error") |> ignore
    )
    
    loginButton.Font<- new System.Drawing.Font("Arial", 12.0f, System.Drawing.FontStyle.Bold)
    loginButton.BackColor <- System.Drawing.Color.BurlyWood

    // Add controls to the form
    form.Controls.Add(usernameLabel)
    form.Controls.Add(passwordLabel)
    form.Controls.Add(usernameTextBox)
    form.Controls.Add(passwordTextBox)
    form.Controls.Add(loginButton)
    
    form



    // Main Form creation for the choice window
let createMainForm () =
    let form = new Form(Text = "Student Management System", Width = 600, Height = 400)
    form.BackColor <- System.Drawing.Color.Beige

    let titlelabel = new Label(Text = "Student Management System", Top = 100, Left = 130, Width = 600, Height = 100)

    titlelabel.Font <- new System.Drawing.Font("Arial", 18.0f, System.Drawing.FontStyle.Bold)
    
    // Admin Button
    let adminButton = new Button(Text = "Admin", Width = 150, Height = 50, Top = 200, Left = 100)
    adminButton.Click.Add(fun _ ->
        let adminForm = createAdminAuthenticationForm()
        adminForm.ShowDialog() |> ignore
    )
    adminButton.Font<- new System.Drawing.Font("Arial", 11.0f, System.Drawing.FontStyle.Bold)
    adminButton.BackColor <- System.Drawing.Color.BurlyWood

    // Viewer Button
    let viewerButton = new Button(Text = "Viewer", Width = 150, Height = 50, Top = 200, Left = 350)
    viewerButton.Click.Add(fun _ ->
        let viewerForm = createViewerForm ()
        viewerForm.ShowDialog() |> ignore
    )
    viewerButton.Font<- new System.Drawing.Font("Arial", 11.0f, System.Drawing.FontStyle.Bold)
    viewerButton.BackColor <- System.Drawing.Color.BurlyWood
    

    // Adding buttons to the form
    form.Controls.Add(adminButton)
    form.Controls.Add(viewerButton)
    form.Controls.Add(titlelabel)

    form


// Application entry point with STAThread
[<STAThread>]
[<EntryPoint>]
let main argv =
    loadDatabaseFromFile ()
    let form = createMainForm ()
    Application.Run(form)
    0
