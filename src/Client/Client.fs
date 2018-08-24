module Client

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Fulma
open Toodeloo.Shared

type Model =
    { entries : list<Todo>
      addForm : Todo
      updateForm : Todo
      errorMsg : option<string>
      editing : option<TaskId>
    }

let defaultModel = {
    entries = [] 
    addForm = Defaults.defaultTodo
    updateForm = Defaults.defaultTodo
    errorMsg = None
    editing = None
    }

// Example how to split Msg into submessages
type HandleTaskMsg =
| UpdatePri of int
| UpdateTask of string
| UpdateDue of System.DateTime
| EditTask of int
| EndEdit 

type Msg =
| Add of Todo
| Delete of int
| Update of Todo
| Init of Result<list<Todo>, exn>
| HandleTask of HandleTaskMsg
| NotifyError of string
| ClearError
| NoOp

module Server =
    open Toodeloo.Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : ITodoProtocol =
        Proxy.remoting<ITodoProtocol> {
            use_route_builder Route.builder
        }

let inline doAsync f x =
    Cmd.ofAsync
        f x
        (fun _ -> Msg.NoOp)
        ((fun x -> x.Message) >> Msg.NotifyError)

let init () : Model * Cmd<Msg> =
    let model = defaultModel
    let cmd =
        Cmd.ofAsync
            Server.api.getTodos ()
            (Ok >> Init)
            (Error >> Init)
    model, cmd

let addEntry (model : Model) (x : Todo) =
    let todo = model.entries
    let newId =
        if todo.Length > 0 then
            todo |> List.map (fun a -> a.taskId) |>  List.max |> (+) 1
        else
            0
    // Example validation, perform any kind of validation here and return
    match x.taskId with
    | 0 -> 
        let todo' = { x with taskId = newId } :: todo
        let model' = { model with entries = todo' }
        let cmd =  doAsync Server.api.createTodo x
        model', cmd
    | _ -> model, (Cmd.ofMsg (NotifyError "What, what in the b***?"))

// Both delete and update are complicated by using lists
// Set would be a better choice 
let deleteEntry (model : Model) (x : int) =
    List.filter (fun a -> a.taskId <> x) model.entries
    |> fun e ->  
        let model' = { model with entries = e }
        let cmd = doAsync Server.api.deleteTodo x
        model', cmd

let updateEntry (model : Model) (x : Todo) =
    let removeEntry x =
        List.filter (fun b -> b.taskId <> x.taskId) model.entries
    List.tryFind (fun a -> a.taskId = x.taskId) model.entries
    |> function 
    | Some _ ->
        let model' = { model with entries = x :: removeEntry x }
        let cmd =  doAsync Server.api.createTodo x
        model', cmd
    | None -> model, (Cmd.ofMsg (NotifyError "Update failed")) 

let editTask model taskId = 
    match model.editing with
    | Some _ -> 
        updateEntry model model.updateForm
        |> fun (m, cmd) ->  
            { m with 
                updateForm = Defaults.defaultTodo
                editing = None 
            }, cmd
    | None -> 
        let todo = 
            match List.tryFind (fun a -> a.taskId = taskId) model.entries with
            | Some x -> x
            | None-> Defaults.defaultTodo
        { model with 
            updateForm = todo
            editing = Some taskId
        }, Cmd.none 

// Reuse of Update messages complicates the update function
// It would be better to have explicit messages for adding and updating,
// and it would make the code easier to read and undestand. 
let handleTaskUpdate (msg : HandleTaskMsg) (model : Model) =
    let form = 
        match model.editing with
        | Some _ -> model.updateForm
        | None   -> model.addForm
    let setForm x = 
        match model.editing with
        | Some _ -> { model with updateForm = x }
        | None   -> { model with addForm = x }
    match msg with
    | UpdateTask y -> setForm { form with task = y }, Cmd.none
    | UpdatePri y ->  setForm { form with priority = y }, Cmd.none 
    | UpdateDue y ->  setForm { form with due = Some y }, Cmd.none
    | EditTask y -> editTask model y
    | EndEdit -> editTask model (form.taskId)

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    // let model' =
    match msg with
    | Add y -> addEntry model y
    | Delete y -> deleteEntry model y
    | Update y -> updateEntry model y 
    | Init (Ok x) -> { defaultModel with entries = x }, Cmd.none
    | Init (Error x) -> model, (Cmd.ofMsg (NotifyError x.Message)) 
    | NotifyError err -> { model with errorMsg = Some err }, Cmd.none
    | ClearError -> { model with errorMsg = None }, Cmd.none
    | HandleTask x -> handleTaskUpdate x model
    // match model' with
    // | Ok x -> x, Cmd.none
    // | Error err -> model, (Cmd.ofMsg (NotifyError err))

let safeComponents =
    let intersperse sep ls =
        List.foldBack (fun x -> function
            | [] -> [x]
            | xs -> x::sep::xs) ls []
    let components =
        [
            "Saturn", "https://saturnframework.github.io/docs/"
            "Fable", "http://fable.io"
            "Elmish", "https://elmish.github.io/elmish/"
            "Fulma", "https://mangelmaxime.github.io/Fulma"
            "Fable.Remoting", "https://zaid-ajaj.github.io/Fable.Remoting/"
        ]
        |> List.map (fun (desc,link) -> a [ Href link ] [ str desc ] )
        |> intersperse (str ", ")
        |> span [ ]
    p []
        [ strong [] [ str "Toodeloo" ]
          str " powered by: "
          components ]

let navbar =
    Navbar.navbar [ Navbar.Color IsDark ] [
        Navbar.Item.div [ ] [
            Heading.h3
                [ Heading.Modifiers [ Modifier.TextColor IsWhite ] ]
                [ str "Toodeloo" ]
        ]
    ]

let button txt onClick =
    Button.button [
        Button.IsFullWidth
        Button.Color IsPrimary
        Button.OnClick onClick
    ] [ str txt ]

let formAddTask (model : Model) (dispatch : Msg -> unit) =
    let dispatch' = HandleTask >> dispatch
    p [] [
        Field.div [] [ Label.label [] [ str "Task" ] ]
        Control.div [] [ Input.text [
          Input.OnChange (fun e -> dispatch' (UpdateTask e.Value))
          Input.Placeholder "Todo" 
          Input.Value model.addForm.task
          ] 
        ]
        Field.div [] [ Label.label [] [ str "Priority" ] ]
        Control.div [] [ Input.number [
          Input.OnChange (fun e -> dispatch' (UpdatePri (int e.Value)))
          Input.Placeholder "0" 
          Input.Value (string model.addForm.priority)
          ] 
        ]
        Field.div [] [ Label.label [] [ str "Due" ] ]
        Control.div [] [ Input.date [
          Input.OnChange (fun e -> dispatch' (UpdateDue (System.DateTime.Parse e.Value)))
          Input.Placeholder "date" 
          Input.Value (string model.addForm.due)
          ] 
        ]
        Field.div [] [ Label.label [] [ str "" ] ]
        Control.div [] [ button "Add entry" (fun _ -> dispatch (Add model.addForm)) ]
    ]

// Add a double click event to each editable td
// It would be better to make the whole row double clickable
let clickToEdit t txt (dispatch : Msg -> unit) = 
    td [
        OnDoubleClick (fun _ -> dispatch <| HandleTask (EditTask t.taskId))
    ] [ str txt ]

let showTaskTable (model : Model) (dispatch : Msg -> unit) =
    let editable task txt elements =
        match model.editing with
        | Some n when n = task.taskId -> td [] elements
        | Some _ -> clickToEdit task txt dispatch
        | None   -> clickToEdit task txt dispatch
    let task t = 
        editable t t.task [ Input.text [ 
                Input.DefaultValue t.task
                Input.OnChange (fun e -> dispatch <| HandleTask (UpdateTask e.Value))
          ]] 
    let due t = 
        editable t (string t.due) [ Input.date [ 
                Input.DefaultValue (string t.due)
                Input.OnChange (fun e -> dispatch <| HandleTask (UpdateDue <| System.DateTime.Parse e.Value))
          ]] 
    let pri t = 
        editable t (string t.priority) [ Input.number [ 
                Input.DefaultValue (string t.priority)
                Input.OnChange (fun e -> dispatch <| HandleTask (UpdatePri <| int e.Value))
          ]] 
    let button i =
            match model.editing with
            | Some n when n = i.taskId ->
                td [] [
                    Button.button [
                        Button.Color IsSuccess
                        Button.IsOutlined
                        Button.OnClick (fun _ -> dispatch <| HandleTask EndEdit)
                    ] [ str "Save" ]
                ]
            | _ ->
                td [] [
                    Button.button [
                        Button.Color IsDanger
                        Button.IsOutlined
                        Button.OnClick (fun _ -> dispatch <| Delete i.taskId)
                    ] [ str "X" ]
                ]
    let cols = [ "Id"; "Priority"; "Task"; "Due"; "Delete" ]
    Table.table [] [
        thead [] [
            for i in cols do yield td [] [str i]
        ]
        tbody [] [
            for i in model.entries do
                yield tr [] [
                    td [] [ str (string i.taskId) ]
                    pri i
                    task i
                    due i
                    button i
                ]
          ]
      ]

let errorNotifier (model : Model) (dispatch : Msg -> unit) =
    match model.errorMsg with
    | Some err ->
          Notification.notification [ Notification.Color IsDanger ] [
              Notification.delete [ GenericOption.Props
                [ OnClick (fun _ -> dispatch ClearError)] ] []
              str err
           ]
    | None -> div [] []

let view (model : Model) (dispatch : Msg -> unit) =
    div [] [
        navbar
        errorNotifier model dispatch
        Container.container [] [
            Box.box' [           ] [
                Heading.h3 [] [ str "My Toodeloo" ]
                formAddTask model dispatch
            ]
            Box.box' [] [ showTaskTable model dispatch ]
            Content.content [] [
                Button.button [ 
                    Button.Color IsDanger
                    Button.OnClick (fun _ -> dispatch (NotifyError "fooo"))
                ] [ str "Generate error" ]
            ]
            Box.box' [] [ str (string model) ]
        ]
        Footer.footer [] [
            Content.content [ Content.Modifiers [
                Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
            ] [ safeComponents ]
        ]
    ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
