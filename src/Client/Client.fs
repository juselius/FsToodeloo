module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared

open Fulma


type Model = 
    { entries : list<Todo>
      todoForm : option<Todo>
      errorMsg : option<string>
    }

type AddTaskMsg =
| UpdatePri of int
| UpdateTask of string
| UpdateDue of System.DateTime

type Msg =
| Add of option<Todo>
| Delete of int
| Update of Todo
| Init of Result<list<Todo>, exn>
| AddTaskMsg of AddTaskMsg
| NotifyError of string
| ClearError

module Server =

    open Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : ITodoProtocol =
        Proxy.remoting<ITodoProtocol> {
            use_route_builder Route.builder
        }

let defaultTodo = 
    { taskId = 0
      priority = 0
      task = "" 
      due = System.DateTime.Now
    }


let init () : Model * Cmd<Msg> =
    let model = { 
        entries = [] 
        todoForm = None 
        errorMsg = None
    }
    let cmd =
        Cmd.ofAsync
            Server.api.getInitTodo
            ()
            (Ok >> Init)
            (Error >> Init)
    model, cmd

let addEntry (model : Model) (x : option<Todo>) = 
    let todo = model.entries
    let newId = 
        if todo.Length > 0 then
            todo |> List.map (fun a -> a.taskId) |>  List.max |> (+) 1
        else
            0
    // perform any kind of validation here 
    let validate = function
        | Some task -> 
            if task.taskId = 0 then
                Ok { task with taskId = newId } 
            else 
                Error "Invalid task id"
        | None -> Error "What what in the b***?"
    validate x |> Result.map (fun t -> 
        { model with 
            entries = t  :: todo 
            todoForm = None
        })

let deleteEntry (model : Model) (x : int) = 
    List.filter (fun a -> a.taskId <> x) model.entries 
    |> fun e -> Ok { model with entries = e }

let updateEntry (model : Model) (x : Todo) = 
    let removeEntry x = 
        List.filter (fun b -> b.taskId = x.taskId) model.entries 
    List.tryFind (fun a -> a.taskId = x.taskId) model.entries 
    |> Option.map (fun t -> 
        Ok { model with entries = t :: removeEntry x })
        
let updateAddTask (msg : AddTaskMsg) (model : Model) =
    let form = 
        match model.todoForm with
        | Some x -> x
        | None -> defaultTodo
    let setForm x = { model with todoForm = Some x } 
    match msg with
    |  UpdateTask y -> setForm { form with task = y } |> Ok
    |  UpdatePri y ->  setForm { form with priority = y } |> Ok
    |  UpdateDue y ->  setForm { form with due = y } |> Ok

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    let model' =
        match msg with
        | Add y -> addEntry model y
        | Delete y -> deleteEntry model y
        | Update y -> 
            match updateEntry model y with
            | Some v -> v
            | None -> Ok model
        | Init (Ok x) -> Ok { 
                entries = x
                todoForm = None
                errorMsg = None
            }
        | Init (Error x) -> Error x.Message
        | NotifyError err -> Ok { model with errorMsg = Some err }
        | ClearError -> Ok { model with errorMsg = None }
        | AddTaskMsg x -> updateAddTask x model
    match model' with
    | Ok x -> x, Cmd.none
    | Error err -> model, (Cmd.ofMsg (NotifyError err))

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

    p [ ]
        [ strong [] [ str "TooDoo" ]
          str " powered by: "
          components ]

let navbar = 
    Navbar.navbar [ Navbar.Color IsDark ] [ 
        Navbar.Item.div [ ] [ 
            Heading.h3 
                [ Heading.Modifiers [ Modifier.TextColor IsWhite ] ]
                [ str "Jupiter TooDoo" ] 
        ] 
    ] 

let button txt onClick =
    Button.button [ 
        Button.IsFullWidth
        Button.Color IsPrimary
        Button.OnClick onClick 
    ] [ str txt ]

let formAddTask (model : Model) (dispatch : Msg -> unit) =
    let dispatch' = AddTaskMsg >> dispatch 
    p [] [ 
        Field.div [] [ Label.label [] [ str "Task" ] ]
        Control.div [] [ Input.text [ 
          Input.OnChange (fun e -> dispatch' (UpdateTask e.Value))
          Input.Placeholder "Todo" ] ]
        Field.div [] [ Label.label [] [ str "Priority" ] ]
        Control.div [] [ Input.number [ 
          Input.OnChange (fun e -> dispatch' (UpdatePri (int e.Value)))
          Input.Placeholder "0" ] ]
        Field.div [] [ Label.label [] [ str "Due" ] ]
        Control.div [] [ Input.date [ 
          Input.OnChange (fun e -> dispatch' (UpdateDue (System.DateTime.Parse e.Value)))
          Input.Placeholder "date" ] ]
        Field.div [] [ Label.label [] [ str "" ] ]
        Control.div [] [ button "Add entry" (fun _ -> dispatch (Add model.todoForm)) ]
    ]

let showTaskTable (model : Model) (dispatch : Msg -> unit) = 
      Table.table [] [
          thead [] [ 
              td [] [ str "Id" ] 
              td [] [ str "Priority" ] 
              td [] [ str "Task" ] 
              td [] [ str "Due" ] 
              td [] [ str "Delete" ] 
          ]
          tbody [] [
            for i in model.entries do
                yield (tr [] [ 
                    td [] [ str (string i.taskId) ] 
                    td [] [ str (string i.priority) ] 
                    td [] [ str i.task ] 
                    td [] [ str (string i.due) ] 
                    td [] [
                        Button.button [ 
                            Button.Color IsDanger
                            Button.IsOutlined 
                            Button.OnClick (fun _ -> dispatch <| Delete i.taskId)
                        ] [ str "X" ] 
                    ]
                ])
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
          Content.content [ Content.Modifiers [ 
              Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] 
          ] [ 
              Heading.h3 [] [ str "My Toodeloo" ] 
              formAddTask model dispatch
        ]
      ]
    
      Container.container [] [
          Content.content [] [
              showTaskTable model dispatch
              p [] [ str (string model) ]  
              Button.button [
                  Button.Color IsDanger
                  Button.OnClick (fun _ -> dispatch (NotifyError "fooo"))
              ] [ str "Error" ] 
          ]
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
