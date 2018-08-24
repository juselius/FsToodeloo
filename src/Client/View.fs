module Toodeloo.View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Fulma
open Toodeloo.Model
open Toodeloo.Shared

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
          Input.Value model.createForm.task
          ] 
        ]
        Field.div [] [ Label.label [] [ str "Priority" ] ]
        Control.div [] [ Input.number [
          Input.OnChange (fun e -> dispatch' (UpdatePri (int e.Value)))
          Input.Placeholder "0" 
          Input.Value (string model.createForm.priority)
          ] 
        ]
        Field.div [] [ Label.label [] [ str "Due" ] ]
        Control.div [] [ Input.date [
          Input.OnChange (fun e -> dispatch' (UpdateDue (System.DateTime.Parse e.Value)))
          Input.Placeholder "date" 
          Input.Value (string model.createForm.due)
          ] 
        ]
        Field.div [] [ Label.label [] [ str "" ] ]
        Control.div [] [ button "Add entry" (fun _ -> dispatch (Add model.createForm)) ]
    ]

// Add a double click event to each editable td
// It would be better to make the whole row double clickable
let clickToEdit t txt (dispatch : Msg -> unit) = 
    td [
        OnDoubleClick (fun _ -> dispatch <| HandleTask (EditTask t.taskId))
    ] [ str txt ]

let showTaskTable (model : Model) (dispatch : Msg -> unit) =
    let editable task txt elements =
        match model.isEditing with
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
                Input.OnChange (fun e -> 
                    dispatch <| HandleTask (
                        UpdateDue <| System.DateTime.Parse e.Value)
                    )
          ]] 
    let pri t = 
        editable t (string t.priority) [ Input.number [ 
                Input.DefaultValue (string t.priority)
                Input.OnChange (fun e -> dispatch <| HandleTask (UpdatePri <| int e.Value))
          ]] 
    let button i =
            match model.isEditing with
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
