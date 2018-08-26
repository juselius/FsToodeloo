module Toodeloo.Client

open Elmish
open Elmish.React
open Toodeloo.View
open Toodeloo.Model

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Create y -> createEntry y model 
    | Read (Ok x) -> 
        let todo = x |> List.map (fun t -> (t.taskId, t)) |> Map.ofList
        { defaultModel with entries = todo }, Cmd.none
    | Read (Error x) -> model, (Cmd.ofMsg (NotifyError x.Message)) 
    | Update msg -> updateEntry msg model 
    | Delete y -> deleteEntry y model 
    | NotifyError err -> { model with errorMsg = Some err }, Cmd.none
    | ApiCall msg -> apiCallHandler msg model
    | CreateFormMsg msg -> handleCreateForm msg model
    | ClearError -> { model with errorMsg = None }, Cmd.none
    | StartEdit id -> startEdit id model 
    | SaveEdit -> saveEntry model
    | CancelEdit -> cancelEdit model 

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