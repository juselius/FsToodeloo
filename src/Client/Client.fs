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
    | Add y -> createEntry model y
    | Delete y -> deleteEntry model y
    | Update y -> updateEntry model y 
    | Init (Ok x) -> 
        let todo = x |> List.map (fun t -> (t.taskId, t)) |> Map.ofList
        { defaultModel with entries = todo }, Cmd.none
    | Init (Error x) -> model, (Cmd.ofMsg (NotifyError x.Message)) 
    | NotifyError err -> { model with errorMsg = Some err }, Cmd.none
    | ClearError -> { model with errorMsg = None }, Cmd.none
    | HandleTask x -> handleTaskUpdate x model
    | ApiCall x -> apiCallHandler x model

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