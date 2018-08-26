module Toodeloo.Model

open Elmish
open Toodeloo.Shared

type Model =
    { entries    : Map<int, Todo>
      createForm : Todo
      errorMsg   : string option
      editing    : int option
    }

let defaultModel = {
    entries = Map.empty
    createForm = Defaults.defaultTodo
    errorMsg = None
    editing = None
    }

// Example how to split Msg into submessages
type UpdateEntryMsg =
| UpdatePri of int
| UpdateTask of string
| UpdateDue of System.DateTime

type ApiCallMsg =
| TaskCreated of Result<unit, string>
| TasksRead of Todo list
| TaskUpdated of Result<unit, string>
| TaskDeleted 

type Msg =
| Read of Result<list<Todo>, exn>
| Create of Todo
| CreateFormMsg of UpdateEntryMsg
| Delete of int
| Update of UpdateEntryMsg
| NotifyError of string
| ClearError
| ApiCall of ApiCallMsg
| StartEdit of int
| SaveEdit 
| CancelEdit 

module Server =
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : ITodoProtocol =
        Proxy.remoting<ITodoProtocol> {
            use_route_builder Route.builder
        }

let notifyExn s (e : exn) = (s + ": " + e.Message) |> Msg.NotifyError

let notifyErr e = Cmd.ofMsg (Msg.NotifyError e)

let init () : Model * Cmd<Msg> =
    let model = defaultModel
    let cmd =
        Cmd.ofAsync
            Server.api.getTodos ()
            (Ok >> Read)
            (Error >> Read)
    model, cmd

let createEntry (x : Todo) (model : Model) =
    let todo = model.entries
    let newId =
        if not todo.IsEmpty then
            Map.toArray todo 
            |> Array.maxBy (fun (a, _) -> a) 
            |> fun (x, _) -> x + 1
        else
            0
    // Example validation, perform any kind of validation here and return
    match x.taskId with
    | 0 -> 
        let todo' = todo |> Map.add newId { x with taskId = newId } 
        let model' = { 
            model with 
                entries = todo' 
                createForm = Shared.Defaults.defaultTodo
            }
        let cmd = 
            Cmd.ofAsync 
                Server.api.createTodo x
                (Msg.ApiCall << ApiCallMsg.TaskCreated)
                (notifyExn "createEntry")
        model', cmd
    | _ -> model, notifyErr "What, what in the b***?"

let deleteEntry (x : int) (model : Model) =
    let model' = { model with entries = Map.remove x model.entries }
    let cmd = 
        Cmd.ofAsync 
            Server.api.deleteTodo x
            (Msg.ApiCall << fun _ -> ApiCallMsg.TaskDeleted)
            (notifyExn "deleteEntry")
    model', cmd

let editEntry (msg : UpdateEntryMsg) (model : Model) =
    let entry = Map.find model.editing.Value model.entries
    match msg with
    | UpdateTask t -> { entry with task = t}
    | UpdatePri p -> { entry with priority = p}
    | UpdateDue d -> { entry with due = Some d}

let updateEntry (msg : UpdateEntryMsg) (model : Model) =
    let entry = editEntry msg model
    let model' = { 
        model with 
            entries = Map.add entry.taskId entry model.entries
        }
    model', Cmd.none

let startEdit id model =
    let model' = { 
        model with 
            editing = Some id 
        }
    model', Cmd.none

let saveEntry model =
    let model' = {
        model with 
            // entries = 
            //     model.entries 
            //     |> Map.add model.editing.Value model.createForm
            // createForm = Shared.Defaults.defaultTodo
            editing = None 
        }
    let cmd = 
        Cmd.ofAsync 
            Server.api.updateTodo model.createForm
            (Msg.ApiCall << ApiCallMsg.TaskUpdated)
            (notifyExn "updateEntry")
    model', cmd

let handleCreateForm (msg : UpdateEntryMsg) (model : Model) =
    let entry = 
        match msg with
        | UpdatePri y -> { model.createForm with priority = y }
        | UpdateDue y ->  { model.createForm with due = Some y }
        | UpdateTask y -> { model.createForm with task = y }
    { model with createForm = entry }, Cmd.none

let apiCallHandler (msg : ApiCallMsg) (model : Model) =
    match msg with
    | TaskCreated b -> 
        match b with
        | Ok () -> model, Cmd.none 
        | Error err -> model, notifyErr ("Create failed: " + err)
    | TasksRead t -> 
        let todo = t |> List.map (fun x -> (x.taskId, x)) |> Map.ofList
        { model with entries = todo }, Cmd.none 
    | TaskUpdated b -> 
        match b with
        | Ok () -> model, Cmd.none 
        | Error err -> model, notifyErr ("Update failed: " + err)
    | TaskDeleted -> model, Cmd.none

let cancelEdit model =
    let model' = { model with editing = None }
    let cmd = 
        Cmd.ofAsync 
            Server.api.getTodos ()
            (Msg.ApiCall << ApiCallMsg.TasksRead)
            (notifyExn "readEntries")
    model', cmd