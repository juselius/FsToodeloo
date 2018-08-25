module Toodeloo.Model

open Elmish
open Toodeloo.Shared

type Model =
    { entries : Map<int, Todo>
      createForm : Todo
      editForm : Todo
      errorMsg : option<string>
      isEditing : option<TaskId>
    }

let defaultModel = {
    entries = Map.empty
    createForm = Defaults.defaultTodo
    editForm = Defaults.defaultTodo
    errorMsg = None
    isEditing = None
    }

// Example how to split Msg into submessages
type HandleTaskMsg =
| UpdatePri of int
| UpdateTask of string
| UpdateDue of System.DateTime
| EditTask of int
| EndEdit 

type ApiCallMsg =
| TaskCreated of Result<unit, string>
| TasksRead of Todo list
| TaskUpdated of Result<unit, string>
| TaskDeleted 

type Msg =
| Add of Todo
| Delete of int
| Update of Todo
| Init of Result<list<Todo>, exn>
| HandleTask of HandleTaskMsg
| NotifyError of string
| ClearError
| ApiCall of ApiCallMsg

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
            (Ok >> Init)
            (Error >> Init)
    model, cmd

let createEntry (model : Model) (x : Todo) =
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
        let todo' = Map.add newId { x with taskId = newId } todo
        let model' = { model with entries = todo' }
        let cmd = 
            Cmd.ofAsync 
                Server.api.createTodo x
                (Msg.ApiCall << ApiCallMsg.TaskCreated)
                (notifyExn "createEntry")
        model', cmd
    | _ -> model, notifyErr "What, what in the b***?"

// Both delete and update are complicated by using lists
// Set would be a better choice 
let deleteEntry (model : Model) (x : int) =
    let model' = { model with entries = Map.remove x model.entries }
    let cmd = 
        Cmd.ofAsync 
            Server.api.deleteTodo x
            (Msg.ApiCall << fun _ -> ApiCallMsg.TaskDeleted)
            (notifyExn "deleteEntry")
    model', cmd

let updateEntry (model : Model) (x : Todo) =
    let entries' = Map.add x.taskId x model.entries
    let model' = { model with entries = entries' }
    let cmd = 
        Cmd.ofAsync 
            Server.api.createTodo x
            (Msg.ApiCall << ApiCallMsg.TaskUpdated)
            (notifyExn "updateEntry")
    model', cmd

let editTask model taskId = 
    match model.isEditing with
    | Some _ -> 
        updateEntry model model.editForm
        |> fun (m, cmd) ->  
            { m with 
                editForm = Defaults.defaultTodo
                isEditing = None 
            }, cmd
    | None -> 
        let todo = 
            match Map.tryFind taskId model.entries with
            | Some x -> x
            | None-> Defaults.defaultTodo
        { model with 
            editForm = todo
            isEditing = Some taskId
        }, Cmd.none 

// Reuse of Update messages complicates the update function
// It would be better to have explicit messages for adding and updating,
// and it would make the code easier to read and undestand. 
let handleTaskUpdate (msg : HandleTaskMsg) (model : Model) =
    let form = 
        match model.isEditing with
        | Some _ -> model.editForm
        | None   -> model.createForm
    let setForm x = 
        match model.isEditing with
        | Some _ -> { model with editForm = x }
        | None   -> { model with createForm = x }
    match msg with
    | UpdateTask y -> setForm { form with task = y }, Cmd.none
    | UpdatePri y ->  setForm { form with priority = y }, Cmd.none 
    | UpdateDue y ->  setForm { form with due = Some y }, Cmd.none
    | EditTask y -> editTask model y
    | EndEdit -> editTask model (form.taskId)

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