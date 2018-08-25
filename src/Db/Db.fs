module Toodeloo.Db

open FSharp.Data.Sql
open Toodeloo.Shared

[<Literal>]
let private ResolutionPath = __SOURCE_DIRECTORY__ + "/bin/libs"

[<Literal>]
let private ConnectionString = 
    "Host=localhost;Database=toodeloo;Username=postgres;Password=secret;Port=5432;"

// This type must not leak outside the assembly, or things will get 
// complicated due to the type provider depencencies
type internal Db =
    SqlDataProvider<
        ConnectionString = ConnectionString,
        DatabaseVendor = Common.DatabaseProviderTypes.POSTGRESQL,
        Owner = "dbo",
        CaseSensitivityChange = Common.CaseSensitivityChange.ORIGINAL,
        ResolutionPath = ResolutionPath,
        UseOptionTypes = true>

// Shorthand type alias
type internal TodoEntity = Db.dataContext.``dbo.todoEntity``

let internal toTodo (t : TodoEntity) =
    {
        taskId = t.Taskid
        priority = t.Priority
        due = t.Due
        task = t.Task
    }
let createTodo (t : Todo) = 
    let ctx = Db.GetDataContext ()
    let db = ctx.Dbo
    let row = db.Todo.Create ()
    row.Priority <- t.priority
    row.Due <- t.due
    row.Task <- t.task
    try 
        ctx.SubmitUpdates ()
        Ok ()
    with ex -> Error ("createTodo: " + ex.Message)

let readTodo id = 
    let ctx = Db.GetDataContext ()
    let db = ctx.Dbo
    query {
        for i in db.Todo do
            where (i.Taskid = id)
            select i
    } 
    |> Seq.toArray 
    |> function 
    | x when x.Length = 0 -> None
    | x -> x |> Seq.head |> (toTodo >> Some)

let readTodos () = 
    let ctx = Db.GetDataContext ()
    let db = ctx.Dbo
    query {
        for i in db.Todo do
            select i
    } 
    |> Seq.map toTodo |> Seq.toList

let updateTodo t =
    let ctx = Db.GetDataContext ()
    let db = ctx.Dbo
    query {
        for i in db.Todo do
            where (i.Taskid = t.taskId)
            select i
    } |> Seq.iter (fun row -> 
        row.Priority <- t.priority
        row.Due <- t.due
        row.Task <- t.task
    )
    try 
        ctx.SubmitUpdates ()
        Ok ()
    with ex -> Error ("updateTodo: " + ex.Message)

let deleteTodo id =
    let ctx = Db.GetDataContext ()
    let db = ctx.Dbo
    query {
        for i in db.Todo do
            where (i.Taskid = id)
            select i
    } |> Seq.iter (fun x -> x.Delete ())
    ctx.SubmitUpdates ()