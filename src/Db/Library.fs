module Toodeloo.Db

open FSharp.Data.Sql

[<Literal>]
let ResolutionPath = __SOURCE_DIRECTORY__ + "/bin/libs"

type Db =
    SqlDataProvider<
        ConnectionString = "Host=localhost;Database=jupitodo;Username=postgres;Password=secret;Port=5432;",
        DatabaseVendor = Common.DatabaseProviderTypes.POSTGRESQL,
        Owner = "dbo",
        CaseSensitivityChange = Common.CaseSensitivityChange.ORIGINAL,
        ResolutionPath = ResolutionPath,
        UseOptionTypes = true>

let dbinit () =
    let ctx = Db.GetDataContext ()
    let db = ctx.Dbo
    let row = db.Todo.Create ()
    row.Taskid = 0
    // row.Priority = 1
    // row.Due = System.DateTime.Now
    // row.Task = "From db with love."
    ctx.SubmitUpdates ()



let hello name =
    printfn "Hello %s" name
