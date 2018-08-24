module Toodeloo.Server.API

open System.Threading.Tasks
open Giraffe
open Toodeloo
open Toodeloo.Shared

let createTodo t =
    task {
        return Db.createTodo t
    }

let getTodos () : Task<list<Todo>> =
    task {
        return Db.readTodos ()
    }
let updateTodo t =
    task {
        return Db.updateTodo t
    }

let deleteTodo id =
    task {
        return Db.deleteTodo id
    }

let todoApi = {
    createTodo = createTodo >> Async.AwaitTask 
    getTodos   = getTodos   >> Async.AwaitTask
    updateTodo = updateTodo >> Async.AwaitTask
    deleteTodo = deleteTodo >> Async.AwaitTask
}
