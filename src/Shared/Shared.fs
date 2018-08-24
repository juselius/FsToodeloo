module Toodeloo.Shared

open System

type TaskId = int

type Todo = 
    { taskId : TaskId
      priority : int
      task : string
      due : DateTime option
    }

/// A type that specifies the communication protocol for client and server
/// Every record field must have the type : 'a -> Async<'b> where 'a can also be `unit`
/// Add more such fields, implement them on the server and they be directly available on client
type ITodoProtocol = { 
    createTodo : Todo -> Async<bool> 
    getTodos   : unit -> Async<list<Todo>> 
    updateTodo : Todo -> Async<bool> 
    deleteTodo : int -> Async<unit> 
    }

module Defaults =
    let defaultTodo = 
        { taskId = 0
          priority = 0
          task = ""
          due = None
        }

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName
