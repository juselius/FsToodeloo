namespace Shared

open System

type TaskId = int

type Todo = 
    { taskId : TaskId
      priority : int
      task : string
      due : DateTime
    }

module Defaults =
    let defaultTodo = 
        { taskId = 0
          priority = 0
          task = ""
          due = System.DateTime.Now
        }

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol for client and server
/// Every record field must have the type : 'a -> Async<'b> where 'a can also be `unit`
/// Add more such fields, implement them on the server and they be directly available on client
type ITodoProtocol = { 
    initialTodo : unit -> Async<list<Todo>> 
    }
