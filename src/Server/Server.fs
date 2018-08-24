module Toodeloo.Main

open System.IO
open Giraffe
open Saturn
open Shared
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Toodeloo

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue Server.API.todoApi
    |> Remoting.buildHttpHandler

let app = application {
    url ("http://0.0.0.0:" + port.ToString () + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_gzip
}

run app
