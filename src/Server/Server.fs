open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Saturn
open Shared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let getInitCounter () : Task<Counter> = task { return 42 }

let webApp =
    let server =
        { getInitCounter = getInitCounter >> Async.AwaitTask }
    remoting server {
        use_route_builder Route.builder
    }

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    router webApp
    memory_cache
    use_static publicPath
    // sitemap diagnostic data cannot be inferred when using Fable.Remoting
    // Saturn issue at https://github.com/SaturnFramework/Saturn/issues/64
    disable_diagnostics
    use_gzip
}

run app
