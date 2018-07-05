module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared

open Fulma


type Model = Counter option

type Msg =
| Increment
| Decrement
| Init of Result<Counter, exn>


module Server =

    open Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : ICounterProtocol =
        Proxy.remoting<ICounterProtocol> {
            use_route_builder Route.builder
        }


let init () : Model * Cmd<Msg> =
    let model = None
    let cmd =
        Cmd.ofAsync
            Server.api.getInitCounter
            ()
            (Ok >> Init)
            (Error >> Init)
    model, cmd

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    let model' =
        match model,  msg with
        | Some x, Increment -> Some (x + 1)
        | Some x, Decrement -> Some (x - 1)
        | None, Init (Ok x) -> Some x
        | _ -> None
    model', Cmd.none

let safeComponents =
    let intersperse sep ls =
        List.foldBack (fun x -> function
            | [] -> [x]
            | xs -> x::sep::xs) ls []

    let components =
        [
            "Saturn", "https://saturnframework.github.io/docs/"
            "Fable", "http://fable.io"
            "Elmish", "https://elmish.github.io/elmish/"
            "Fulma", "https://mangelmaxime.github.io/Fulma"
            "Fable.Remoting", "https://zaid-ajaj.github.io/Fable.Remoting/"
        ]
        |> List.map (fun (desc,link) -> a [ Href link ] [ str desc ] )
        |> intersperse (str ", ")
        |> span [ ]

    p [ ]
        [ strong [] [ str "SAFE Template" ]
          str " powered by: "
          components ]

let show = function
| Some x -> string x
| None -> "Loading..."

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "SAFE Template" ] ] ]

          Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
                Columns.columns []
                    [ Column.column [] [ button "-" (fun _ -> dispatch Decrement) ]
                      Column.column [] [ button "+" (fun _ -> dispatch Increment) ] ] ]

          Footer.footer [ ]
                [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ] ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

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
