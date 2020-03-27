namespace TypedSicm

open Browser

module HTML =

    let graphId = "graph"

    let graph = document.createElement "div"
    graph.id <- graphId

    document.body.appendChild(graph) |> ignore

    let resetButton = document.createElement "button"
    resetButton.innerText <- "Reset"
    document.body.appendChild(resetButton) |> ignore

    let backButton = document.createElement "button"
    backButton.innerText <- "<"
    document.body.appendChild(backButton) |> ignore

    let forwardButton = document.createElement "button"
    forwardButton.innerText <- ">"
    document.body.appendChild(forwardButton) |> ignore

    let status = document.createElement "h3"
    status.innerText <- ""
    document.body.appendChild(status) |> ignore
