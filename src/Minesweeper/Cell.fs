namespace Minesweeper

type Cell =
    | Covered of Cell
    | Bomb
    | Number of uint8

module Cell =
    let rec isBomb v =
        match v with
        | Bomb -> true
        | Covered x -> isBomb x
        | _ -> false

    let hello name =
        printfn "Hello %s" name
