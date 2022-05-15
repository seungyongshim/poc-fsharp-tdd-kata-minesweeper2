namespace Minesweeper

open System

type Cell =
    | Covered of Cell
    | Bomb
    | Number of int
    
       

module Cell =
    let rec isBomb v =
        match v with
        | Bomb -> true
        | Covered x -> isBomb x
        | _ -> false

    let charTo v =
        match v with
        | Bomb -> '*'
        | Covered _ -> '.'
        | Number n -> Convert.ToChar(n + 48)

    let rec addTo v =
        match v with
        | Covered x -> addTo x
        | Number n -> Number (n + 1)
        | _ -> v


