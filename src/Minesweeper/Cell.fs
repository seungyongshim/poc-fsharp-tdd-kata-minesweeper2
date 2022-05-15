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

    let char v =
        match v with
        | Bomb -> '*'
        | Covered _ -> '.'
        | Number n -> Convert.ToChar(n + 48)

    let rec add v =
        match v with
        | Covered x -> add x
        | Number n -> n + 1 |> Number 
        | _ -> v

    let click v =
        match v with
        | Covered x -> x
        | _ -> v
