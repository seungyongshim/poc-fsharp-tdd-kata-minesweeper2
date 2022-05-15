namespace Minesweeper

open System

type Minefield =
    | Setup of width:int * height:int
    | SetupWithBombPos of width:int * height:int * seq<int * int>
    | Playing of width:int * height:int * Map<int * int, Cell>

module Minefield =
    let getCells v =
        match v with
        | Playing (w, h, z) -> z
        | _ -> Map.empty

    let click p v =
        let map = Option.map
        let mapClick = map Cell.click
        let q z =
            match z.Item p with
            | Covered _ ->
                let clicked = z |> Map.change p mapClick
                match clicked.Item p with
                | Number 0 -> clicked 
                | _ -> clicked
            | _ -> z
        match v with
        | Playing (w, h, z) -> Playing (w, h, q z)

    let rec start v =
        match v with
        | Setup (w, h) -> Playing (w, h, query {
            for y in 0..h - 1 do
            for x in 0..w - 1 do
            select ((y, x), 0 |> Number |> Covered)
        } |> Map.ofSeq)
        | SetupWithBombPos (w, h, z) ->
            let cells = Setup (w, h) |> start |> getCells
            let cellsWithBombs = (cells, z) ||> Seq.fold (fun acc key ->
                let map = Option.map
                let mapAdd = map Cell.add
                let (+) a b = (fst(a) + fst(b), snd(a) + snd(b))
                acc |> Map.change key (map(fun i -> Covered Bomb))
                    |> Map.change (key + (-1, -1)) mapAdd
                    |> Map.change (key + (-1,  0)) mapAdd
                    |> Map.change (key + (-1,  1)) mapAdd
                    |> Map.change (key + ( 0, -1)) mapAdd
                    |> Map.change (key + ( 0,  1)) mapAdd
                    |> Map.change (key + ( 1, -1)) mapAdd
                    |> Map.change (key + ( 1,  0)) mapAdd
                    |> Map.change (key + ( 1,  1)) mapAdd
            )
            Playing(w, h, cellsWithBombs)
        | _ -> v

    let count v =
        let count = Map.count
        match v with
        | Playing (w, h, z) -> z |> count
        | _ -> 0
   
