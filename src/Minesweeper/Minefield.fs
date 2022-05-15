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

    let rec start v =
        match v with
        | Setup (w, h) -> Playing (w, h, query {
            for y in 0..h - 1 do
            for x in 0..w - 1 do
            select ((y, x), 0 |> Number |> Covered)
        } |> Map.ofSeq)
        | SetupWithBombPos (w, h, z) ->
            let cells = Setup (w, h) |> start |> getCells
            let (+) a b = (fst(a) + fst(b), snd(a) + snd(b))
            let cellsWithBombs = (cells, z) ||> Seq.fold (fun acc key ->
                let mapAdd = Option.map Cell.add
                acc |> Map.change key (Option.map (fun i -> Covered Bomb))
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
        match v with
        | Playing (w, h, z) -> z |> Map.count
        | _ -> 0
   
