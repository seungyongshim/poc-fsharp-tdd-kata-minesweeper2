namespace Minesweeper

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
            for y in 0..h - 1  do
            for x in 0..w - 1 do
            select ((y, x), 0 |> Number |> Covered)
        } |> Map.ofSeq)
        | SetupWithBombPos (w, h, z) ->
            let playing = Setup (w, h) |> start
            let cells = playing |> getCells
            let cellsWithBombs = (cells, z) ||> Seq.fold (fun acc key ->
                Map.change key (fun x -> Some(Covered Bomb)) acc 
            )
            Playing(w, h, cellsWithBombs)
        | _ -> v

    let count v =
        match v with
        | Playing (w, h, z) -> z |> Map.count
        | _ -> 0
   
