namespace Minesweeper

open System
open System.Text

type Minefield =
    | Setup of width:int * height:int
    | SetupWithBombPos of width:int * height:int * seq<int * int>
    | Playing of width:int * height:int * Map<int * int, Cell>
    | Win of width:int * height:int * Map<int * int, Cell>
    | Loose of width:int * height:int * Map<int * int, Cell>

module Minefield =
    let rec string v =
        let print w z =
            let sb = (StringBuilder(), z) ||> Map.fold(fun s (y, x) z ->
                let _1 = s.Append(z |> Cell.char)
                match x with
                | _ when x = w - 1 -> _1.Append('\n')
                | _ -> _1.Append(' '))
            sb.ToString()
        match v with
        | Win (w, _, z) ->
            let r = (w, z) ||> print
            r + "\nWin"
        | Loose (w, _, z) -> 
            let r = (w, z) ||> print
            r + "\nLoose"
        | Playing (w, _, z) -> (w, z) ||> print
            
        | _ -> String.Empty

    let ifWin v =
        match v with
        | Playing (w, h, z) ->
            let isNotWin = z |> Map.exists (fun x y ->
                match y with
                | Covered(Number _) -> true
                | _ -> false)
            match isNotWin with
            | false -> Win(w, h, z)
            | _ -> v
        | _ -> v

    let getCells = function
        | Playing (w, h, z) -> z
        | _ -> Map.empty

    let rec click p v =
        match v with
        | Playing (w, h, z) ->
            let map = Option.map
            let mapClick = map Cell.click
            match Map.tryFind p z with
            | Some(Covered _) ->
                let clicked = z |> Map.change p mapClick
                match Map.tryFind p clicked with
                | Some(Bomb) -> Loose(w, h, clicked)
                | Some(Number 0) ->
                    let (+) a b = (fst(a) + fst(b), snd(a) + snd(b))
                    Playing(w, h, clicked) |> click (p + (-1, -1))
                                           |> click (p + (-1, 0))
                                           |> click (p + (-1, 1))
                                           |> click (p + (0, -1))
                                           |> click (p + (0, 1))
                                           |> click (p + (1, -1))
                                           |> click (p + (1, 0))
                                           |> click (p + (1, 1))
                                           |> ifWin
                | _ -> Playing(w, h, clicked) |> ifWin
            | _ -> v
        | _ -> v

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

    let count  = function
        | Playing (w, h, z) -> z |> Map.count
        | _ -> 0
