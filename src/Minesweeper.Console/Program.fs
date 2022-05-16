open System.Security.Cryptography
open Minesweeper.Cell
open Minesweeper.Minefield
open Minesweeper
open System

let setup width height bombs = SetupWithBombPos(width, height,
    Seq.initInfinite(fun _ -> (RandomNumberGenerator.GetInt32(height), RandomNumberGenerator.GetInt32(width)))
    |> Seq.distinct
    |> Seq.take bombs)

let game = setup 3 3 2 |> start

let rec gameloop g =
    match g with
    | Playing _ -> 
        g |> string |> Console.WriteLine
        Console.WriteLine()
        "Row : " |> Console.Write
        let y = Console.ReadLine() |> Convert.ToInt32 
        "Col : " |> Console.Write
        let x = Console.ReadLine() |> Convert.ToInt32
        let n = g |> click (y, x)
        Console.SetCursorPosition(0, 0)
        gameloop n
    | _ -> g |> string |> Console.WriteLine

gameloop game
    
