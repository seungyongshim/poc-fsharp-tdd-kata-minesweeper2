module MinefieldSpec

open System
open Xunit
open Minesweeper
open Minesweeper.Minefield


[<Fact>]
let Should_be_Create() =
    let sut = Setup (3, 3) |> start
    let ret = sut |> count 
    Assert.Equal(9, ret)

[<Fact>]
let Should_be_Create_With_Bombs() =
    let sut = SetupWithBombPos(3, 3, seq{(0, 0); (1,0)}) |> start
    let ret = sut |>
              getCells |>
              Map.filter(fun x -> fun v -> v |> Cell.isBomb) |>
              Map.count
    Assert.Equal(2, ret)
