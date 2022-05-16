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
    let ret = sut |> getCells
                  |> Map.filter(fun x v -> v |> Cell.isBomb)
                  |> Map.count
    Assert.Equal(2, ret)

[<Fact>]
let Should_be_click_on_bomb() =
    let sut = SetupWithBombPos(3, 3, seq{(0, 0)}) |> start
    let ret = sut |> click (0, 0)
    Assert.True(match ret with
                | Loose _ -> true
                | _ -> false)

[<Fact>]
let Should_be_click_on_zero() =
    let sut = SetupWithBombPos(3, 3, seq{(2, 2)}) |> start
    let ret = sut |> click (0, 0)
    Assert.True(match ret with
                | Win _ -> true
                | _ -> false)

[<Fact>]
let Should_be_click_on_zero1() =
    let sut = SetupWithBombPos(3, 3, seq{(2, 1); (1, 2)}) |> start
    let ret = sut |> click (0, 0) |> string
    
    Assert.Equal("01.\r\n12.\r\n...\r\n", ret);
