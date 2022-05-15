module Tests

open System
open Xunit
open Minesweeper
open Minesweeper.Cell

[<Fact>]
let Should_be_Bomb() =
    let sut = Bomb
    let ret = isBomb sut 
    Assert.True(ret)
