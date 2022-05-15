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

[<Fact>]
let Should_be_Covered_Bomb() =
    let sut = Covered Bomb
    let ret = sut |> isBomb 
    Assert.True(ret)

[<Fact>]
let Should_be_Number_not_Bomb() =
    let sut = 1 |> Number 
    let ret = sut |> isBomb 
    Assert.False(ret)

[<Fact>]
let Should_be_Number() =
    let sut = Number 1
    let ret = sut |> char
    Assert.Equal('1', ret)

[<Fact>]
let Should_be_Number_Add() =
    let sut = Number 1
    let ret = sut |> add |> char 
    Assert.Equal('2', ret)

[<Fact>]
let Should_be_Number_Click() =
    let sut = 1 |> Number |> Covered
    let ret = sut |> click |> char 
    Assert.Equal('1', ret)
