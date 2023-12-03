
open System.IO

type Round = {
    red: int
    green: int
    blue: int
}

let emptyRound = {
    red = 0
    green = 0
    blue = 0
}

type Game = int * Round array

let parseRound (round: string): Round =
    round.Split ", "
    |> Array.map (fun s -> s.Split " ")
    |> Array.fold 
        (fun (state: Round) [| count: string; color : string |] ->
            match color with
            | "red" -> { state with red = count |> int<string> }
            | "green" -> { state with green = count |> int<string> }
            | "blue" -> { state with blue = count |> int<string> })
        emptyRound

let parseGame (game: string) =
    game.Substring(game.IndexOf(": ") + 2)
    |> (fun l -> l.Split "; ")
    |> Array.map parseRound

let maxMerge (r1: Round) (r2: Round): Round =
    {
        red = (max r1.red r2.red)
        green = (max r1.green r2.green)
        blue = (max r1.blue r2.blue)
    }

let maxRound (rounds : Round array) = 
    rounds
    |> Array.fold (fun state game -> maxMerge state game) emptyRound


let isPossible (bag : Round) (game: Game): bool =
    let maxGame: Round = 
        game
        |> snd
        |> maxRound

    maxGame.red <= bag.red 
        && maxGame.green <= bag.green 
        && maxGame.blue <= bag.blue
    

let part1 (bag : Round) (games: Game array) =
    games
    |> Array.filter (isPossible bag)
    |> Array.map fst
    |> Array.sum

let games : Game array = 
    File.ReadAllLines("input.txt")
    |> Array.mapi (fun idx line -> (idx + 1), parseGame line)

let bag : Round = {
    red = 12
    green = 13
    blue = 14
}



part1 bag games
|> printfn "Part 1: %A" 
