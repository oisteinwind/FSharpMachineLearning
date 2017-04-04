(*
GOOGLE CHARTS

Get GoogleChart working by including the following in your .fsx file

Example usage:
[ (0.4, 1.2); (1., 4.3); (2.2, 10.5); (3.5, 12.); (4.0, 15.) ] |> Chart.Line |> Chart.Show
*)
#I "packages/Google.DataTable.Net.Wrapper/lib"
#I "packages/XPlot.GoogleCharts/lib/net45"
#I "packages/Newtonsoft.Json/lib/net45"
#I "packages/FSharp.Data/lib/net40"
#r "XPlot.GoogleCharts.dll"
#r "Google.DataTable.Net.Wrapper.dll"
#r "FSharp.Data.dll"
#r "Newtonsoft.Json.dll"

open XPlot.GoogleCharts
open FSharp.Data
open System
open Newtonsoft



let rec factorial n =
    if n = 0.0 then
        1.0
    else 
        n * factorial (n - 1.0)


// Euler method
let euler h f (x,y) = 
    x + h, 
    y + h * f (x,y),
    h
let rungeKutta h f (x,y) =
    let k1 = h * f(x,y)
    let k2 = h * f(x+0.5*h,y+0.5*k1)
    let k3 = h * f(x+0.5*h,y+0.5*k2)
    let k4 = h * f(x+h,y+k3)
    x+h,
    y+k1/6.0+k2/3.0+k3/3.0+k4/6.0,
    h

let coordinates (a:float,b:float,c:float) = (a, b)
// dy/dx = y
let myFunc = fun (x,y) -> y

let eulerIterator (x, y, h) =
    if x > 5.0 then None
    else Some((x, y, h), (euler h myFunc) (x, y))

let rungeKuttaIterator (x,y,h) =
    if x > 5.0 then None
    else Some((x,y,h), (rungeKutta h myFunc) (x,y))

let myEulerSeq h =
    Seq.unfold eulerIterator (0.0, 1.0, h)
    |> Seq.map(coordinates)
let myRungeKuttaSeq h =
    Seq.unfold rungeKuttaIterator (0.0,1.0,h)
    |> Seq.map(coordinates)

let exact = [0. .. 0.1.. 5.0] |> Seq.map(fun x->x,exp x)
Chart.Line[exact; (myEulerSeq 0.1); (myRungeKuttaSeq 1.0)] |> Chart.WithLabels["Exact";"Euler";"Runge Kutta"] |> Chart.Show


let data = WorldBankData.GetDataContext();

let specificData = data.Countries.China.Indicators.``CO2 emissions (kt)``

specificData |> Chart.Scatter |> Chart.Show
