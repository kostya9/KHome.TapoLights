// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.V2
open KHome

let toHsv (r: uint) (g: uint) (b: uint) =
    let r' = float r / 255.0
    let g' = float g / 255.0
    let b' = float b / 255.0
    
    let cMax = Math.Max(Math.Max(r', g'), b')
    let cMin = Math.Min(Math.Min(r', g'), b')
    let d = cMax - cMin
    
    let mutable hue =
          60.0 *
          match cMax with
          | _ when d = 0.0 -> 0.0
          | _ when cMax = r' -> ((g' - b') / d)
          | _ when cMax = g' -> ((b' - r') / d) + 2.0
          | _ when cMax = b' -> ((r' - g') / d) + 4.0
          | _ -> failwith "Could not happen"
              
    hue <-
        if hue < 0.0
        then hue + 360.0
        else hue    
    
    let saturation =
        match cMax with
        | 0.0 -> 0.0
        | _ -> d / cMax
        
    let value = cMax
        
    (hue |> int, saturation * 100.0 |> int, value * 100.0 |> int)

[<EntryPoint>]
let main _ =
    let result = task {
        let username = Environment.GetEnvironmentVariable("TPLINK_LOGIN")
        let password = Environment.GetEnvironmentVariable("TPLINK_PASSWORD")
        let ipLeft = "192.168.1.110"
        let ipRight = "192.168.1.114"
        
        let! authenticatedLeft = Tapo.authenticate ipLeft username password
        let! authenticatedRight = Tapo.authenticate ipRight username password
        
        let mutable cancelled = false
        Console.CancelKeyPress.Add (fun _ -> cancelled <- true)
        
        let dc = WindowsInterop.GetDC 0
        
        while not cancelled do
            
            let mutable pos = Unchecked.defaultof<WindowsInterop.POINT>
            WindowsInterop.GetCursorPos &pos |> ignore
            let color = WindowsInterop.GetPixel(dc, pos.x, pos.y)
            
            let r = (color &&& 255u)
            let g = ((color >>> 8) &&& 255u)
            let b = ((color >>> 16) &&& 255u)
            
            if color > 0u && color < 0xFFFFFFFFu then
                
                let (h, s, v) = toHsv r g b
                
                // Brightness can not be 0
                let v = Math.Clamp(v, 1, 100) 
                
                Console.WriteLine($"---")
                Console.WriteLine($"Color (BGR): {r} {g} {b}")
                Console.WriteLine($"Color (HSV): {h} {s} {v}")
                
                let setColorOptions: Tapo.SetColorOptions = { Brightness = v; Hue = h; Saturation = s }
            
                let sw = Stopwatch.StartNew();
                let! _ = Task.WhenAll([
                    Tapo.setColor authenticatedLeft setColorOptions;
                    Tapo.setColor authenticatedRight setColorOptions
                 ])
                
                Console.WriteLine($"Sent commands in ${sw.ElapsedMilliseconds}ms")
                
                ()
                
            Thread.Sleep(50)
            
        WindowsInterop.ReleaseDC (0, dc) |> ignore

        return 0
    }
    
    result.Result