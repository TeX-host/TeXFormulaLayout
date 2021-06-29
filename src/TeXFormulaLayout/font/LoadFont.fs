namespace TeXFormulaLayout

module LoadFont =
    open System
    open System.IO

    /// Types
    type FontSize = Int32
    type CharCode = Int32

    type Dist = Int32
    [<Literal>]
    let Zero: Dist = 0
    [<Literal>]
    let One: Dist = 65536 // 2^16

    /// ---- Font Types
    type FontNum = Int32

    /// xref: texbook:p199:S546
    type VarCharInfo = {
        top: CharCode option
        // mid: CharCode option
        bot: CharCode option
        rep: CharCode option
    }

    /// xref: neuform:p6:sec3.1, mfbook:p315, texbook:p63
    type CharInfo = {
        // ---- 4D for each character
        /// the width of the bounding box
        width: Dist
        /// the height (above the baseline) of the bounding box
        height: Dist    
        /// the depth (below the baseline) of the bounding box
        depth: Dist     
        /// the character’s “italic correction”
        italic: Dist    

        /// larger version of the same char
        larger: CharCode option
        varChar: VarCharInfo    /// NOTE: not used, set to None
    }
    type Font = CharInfo list


    /// Custom operator for combining paths
    let (+/) path1 path2 = Path.Combine(path1, path2)

    let getFontName (famliyName: String) (fontSize: FontSize) =
        let fontDir = AppContext.BaseDirectory +/ "fonts"
        let fontName = famliyName + fontSize.ToString("D2")
        fontDir +/ fontName

    let splitLine (line: String) =
        match line.Length with
        | 0 -> ('\u0000', "") /// TODO: maybe throw an error?
        | 1 -> (line.[0], "")
        | _ -> (line.[0], line.Substring(1, line.Length-1))

    let checkType id line =
        let typ, param = splitLine line
        match typ with
        | 'C' | 'E' -> id + 1
        | 'W' | 'H' | 'D' | 'I' | 'L' | 'T' | 'B' | 'R' -> id
        | _ -> id
    
    let loadFont (famliyName: String, fontSize: FontSize) =
        let fontPath = getFontName famliyName fontSize
        printfn "%s" fontPath

        let lines = File.ReadLines(fontPath)
        let l = List.ofSeq lines
        printfn "%A" l

        let tag list = List.scan checkType 0 list
        let tags = tag l |> List.tail
        let tagedList = List.zip tags l
        let g = List.groupBy (fun (t, _) -> t) tagedList
        let g1 = g |> List.map snd
        let g2 = g1 |> List.map (List.map snd)

        printfn "%A" tags
        printfn "%A" tagedList
        printfn "%A" g
        printfn "%A" g1
        printfn "%A" g2

        let g3 = List.filter (fun l -> List.head l = "E" |> not) g2

        let c = List.head g2
        let readCharInfo (fontSize: FontSize) (line: String list) =
            let new_font = {
                width = Zero; height = Zero; depth = Zero; italic = Zero
                larger = None
                varChar = { top = None; bot = None; rep = None; }
            }

            let floatMul (d: Dist) (f: float) = round (f * float d) |> int32
            let getDist (fontSize: FontSize) (param: String) = float param |> floatMul fontSize
            let getOct (param: String) = "0o" + param |> int32

            let updateFont (font: CharInfo) (s: string) =
                let parseDist = getDist fontSize
                let typ, param = splitLine s
                match typ with
                | 'W' -> {font with width = parseDist param}
                | 'H' -> {font with height = parseDist param}
                | 'D' -> {font with depth = parseDist param}
                | 'I' -> {font with italic = parseDist param}
                | 'L' -> {font with larger = getOct param |> Some}
                | 'T' -> {font with varChar = {font.varChar with top = getOct param |> Some}}
                | 'B' -> {font with varChar = {font.varChar with bot = getOct param |> Some}}
                | 'R' -> {font with varChar = {font.varChar with rep = getOct param |> Some}}
                | 'C' | 'E' -> font
                | _ -> font

            List.fold updateFont new_font line
        let ret = readCharInfo 10 c
        
        let ret1 = List.map (readCharInfo 100) g3
        printfn "%A" c
        printfn "%A" ret1
        printfn "%A" g3
