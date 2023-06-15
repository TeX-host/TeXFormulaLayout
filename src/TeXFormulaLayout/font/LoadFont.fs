namespace TeXFormulaLayout

/// Read cm fonts
module LoadFont =
    open System
    open System.IO
    open TeXFormulaLayout.FontTypes
    open TeXFormulaLayout.Distance

    /// Custom operator for combining paths
    let (+/) path1 path2 = Path.Combine(path1, path2)

    let fontDirPath =
        AppContext.BaseDirectory
        +/ "fonts"

    let getFontPath (fontDir) (famliyName: String) (fontSize: FontSize) =
        let fontName =
            famliyName
            + fontSize.ToString("D2")

        fontDir +/ fontName

    /// split one line to headChar::tailString
    let splitLine (line: String) =
        match line.Length with
        | 0 -> ('\u0000', "") /// TODO: maybe throw an error?
        | 1 -> (line.[0], "")
        | _ -> (line.[0], line.Substring(1, line.Length - 1))

    /// count group id to split lines into group
    let countGroupID id line =
        let typ, param = splitLine line

        match typ with
        // C: start a new group; E: end all group
        | 'C'
        | 'E' -> id + 1
        // others dosn't change group id
        | _ -> id

    let getDist (fontSize: FontSize) (param: String) = floatMul (float param, int fontSize)

    let getOct (param: String) = "0o" + param |> int

    /// read char info from CharInfoList
    let readCharInfo (fontSize: FontSize) (lineGroup: String list) =
        let buildChar (font: CharInfo) (s: string) =
            let parseDist = getDist fontSize
            let typ, param = splitLine s

            match typ with
            | 'W' -> { font with width = parseDist param }
            | 'H' -> { font with height = parseDist param }
            | 'D' -> { font with depth = parseDist param }
            | 'I' -> { font with italic = parseDist param }
            | 'L' -> {
                font with
                    larger = getOct param |> Some
              }
            | 'T' -> {
                font with
                    varChar = {
                        font.varChar with
                            top = getOct param |> Some
                    }
              }
            | 'B' -> {
                font with
                    varChar = {
                        font.varChar with
                            bot = getOct param |> Some
                    }
              }
            | 'R' -> {
                font with
                    varChar = {
                        font.varChar with
                            rep = getOct param |> Some
                    }
              }
            | 'C'
            | 'E' -> font
            | _ -> font

        // new CharInfo template
        let newCharInfo = {
            width = Zero
            height = Zero
            depth = Zero
            italic = Zero
            larger = None
            varChar = { top = None; bot = None; rep = None }
        }

        List.fold buildChar newCharInfo lineGroup

    /// load font info from font files.
    ///
    /// TODO: use parser to read real .tfm fonts
    let readFontInfo (fontSize: FontSize) (lines: String list) : Font =
        // ---- tag lines
        let tags =
            List.scan countGroupID 0 lines
            |> List.tail

        let tagedTupList = List.zip tags lines
        //printfn "tags = %A" tags
        //printfn "tagedTupList = %A" tagedTupList
        /// tags = [1; 1; 1; 1; 1; 1; 2; 2; 2; 2; 2; 2; 2]
        /// tagedTupList = [
        ///     (1, "C0"); (1, "W0.471061"); (1, "H0.042223"); (1, "D1.157789");
        ///     (1, "I0");(1, "L20");
        ///     (2, "C1"); (2, "W0.428238"); (2, "H0.042223"); (2, "D1.157789");
        ///     (2, "T151"); (2, "B171"); (2, "R77")]

        // ---- split lines into char group
        let groupedTupListWithGid = List.groupBy fst tagedTupList
        let groupedTupList = List.map snd groupedTupListWithGid
        let groupedList = List.map (List.map snd) groupedTupList
        //printfn "groupedTupListWithGid = %A" groupedTupListWithGid
        //printfn "groupedTupList = %A" groupedTupList
        //printfn "groupedList = %A" groupedList
        /// groupedTupListWithGid = [
        ///     (1, [(1, "C0");
        ///             (1, "W0.471061"); (1, "H0.042223"); (1, "D1.157789");
        ///             (1, "I0"); (1, "L20")]);
        ///     (2, [(2, "C1");
        ///             (2, "W0.428238"); (2, "H0.042223"); (2, "D1.157789");
        ///             (2, "T151"); (2, "B171"); (2, "R77")])]
        /// groupedTupList = [
        ///     [(1, "C0");
        ///         (1, "W0.471061"); (1, "H0.042223"); (1, "D1.157789");
        ///         (1, "I0"); (1, "L20")];
        ///     [(2, "C1");
        ///         (2, "W0.428238"); (2, "H0.042223"); (2, "D1.157789");
        ///         (2, "T151"); (2, "B171"); (2, "R77")]]
        /// groupedList = [
        ///     ["C0"; "W0.471061"; "H0.042223"; "D1.157789"; "I0"; "L20"];
        ///     ["C1"; "W0.428238"; "H0.042223"; "D1.157789"; "T151"; "B171"; "R77"]]

        let font = List.map (readCharInfo fontSize) groupedList
        //printfn "font = %A" font
        /// font = [
        ///     { width = 5; height = 0; depth = 12; italic = 0; larger = Some 16;
        ///         varChar = { top = None; bot = None; rep = None } };
        ///     { width = 4; height = 0; depth = 12; italic = 0; larger = None;
        ///         varChar = { top = Some 105; bot = Some 121; rep = Some 63 } }]
        font

    let readFontLines (fontSize: FontSize) (fontPath: String) =
        // ---- read all lines
        let raw_lines =
            File.ReadLines(fontPath)
            |> List.ofSeq
        // filter out last line ("E")
        let lines = List.filter ((<>) "E") raw_lines
        //printfn "raw_lines = %A" raw_lines
        //printfn "lines = %A" lines
        /// ---- test with `loadFont "TS" 10`
        /// raw_lines = [
        ///     "C0"; "W0.471061"; "H0.042223"; "D1.157789"; "I0"; "L20";
        ///     "C1"; "W0.428238"; "H0.042223"; "D1.157789"; "T151"; "B171"; "R77"; "E"]
        /// lines = [
        ///     "C0"; "W0.471061"; "H0.042223"; "D1.157789"; "I0"; "L20";
        ///     "C1"; "W0.428238"; "H0.042223"; "D1.157789"; "T151"; "B171"; "R77"]

        readFontInfo fontSize lines

    let loadFont (famliyName: String, fontSize: FontSize) : Font =
        getFontPath fontDirPath famliyName fontSize
        |> (readFontLines fontSize)
