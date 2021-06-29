namespace TeXFormulaLayout

module LoadFont =
    open System
    open System.IO

    /// Types
    type FontSize = Int32
    type Dist = Int32
    type CharCode = Int32

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
        varChar: VarCharInfo
    }
    type Font = CharInfo list


