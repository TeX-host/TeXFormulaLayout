namespace TeXFormulaLayout

/// Collection of functions which slightly simplify the input of formulae.
/// (This should be superseded by the future parser of formulae in TeX notation.)
module EquInput =
    open FontTypes
    open MathTypes

    let SumSym = MathChar (Op, FontFamily.EX, 80)
    let IntSym = MathChar (Op, FontFamily.EX, 82)

    let ord (c: char) = System.Convert.ToInt32(c)
    let sym ch  =
        if System.Char.IsLetter ch then
            (Ord,   FontFamily.MI, ord ch)
        elif System.Char.IsDigit ch || ch = '@' then
            (Ord,   FontFamily.RM, ord ch)
        elif ch = '(' || ch = '[' then
            (Open,  FontFamily.RM, ord ch)
        elif ch = ')' || ch = ']' then
            (Close, FontFamily.RM, ord ch)
        elif ch = '=' || ch = ':' then
            (Rel,   FontFamily.RM, ord ch)
        elif ch = '<' || ch = '>' then
            (Rel,   FontFamily.MI, ord ch)
        elif "!?;".Contains(ch) then
            (Punct, FontFamily.RM, ord ch)
        else
            match ch with
            | ',' -> (Punct, FontFamily.MI, 59)
            | '+' -> (Bin,   FontFamily.RM, 43)
            | '-' -> (Bin,   FontFamily.SY,  0)
            | '*' -> (Bin,   FontFamily.SY,  3)
            | '.' -> (Bin,   FontFamily.SY,  1)
            | _   -> raise (NotImplemented ("Character " + string ch))

    let inline (>>) f g = fun x -> g (f x)
    let inline (<<) f g = fun x -> f (g x)
    let trans (s: string) : MList = List.map (MathChar << sym) (Seq.toList s)

    let overline  = Overline
    let underline = Underline

    let fraction num den =
        GenFraction {
            num = num
            den = den
            thickness = None
            left = 0
            right = 0
        }
    let atop top bot =
        GenFraction {
            num = top
            den = bot
            thickness = Some 0
            left = 0
            right = 0
        }
    let sup a b =
        Script {
            nucleus = a
            supOpt = Some b
            subOpt = None
        }
    let sub a b =
        Script {
            nucleus = a
            supOpt = None
            subOpt = Some b
        }
    let supsub a b c =
        Script {
            nucleus = a
            supOpt = Some b
            subOpt = Some c
        }
    let bigop sym subopt supopt =
        BigOp (
            Default, 
            {
                nucleus = [sym]
                supOpt = supopt
                subOpt = subopt
            }
        )
    let sum = bigop SumSym
    let int = bigop IntSym

    let math kind ml = Kind (kind, ml)

    let style = Style
    let choice d t s ss fs : MList =
        match fs with
        | FontStyle.D -> d
        | FontStyle.T -> t
        | FontStyle.S -> s
        | FontStyle.SS -> ss
