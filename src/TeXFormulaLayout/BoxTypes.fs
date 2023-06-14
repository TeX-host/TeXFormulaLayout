namespace TeXFormulaLayout

module BoxTypes =
    open FontTypes
    open Distance

    type InfOrder = | Normal | Fil | Fill | Filll
    type GlueParam =
        | Natural
        | Stretching of float * InfOrder
        | Shrinking  of float * InfOrder
    type GlueSpec = {
        size:    Dist
        stretch: Dist * InfOrder
        shrink:  Dist * InfOrder
    }
    type Dim = {
        width:  Dist
        depth:  Dist
        height: Dist
    }
    type BoxKind = HBox | VBox

    type Node =
        | Char    of FontNum * CharCode
        | Box     of Dist * box     (* dist = shift_amount *)
        | Rule    of Dim            (* no running dimensions! *)
        | Glue    of GlueSpec
        | Kern    of Dist
        | Penalty of Penalty
    and box = {
        kind:      BoxKind
        width:     Dist
        depth:     Dist
        height:    Dist
        content:   Node list
        glueParam: GlueParam
    }
    type HList = Node list
    type VList = Node list

    let Box0 b = Box (0, b)     (* creates node with zero shift *)
    let HL   b = [Box0 b]       (* creates horizontal list from box *)
