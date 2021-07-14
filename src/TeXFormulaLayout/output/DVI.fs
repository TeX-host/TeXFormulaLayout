namespace TeXFormulaLayout
open System

/// Basic binary stream output
module DVI =
    /// [p215#585]
    /// a list of all the commands that may appear in a DVI file.
    type DVICommands =
        /// typeset character 0 and move right
        | SET_CHAR_0 = 0
        /// EMIT SET_CHAR_[1~126]
        | SET_CHAR_127 = 127

        /// typeset a character and move right
        | SET1 = 128
        | SET2 = 129
        | SET3 = 130
        | SET4 = 131
        /// typeset a rule and move right.
        | SET_RULE = 132

        | PUT1 = 133
        | PUT2 = 134
        | PUT3 = 135
        | PUT4 = 136
        /// typeset a rule
        | PUT_RULE = 137

        /// no operation
        | NOP = 138
        /// beginning of page
        | BOP = 139
        /// ending of page
        | EOP = 140
        /// save the current positions
        | PUSH = 141
        /// restore previous positions.
        | POP = 142

        /// move right
        | RIGHT1 = 143
        | RIGHT2 = 144
        | RIGHT3 = 145
        | RIGHT4 = 146

        /// move right by w
        | W0 = 147
        /// move right and set w
        | W1 = 148
        | W2 = 149
        | W3 = 150
        | W4 = 151

        /// move right by x
        | X0 = 152
        /// move right and set x
        | X1 = 153
        | X2 = 154
        | X3 = 155
        | X4 = 156

        /// move down
        | DOWN1 = 157
        | DOWN2 = 158
        | DOWN3 = 159
        | DOWN4 = 160

        /// move right by y
        | Y0 = 161
        /// move right and set y
        | Y1 = 162
        | Y2 = 163
        | Y3 = 164
        | Y4 = 165

        /// move right by z
        | Z0 = 166
        /// move right and set z
        | Z1 = 167
        | Z2 = 168
        | Z3 = 169
        | Z4 = 170

        /// set current font to 0
        | FNT_NUM_0 = 171
        | FNT_NUM_1 = 172
        // emit fnt_num_(2~62)
        | FNT_NUM_63 = 234

        /// set current font
        | FNT1 = 235
        | FNT2 = 236
        | FNT3 = 237
        | FNT4 = 238

        /// extension to DVI primitives
        | XXX1 = 239
        | XXX2 = 240
        | XXX3 = 241
        /// potentially long extension to DVI primitives.
        | XXX4 = 242

        /// define the meaning of a font number
        | FNT_DEF_1 = 243
        | FNT_DEF_2 = 244
        | FNT_DEF_3 = 245
        | FNT_DEF_4 = 246

        /// preamble
        | PRE = 247
        /// postamble beginning
        | POST = 248
        /// postamble ending
        | POST_POST = 249

        // [250~255] are undefined at the present time
    // end enum DVICommands
