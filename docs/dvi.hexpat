#pragma MIME application/x-dvi
// Tex DVI format
//  + WerWolv/ImHex:  https://github.com/WerWolv/ImHex
//  + .hexpat Patterns:  https://github.com/WerWolv/ImHex-Patterns
//  + ref doc:  https://docs.werwolv.net/pattern-language/core-language/data-types

// ---- import libs
#include <std/mem.pat>
#include <std/io.pat>


/// [p215#585]
/// a list of all the commands that may appear in a DVI file.
enum DviCmd : u8 {
  /// typeset character 0 and move right
  SET_CHAR_0 = 0,
  SET_CHAR_1 = 1,
  SET_CHAR_2 = 2,
  SET_CHAR_3 = 3,
  SET_CHAR_4 = 4,
  SET_CHAR_5 = 5,
  SET_CHAR_6 = 6,
  SET_CHAR_7 = 7,
  SET_CHAR_8 = 8,
  SET_CHAR_9 = 9,
  SET_CHAR_10 = 10,
  SET_CHAR_11 = 11,
  SET_CHAR_12 = 12,
  SET_CHAR_13 = 13,
  SET_CHAR_14 = 14,
  SET_CHAR_15 = 15,
  SET_CHAR_16 = 16,
  SET_CHAR_17 = 17,
  SET_CHAR_18 = 18,
  SET_CHAR_19 = 19,
  SET_CHAR_20 = 20,
  SET_CHAR_21 = 21,
  SET_CHAR_22 = 22,
  SET_CHAR_23 = 23,
  SET_CHAR_24 = 24,
  SET_CHAR_25 = 25,
  SET_CHAR_26 = 26,
  SET_CHAR_27 = 27,
  SET_CHAR_28 = 28,
  SET_CHAR_29 = 29,
  SET_CHAR_30 = 30,
  SET_CHAR_31 = 31,
  SET_CHAR_32 = 32,
  SET_CHAR_33 = 33,
  SET_CHAR_34 = 34,
  SET_CHAR_35 = 35,
  SET_CHAR_36 = 36,
  SET_CHAR_37 = 37,
  SET_CHAR_38 = 38,
  SET_CHAR_39 = 39,
  SET_CHAR_40 = 40,
  SET_CHAR_41 = 41,
  SET_CHAR_42 = 42,
  SET_CHAR_43 = 43,
  SET_CHAR_44 = 44,
  SET_CHAR_45 = 45,
  SET_CHAR_46 = 46,
  SET_CHAR_47 = 47,
  SET_CHAR_48 = 48,
  SET_CHAR_49 = 49,
  SET_CHAR_50 = 50,
  SET_CHAR_51 = 51,
  SET_CHAR_52 = 52,
  SET_CHAR_53 = 53,
  SET_CHAR_54 = 54,
  SET_CHAR_55 = 55,
  SET_CHAR_56 = 56,
  SET_CHAR_57 = 57,
  SET_CHAR_58 = 58,
  SET_CHAR_59 = 59,
  SET_CHAR_60 = 60,
  SET_CHAR_61 = 61,
  SET_CHAR_62 = 62,
  SET_CHAR_63 = 63,
  SET_CHAR_64 = 64,
  SET_CHAR_65 = 65,
  SET_CHAR_66 = 66,
  SET_CHAR_67 = 67,
  SET_CHAR_68 = 68,
  SET_CHAR_69 = 69,
  SET_CHAR_70 = 70,
  SET_CHAR_71 = 71,
  SET_CHAR_72 = 72,
  SET_CHAR_73 = 73,
  SET_CHAR_74 = 74,
  SET_CHAR_75 = 75,
  SET_CHAR_76 = 76,
  SET_CHAR_77 = 77,
  SET_CHAR_78 = 78,
  SET_CHAR_79 = 79,
  SET_CHAR_80 = 80,
  SET_CHAR_81 = 81,
  SET_CHAR_82 = 82,
  SET_CHAR_83 = 83,
  SET_CHAR_84 = 84,
  SET_CHAR_85 = 85,
  SET_CHAR_86 = 86,
  SET_CHAR_87 = 87,
  SET_CHAR_88 = 88,
  SET_CHAR_89 = 89,
  SET_CHAR_90 = 90,
  SET_CHAR_91 = 91,
  SET_CHAR_92 = 92,
  SET_CHAR_93 = 93,
  SET_CHAR_94 = 94,
  SET_CHAR_95 = 95,
  SET_CHAR_96 = 96,
  SET_CHAR_97 = 97,
  SET_CHAR_98 = 98,
  SET_CHAR_99 = 99,
  SET_CHAR_100 = 100,
  SET_CHAR_101 = 101,
  SET_CHAR_102 = 102,
  SET_CHAR_103 = 103,
  SET_CHAR_104 = 104,
  SET_CHAR_105 = 105,
  SET_CHAR_106 = 106,
  SET_CHAR_107 = 107,
  SET_CHAR_108 = 108,
  SET_CHAR_109 = 109,
  SET_CHAR_110 = 110,
  SET_CHAR_111 = 111,
  SET_CHAR_112 = 112,
  SET_CHAR_113 = 113,
  SET_CHAR_114 = 114,
  SET_CHAR_115 = 115,
  SET_CHAR_116 = 116,
  SET_CHAR_117 = 117,
  SET_CHAR_118 = 118,
  SET_CHAR_119 = 119,
  SET_CHAR_120 = 120,
  SET_CHAR_121 = 121,
  SET_CHAR_122 = 122,
  SET_CHAR_123 = 123,
  SET_CHAR_124 = 124,
  SET_CHAR_125 = 125,
  SET_CHAR_126 = 126,
  SET_CHAR_127 = 127,

  /// typeset a character and move right
  SET1 = 128,
  SET2 = 129,
  SET3 = 130,
  SET4 = 131,
  /// typeset a rule and move right.
  SET_RULE = 132,

  PUT1 = 133,
  PUT2 = 134,
  PUT3 = 135,
  PUT4 = 136,
  /// typeset a rule
  PUT_RULE = 137,

  /// no operation
  NOP = 138,
  /// beginning of page
  BOP = 139,
  /// ending of page
  EOP = 140,
  /// save the current positions
  PUSH = 141,
  /// restore previous positions.
  POP = 142,

  /// move right
  RIGHT1 = 143,
  RIGHT2 = 144,
  RIGHT3 = 145,
  RIGHT4 = 146,

  /// move right by w
  W0 = 147,
  /// move right and set w
  W1 = 148,
  W2 = 149,
  W3 = 150,
  W4 = 151,

  /// move right by x
  X0 = 152,
  /// move right and set x
  X1 = 153,
  X2 = 154,
  X3 = 155,
  X4 = 156,

  /// move down
  DOWN1 = 157,
  DOWN2 = 158,
  DOWN3 = 159,
  DOWN4 = 160,

  /// move right by y
  Y0 = 161,
  /// move right and set y
  Y1 = 162,
  Y2 = 163,
  Y3 = 164,
  Y4 = 165,

  /// move right by z
  Z0 = 166,
  /// move right and set z
  Z1 = 167,
  Z2 = 168,
  Z3 = 169,
  Z4 = 170,

  /// set current font to 0
  FNT_NUM_0 = 171,
  FNT_NUM_1 = 172,
  FNT_NUM_2 = 173,
  FNT_NUM_3 = 174,
  FNT_NUM_4 = 175,
  FNT_NUM_5 = 176,
  FNT_NUM_6 = 177,
  FNT_NUM_7 = 178,
  FNT_NUM_8 = 179,
  FNT_NUM_9 = 180,
  FNT_NUM_10 = 181,
  FNT_NUM_11 = 182,
  FNT_NUM_12 = 183,
  FNT_NUM_13 = 184,
  FNT_NUM_14 = 185,
  FNT_NUM_15 = 186,
  FNT_NUM_16 = 187,
  FNT_NUM_17 = 188,
  FNT_NUM_18 = 189,
  FNT_NUM_19 = 190,
  FNT_NUM_20 = 191,
  FNT_NUM_21 = 192,
  FNT_NUM_22 = 193,
  FNT_NUM_23 = 194,
  FNT_NUM_24 = 195,
  FNT_NUM_25 = 196,
  FNT_NUM_26 = 197,
  FNT_NUM_27 = 198,
  FNT_NUM_28 = 199,
  FNT_NUM_29 = 200,
  FNT_NUM_30 = 201,
  FNT_NUM_31 = 202,
  FNT_NUM_32 = 203,
  FNT_NUM_33 = 204,
  FNT_NUM_34 = 205,
  FNT_NUM_35 = 206,
  FNT_NUM_36 = 207,
  FNT_NUM_37 = 208,
  FNT_NUM_38 = 209,
  FNT_NUM_39 = 210,
  FNT_NUM_40 = 211,
  FNT_NUM_41 = 212,
  FNT_NUM_42 = 213,
  FNT_NUM_43 = 214,
  FNT_NUM_44 = 215,
  FNT_NUM_45 = 216,
  FNT_NUM_46 = 217,
  FNT_NUM_47 = 218,
  FNT_NUM_48 = 219,
  FNT_NUM_49 = 220,
  FNT_NUM_50 = 221,
  FNT_NUM_51 = 222,
  FNT_NUM_52 = 223,
  FNT_NUM_53 = 224,
  FNT_NUM_54 = 225,
  FNT_NUM_55 = 226,
  FNT_NUM_56 = 227,
  FNT_NUM_57 = 228,
  FNT_NUM_58 = 229,
  FNT_NUM_59 = 230,
  FNT_NUM_60 = 231,
  FNT_NUM_61 = 232,
  FNT_NUM_62 = 233,
  FNT_NUM_63 = 234,

  /// set current font
  FNT1 = 235,
  FNT2 = 236,
  FNT3 = 237,
  FNT4 = 238,

  /// extension to DVI primitives
  XXX1 = 239,
  XXX2 = 240,
  XXX3 = 241,
  /// potentially long extension to DVI primitives.
  XXX4 = 242,

  /// define the meaning of a font number
  FNT_DEF_1 = 243,
  FNT_DEF_2 = 244,
  FNT_DEF_3 = 245,
  FNT_DEF_4 = 246,

  /// preamble
  PRE = 247,
  /// postamble beginning
  POST = 248,
  /// postamble ending
  POST_POST = 249,

  // (*  [250~255] are undefined at the present time  *)
  UNDEFINED = 250 ... 255
}; // DviCmd

// u32 with big endian
using Nat1 = u8;
using Nat2 = be u16;
using Nat3 = be u24;
using Nat4 = be u32;

struct Str {
    u8 len;
    char strings[while($ <= addressof(len) + len)];
} [[inline]]; // Str

struct NumDen {
    Nat4 num;
    Nat4 den;
} [[inline]]; // NumDen


// ==========================
struct Set1 {
    char c;
} [[inline]]; // Set1
struct SetRule {
    Nat4 height;
    Nat4 width;
}; // SetRule

struct Put1 {
    char c;
} [[inline]]; // Put1
struct PutRule {
    Nat4 height;
    Nat4 width;
}; // PutRule

struct Bop {
    Nat4 pageNum;
    /// c1[4] ~ c9[4]
    Nat4 c[9];
    Nat4 prevPos;
}; // Bop

struct Right1 {
    u8 b;
}; // Right1
struct Right2 {
    Nat2 b;
}; // Right2
struct Right3 {
    Nat3 b;
}; // Right3
struct Right4 {
    Nat4 b;
}; // Right4
struct Down1 {
    u8 b;
}; // Down1
struct Down2 {
    Nat2 b;
}; // Down2
struct Down3 {
    Nat3 b;
}; // Down3
struct Down4 {
    Nat4 b;
}; // Down4

struct Fnt1 {
    u8 f;
}; // Fnt1
struct FontDef {
    u8 font_number;
    Nat4 checksum;
    Nat4 scale;
    Nat4 design_size;
    u8 str_len1;
    Str fontname;
}; // FontDef

struct Pre {
    u8 version;
    NumDen num_den;
    Nat4 mag;

    Str banner;
} [[inline]]; // Pre
struct Post {
    Nat4 prevPos;
    NumDen num_den;
    Nat4 mag;
    Nat4 maxVerticalHeight;
    Nat4 maxHorizonWidth;
    Nat2 maxStackDepth;
    Nat2 totalPagesNum;
} [[inline]]; // Post

struct Fil223 {
    u8 fill[while(std::mem::read_unsigned($, 1) == 0xDF)];
} [[inline]];
struct PostPost {
    Nat4 postPos;
    u8 version;
    Fil223 fill;
}; // PostPost


fn not_impl(DviCmd cmd) {
    // std::error(std::format("Not impl {}", cmd));
    std::warning(std::format("Not impl {}", cmd));
};

fn no_args() {};

struct Cmd {
    /// Cmd type
    DviCmd cmd;

    /// parse args
    match (cmd) {
        // ---- SetChar
        (DviCmd::SET_CHAR_0 ... DviCmd::SET_CHAR_127): no_args();
        (DviCmd::SET1): Set1 set1;
        (DviCmd::SET_RULE): SetRule setRule;
        // ---- PutChar
        (DviCmd::PUT1): Put1 put1;
        (DviCmd::PUT_RULE): PutRule putRule;

        (DviCmd::NOP): no_args();
        (DviCmd::BOP): Bop bop;
        (DviCmd::EOP): no_args();
        // ---- Stack
        (DviCmd::PUSH): no_args();
        (DviCmd::POP): no_args();

        (DviCmd::RIGHT1): Right1 right;
        (DviCmd::RIGHT2): Right2 right;
        (DviCmd::RIGHT3): Right3 right;
        (DviCmd::RIGHT4): Right4 right;
        (DviCmd::DOWN1): Down1 down;
        (DviCmd::DOWN2): Down2 down;
        (DviCmd::DOWN3): Down3 down;
        (DviCmd::DOWN4): Down4 down;

        // ---- Font
        (DviCmd::FNT_NUM_0 ... DviCmd::FNT_NUM_63): no_args();
        (DviCmd::FNT1): Fnt1 font;
        (DviCmd::FNT_DEF_1): FontDef fontDef;

        (DviCmd::PRE): Pre pre  [[inline]];
        (DviCmd::POST): Post post  [[inline]];
        (DviCmd::POST_POST): PostPost pp  [[inline]];

        (DviCmd::UNDEFINED): std::warning(std::format("Undefined Cmd {}", cmd));
        (_) : not_impl(cmd);
    } // match (cmd)
}  [[inline]]; // Cmd


/// Main DVI file.
struct DVI {
    Cmd cmd[while(!std::mem::eof())]  [[inline]];
}; // DVI

DVI dvi @ 0x00;
