#pragma MIME application/x-dvi
// Tex DVI format
#include <std/mem.pat>
#include <std/io.pat>


/// [p215#585]
/// a list of all the commands that may appear in a DVI file.
enum DviCmd : u8 {
  /// typeset character 0 and move right
  SET_CHAR_0 = 0,
  // (*  EMIT SET_CHAR_[1~126]  *)
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
  // (*  emit FNT_NUM_[2~62]  *)
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
