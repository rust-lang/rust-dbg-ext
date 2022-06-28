/***

#if @cargo_profile == release
  // Local variables don't seem to show up in release builds
  #ignore-test

#if @gdb
  run
  #check Breakpoint @{ .* }@ main @{ .* }@ at @{ .* }@ main.rs:

  print _u8
  #check = 123
  print _u16
  #check = 456
  print _u32
  #check = 789
  print _u64
  #check = 123456789
  print _str
  #check = "I am a string"

  continue
  #check Breakpoint @{ .* }@ main @{ .* }@ at @{ .* }@ main.rs:

  print _i8
  #check = 123
  print _i16
  #check = 456
  print _i32
  #check = 789
  print _i64
  #check = 123456789
  print  _bool
  #check = false

#if @cdb
  g
  #check Breakpoint @{ .* }@ hit

  dx _u8,d
  #check _u8,d           : 123 [Type: @u8@]
  dx _u16,d
  #check _u16,d           : 456 [Type: @u16@]
  dx _u32,d
  #check _u32,d           : 789 [Type: @u32@]
  dx _u64,d
  #check _u64,d           : 123456789 [Type: @u64@]
  dx _str
  #check _str             : "I am a string" [Type: str]

  g
  #check Breakpoint @{ .* }@ hit

  dx _i8,d
  #check _i8,d           : 123 [Type: @i8@]
  dx _i16,d
  #check _i16,d           : 456 [Type: @i16@]
  dx _i32,d
  #check _i32,d           : 789 [Type: @i32@]
  dx _i64,d
  #check _i64,d           : 123456789 [Type: @i64@]
  dx _bool
  #check _bool             : false [Type: bool]

#if @lldb
  run
  #check stop reason = breakpoint

  print _u16
  #check = 456
  print _u32
  #check = 789
  print _u64
  #check = 123456789
  print  _str
  #check "I am a string"

  continue
  #check stop reason = breakpoint

  print _i16
  #check = 456
  print _i32
  #check = 789
  print _i64
  #check = 123456789
  print  _bool
  #check = false


***/

fn main() {
    let _u8 = 123u8;
    let _u16 = 456u16;
    let _u32 = 789u32;
    let _u64 = 123456789u64;
    let _str = "I am a string";

    zzz(); // #break

    let _i8 = 123i8;
    let _i16 = 456i16;
    let _i32 = 789i32;
    let _i64 = 123456789i64;
    let _bool = false;

    zzz(); // #break
}

#[inline(never)]
fn zzz() {}
