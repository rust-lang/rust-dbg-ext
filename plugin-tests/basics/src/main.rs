/***

#if @cargo_profile == release
  // Local variables don't seem to show up in release builds
  #ignore-test

#if @gdb
  run
  #check Breakpoint @{ .* }@ main @{ .* }@ at @{ .* }@ main.rs:

  print _u32
  #check = 123

  print _str
  #check = "I am a string"

#if @cdb
  g
  #check Breakpoint @{ .* }@ hit

  dx _u32,d
  #check _u32,d           : 123 [Type: unsigned int]
  dx _str
  #check _str             : "I am a string" [Type: str]

#if @lldb
  run
  #check stop reason = breakpoint

  print _u32
  #check = 123
  print  _str
  #check "I am a string"

  continue
  #check stop reason = breakpoint

  print _i64
  #check = 456
  print  _bool
  #check = false


***/

fn main() {
    let _u32 = 123u32;
    let _str = "I am a string";

    zzz(); // #break

    let _i64 = 456i64;
    let _bool = false;

    zzz(); // #break
}

#[inline(never)]
fn zzz() {}
