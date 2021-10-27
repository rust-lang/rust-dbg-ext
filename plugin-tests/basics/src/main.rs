/***

#if gdb
  run
  #check Breakpoint @{ .* }@ main @{ .* }@ at @{ .* }@ main.rs:

  print _u32
  #check = 123

  print _str
  #check = "I am a string"

#if cdb
  g
  #check Breakpoint @{ .* }@ hit

  dx _u32,d
  #check _u32,d           : 123 [Type: unsigned int]
  dx _str
  #check _str             : "I am a string" [Type: str]

***/

fn main() {
  let _u32 = 123u32;
  let _str = "I am a string";

  zzz(); // #break
}

#[inline(never)]
fn zzz() {}
