/***

#if @cargo_profile == release || @lldb
  // Local variables don't seem to show up in release builds
  #ignore-test


#phase live
#phase crashdump

#if @gdb
  #if @phase == live
    run
    #check Breakpoint @{ .* }@ main @{ .* }@ at @{ .* }@ main.rs:
    #generate-crashdump

  print _a
  #check 123

  print _b
  #check "abc"

  print _c
  #check false

#if @cdb
  #if @phase == live
    g
    #check Breakpoint @{ .* }@ hit
    #generate-crashdump

  dx _a
  #check 123

  dx _b
  #check "abc"

  dx _c
  #check false


***/

fn main() {
    let _a = 123;
    let _b = "abc";
    let _c = false;

    zzz(); // #break
}

#[inline(never)]
fn zzz() {}
