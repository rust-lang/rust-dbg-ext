/***

#if @cargo_profile == release || @lldb
  #ignore-test

#if @generate_linux_crashdumps
  // On Linux, we are generating a crashdump during the live phase
  // and are running the test again against the crashdump.
  #phase live
  #phase crashdump

  #if @gdb
    #if @phase == live
      run
      #check Breakpoint @{ .* }@ main @{ .* }@ at @{ .* }@ main.rs:
      #generate-crashdump

    print /u _a
    #check 123
    print /u _b
    #check 424242424242424242
    print _c
    #check false

#if not @generate_linux_crashdumps
  #phase crashdump

  #if @cdb
    // We expect to be at zzz()
    dx _a,d
    #check 123
    dx _b,d
    #check 424242424242424242
    dx _c
    #check false

***/

fn main() {
    let _a = 123u16;
    let _b = 424242424242424242u64;
    let _c = false;

    zzz(); // #break
}

#[inline(never)]
fn zzz() {}
