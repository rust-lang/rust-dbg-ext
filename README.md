# Rust Debugging Extensions

This repository contains code related to the [async crashdump debugging initiative][initiative].

Initially this repository will host three main projects:

1. A framework for testing debugger extensions in a robust way
2. A debugger extension for LLDB that provides support for debugging async Rust programs.
3. Another debugger extension that does the same for WinDbg.

The code in this repository is currently considered [experimental] which means that it can change or become unmaintained at any time.
Its main purpose is to enable gathering experience with different approaches for improving crash dump debugging of async Rust programs.

[initiative]: https://github.com/rust-lang/async-crashdump-debugging-initiative
[experimental]: https://github.com/rust-lang/rfcs/blob/master/text/3119-rust-crate-ownership.md#categories
