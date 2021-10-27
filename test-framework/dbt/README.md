# Unnamed Rust Debugger Test Framework

This is a framework for testing debuggers and debugger plugins against Rust code. Its goals are

- to enable writing tests that stay robust even while debugger output might change between versions, and
- to enable gathering data on which debugger versions have been tested.


## How to write a debugger test

The framework expects tests to be specified as a Cargo workspace where each executable
that results from compiling the workspace represents one test case.
The `.rs` containing the main function for a test case is also expected to contain a test script
that specifies what debugger commands are to be run and which output is expected from those commands.

A simple test case might look like:


```rust
/***

// This is the test script. Use `//` for writing comments.

// The following block will only be executed if the debugger under test is GDB.
// Indentation is used for specifying blocks, like in Python
#if gdb
  // lines without a #-directive are interpreted as debugger commands
  run
  p x

  // Use #check for testing if the commands above produced the expected output
  #check "my value is 42"

// Now do the same for CDB
#if cdb
  g
  dx x
  #check "my value is 42"

***/

fn main() {

    let x = "my value is 42";

    foo(); // #break - This will set a breakpoint for the given line
}

#[inline(never)]
fn foo() {}
```

## Version-specific debugger output checking

As seen above, test scripts support conditional execution of statements, both for debugger commands and for checks.
This can be used execute different commands not only for different kinds of debuggers but also for different
versions of the same debugger. The same goes for checks. For example, the following will expect different outputs
for different versions of GDB:

```rust
/***

#if gdb
  run
  p x

  #if version == 4
    #check { __0 = 4, __1 = 2 }

  #if version == 5
    #check (4, 2)

***/
```

## Test Script Syntax and Semantics

Test scripts contain one statement per line. Statements with the same indentation
form a block. The entire script is delimited by a line containing only `/***` at
the beginning and a line containing only `***/` at the end. There can only be one
test script in a `.rs` defining a test case.

The script is always evaluated from top to bottom. When generating a debugger
script each statement that does not start with a #-keyword will be emitted verbatim
as a command into the debugger script.
When checking the output of a debugger session each `#check` line causes the
evaluator to scan forward through the output until a matching line is found.

```
// These are commands that will be emitted into the debugger script:
run
print foo

// These are check statements that will be evaluated against the output of the
// debugger session:
#check 123
```


### #check and #check-unordered

Debugger output can be tested by `#check` statements. A `#check` statement starts
with the keyword `#check` and is followed by a check specification which is a string
that is expected to be found anywhere in a debugger output line. E.g. the check
statement `#check abc` will match all of the following lines:

- `abc`
- `  abc  `
- `  abcd`
- `xyz abc def`
- `xyzabcdef`

But it won't match any of

- `abd`
- `ab`
- `a b c`

Check specifications can contain regular expressions, delimited by `@{` and `}@`, as in:

```
#check fn Foo::Bar( @{ .* }@ ) -> u32
```

which would match any of

- `fn Foo::Bar() -> u32`
- `fn Foo::Bar(u32) -> u32`
- `fn Foo::Bar(x: u64, _: &str) -> u32`

Checking can also be done via `#check-unordered` statements, which matches all lines in
in its nested block irrespective of their order in the output. This is useful for checking
debugger commands with indeterministic output order.

```
// Each line in the nested block is a check specification:
#check-unordered
   fn mod1::foo()
   fn mod1::bar()
   fn mod1::baz()
```

All of the lines given in the nested block must be found in the output for the check to succeed.

NOTE: All whitespace in debugger output and check specifications is *normalized*, that is, every
range of whitespace characters within the text is replaced by a single space character. E.g.

- `abc   def    ghi` becomes `abc def ghi`
- `abc\t\t\t  \t \t \t def` becomes `abc def`

All whitespace before and after the regex braces `@{` and `}@` is completely ignored so that all
of the following are equivalent:

- `abc @{ .* }@ def`
- `abc@{ .* }@def`
- `abc @{.*}@ def`
- `abc  @{   .*   }@   def`

This way regular expressions can be made more visible/readable by leaving some space around them.


### Conditional Evaluation

The emission of debugger commands and evaluation of `#check` statements can be made
conditional via `#if` statements. An `#if` statement is expected to be followed by
a nested block. If the condition of the `#if` statement evaluates to true the
statements in the nested block will be executed otherwise the block will be skipped.

```
<if> =  #if <condition> { ("&&" | "||") <condition> }
          <nested-block>

<condition> = <debugger-name>
            | <variable-name> <op> <literal string>

<op> = "=="       // equals
     | "!="       // regex match
     | "contains" // str::contains()
```

Multiple conditions can be concatenated by the logical operators `&&` and `||`.
`<debugger-name>` is one of `gdb`, `lldb`, and `cdb` and will evaluate to true
if the script is run against that debugger.
`<variable-name>` is the name of a variable pre-defined in the evaluation context
(currently `version` is the only available variable, denoting the debugger version).

### Ignoring Tests

A test can be ignored by "executing" the `#ignore-test` statement. Together with conditional
execution this gives fine-grained control over when a given test should be ignored.

```rust
/***

// Ignore this test for old LLDB versions
#if lldb && version < 7
    #ignore-test

***/
```

### Setting breakpoints via #break

It can be very cumbersome to set breakpoints via debugger commands because line numbers frequently
change when modifying a test case. Therefore the framework allows for setting breakpoints
by specifying the keyword `#break` on any line of the `.rs` file containing the test case's main
function:

```rust
/***

#if gdb
  run
  print x
  #check "my value is 42"
  continue

  print x
  #check = 7
  continue

  print x
  #check = (1, 2, 3)
  continue

***/

fn main() {

    let x = "my value is 42";
    foo(); // #break - first breakpoint
    let x = 7;
    foo(); // #break - second breakpoint
    let x = (1, 2, 3);
    foo(); // #break - third breakpoint
}

#[inline(never)]
fn foo() {}

```


### Not Yet Implemented

The following features are not yet implemented:

- `#check-unordered` does not actually work yet
- more pre-defined variables, like `os`, `arch`, `rust` etc.
- `#else` statements if there turns out to be a need for them
- naming values found in output and using them later (like `[[name:regex]]` in LLVM's FileCheck)
- grouping of condition in if statements via parentheses
