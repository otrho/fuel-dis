# Fuel Disassembler

A disassembler for the [Fuel](https://github.com/FuelLabs) VM byte code.

![Nice Colours](https://raw.githubusercontent.com/otrho/fuel-dis/main/dis.png)

## About

The [forc](https://github.com/FuelLabs/sway/tree/master/forc) utility provides a `parse-bytecode` command which will print a bare-bones disassembly with
the raw values decoded from a contract's binary bytecode.  It is limited by assuming that every
32-bit word is an executable instruction, although it does attempt to output some helpful comments
where it can.  But generally it is quite hard to read.

`forc-dis` attempts to produces a far more readable disassembly.  It will follow the control flow
from the entry point, making no assumptions about whether decoded bytes are instructions or data.
It also gives readable names to registers, annotates locations which are read by memory instructions
and names known constant values used by the VM.

It also syntax highlights the listing in a fairly pleasing colour scheme.

## Installation

### Via crates.io

```
cargo install forc-dis
```

### Via source

Clone this repository and build with Cargo as usual.

```
git clone https://github.com/otrho/fuel-dis.git
cd fuel-dis
cargo build --release
```

It can then be run in place using `cargo run --release`.  Or it can be installed using `cargo
install --path .`.

## Use

`forc-dis` will take the path to the bytecode binary as a command line argument.

Alternatively, if `forc-dis` is in your path it may be used as a Forc plugin.  From within a Forc
project it can be invoked using `forc dis`, in which case it will attempt to find the bytecode
binary automatically.

## Example

```
$ forc new example

To compile, use `forc build`, and to run tests use `forc test`

----

Read the Docs:
- Sway Book: https://fuellabs.github.io/sway/latest
- Rust SDK Book: https://fuellabs.github.io/fuels-rs/latest
- TypeScript SDK: https://github.com/FuelLabs/fuels-ts

Join the Community:
- Follow us @SwayLang: https://twitter.com/SwayLang
- Ask questions in dev-chat on Discord: https://discord.com/invite/xfpK4Pe

Report Bugs:
- Sway Issues: https://github.com/FuelLabs/sway/issues/new


$ cd example/
$ cat src/main.sw
contract;

abi MyContract {
    fn test_function() -> bool;
}

impl MyContract for Contract {
    fn test_function() -> bool {
        true
    }
}

$ forc build
  Creating a new `Forc.lock` file. (Cause: lock file did not exist)
    Adding core
    Adding std git+https://github.com/fuellabs/sway...
   Created new lock file at...
  Compiled library "core".
  Compiled library "std".
  Compiled contract "example".
  Bytecode size is 60 bytes.

$ forc dis
00000000      ji      block_0010

00000004  47 00 00 00                                      G...

00000008      WORD        0000000000000034h       ; load @ 00000010

00000010  block_0010:                             ; from 00000000
00000010      lw      $ds $is 1                   ; $ds = 0000000000000034h
00000014      add     $ds $ds $is
00000018      lw      $r0 $fp 73
0000001c      lw      $r1 $ds 0                   ; $r1 = 000000002151bd4bh
00000020      eq      $r2 $r0 $r1
00000024      jnzi    $r2 block_0030
00000028      movi    $tmp 123
0000002c      rvrt    $tmp

00000030  block_0030:                             ; from 00000024
00000030      ret     $one

00000034  DATA SECTION:
00000034      WORD        000000002151bd4bh       ; load @ 0000001c
```
