# nanoVM — LC-3 Virtual Machine (C++)

`nanoVM` is a **clean, from-scratch LC-3 virtual machine** implemented in C++.  
It focuses on **correct ISA semantics, host safety, and debuggability**, rather than speed or OS emulation.

This is a **single-file VM** (`nanoVM.cpp`) with an industry-style fault and tracing system.

---

## What This Implements

- LC-3 **16-bit word-addressed memory** (64K words)
- LC-3 **register file** (R0–R7, PC, COND)
- Complete **fetch–decode–execute loop**
- Core LC-3 **instruction set**
- **Memory-mapped I/O** (KBSR / KBDR)
- LC-3 **TRAP routines**
- **Big-endian image loading** on little-endian hosts
- **Fault system with diagnostics**
- **Instruction trace ring buffer**
- **Binary fault dump for post-mortem analysis**

---

## Architecture Summary

### Memory
- `uint16_t memory[65536]`
- Word-addressed (LC-3 native)
- Accessed only via `mem_read()` / `mem_write()`

### Registers
- General: `R0–R7`
- Special: `PC`, `COND`
- Stored in `uint16_t reg[]`

### Condition Flags
- `FL_NEG`, `FL_ZRO`, `FL_POS`
- Updated after arithmetic / logical instructions

---

## Instruction Set

Implemented opcodes:
- `ADD`, `AND`, `NOT`
- `BR`
- `LD`, `LDI`, `LDR`, `LEA`
- `ST`, `STI`, `STR`
- `JMP`, `JSR`
- `TRAP`

Unimplemented / invalid instructions fault cleanly.

---

## TRAP Routines

Implemented directly in the VM (no LC-3 OS):

- `GETC`  (x20) — read char, no echo
- `OUT`   (x21) — output char
- `PUTS`  (x22) — string (1 char / word)
- `IN`    (x23) — prompt + echo
- `PUTSP` (x24) — packed string (2 chars / word)
- `HALT`  (x25)

All character output:
- Uses only low 8 bits
- Explicitly masked and cast for safety

---

## Endianness Handling

- LC-3 `.obj` files are **big-endian**
- Host machines are typically **little-endian**
- Every word loaded from an image file is byte-swapped
- Runtime execution uses host-endian `uint16_t`

---

## Keyboard & I/O

- Uses **memory-mapped I/O**
  - `0xFE00` — KBSR
  - `0xFE02` — KBDR
- Non-blocking keyboard polling
- Windows console input buffering disabled for raw input
- Output flushed to behave interactively

---

## Fault Handling System

Guest errors **do not crash the host**.

On a fault:
- Terminal state is restored
- Fault reason is printed
- Full register state is printed
- Instruction trace is printed
- Binary dump is written
- VM exits cleanly

Fault types include:
- Invalid memory access
- Invalid opcode
- Invalid trap
- Unterminated strings
- I/O / EOF errors

---

## Instruction Ring Buffer

- Stores the **last 32 executed PCs**
- Circular buffer
- Printed automatically on fault
- Shows execution history leading to failure

---

## Binary Fault Dump (`vm_fault.dump`)

On every fault, the VM writes a binary snapshot containing:
- Registers
- PC and COND
- Instruction trace buffer
- Fault metadata
- Full memory image (64K words)

This enables:
- Offline debugging
- Replay tooling
- Automated analysis

Dump format is versioned and extensible.

---

## Building

### Windows (MinGW / g++)

```sh
g++ nanoVM.cpp -o nanoVM
