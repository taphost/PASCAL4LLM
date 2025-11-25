# Turbo Pascal 3.0 Notes

Guidance for generating code that targets Turbo Pascal 3.0 (1986). This version predates units and the TP4+ code organization model, so shared code for later compilers often does not apply. Cross-check specific switches/limits against the original manual when in doubt.

## Scope and language constraints
- Single-source programs only: no `uses` and no units. Keep all code in one `.PAS` file.
- No object model: the `object` type and virtual/dynamic methods do not exist.
- No `asm ... end` inline assembler (arrives in TP6). Machine code can be injected only via `Inline(...)` byte sequences if required.
- Strings are fixed-length ShortString-style buffers (length byte + data, max 255).
- 16-bit types throughout: `Integer` is 16-bit; `Real` uses the 6-byte format; `LongInt` is 32-bit if available—confirm before emitting 32-bit math.
- File I/O supports text/typed/untyped files; there is no unit-based RTL beyond what the compiler provides.
- 8.3 filenames and DOS paths only; keep identifiers short when embedding them into filenames.

## Directives and switches (high level)
- Defaults favor speed/size: many checks are off. Notable defaults: `$B+` (I/O via CON), `$C+` (Ctrl-C/Ctrl-S handling), `$I+` (I/O checking), `$R-` (range checks off), `$V+` (strict var-string length), `$U-` (user interrupt disabled). Recursion is off in CP/M-80 unless `$A-` (non-absolute/recursive) is set.
- PC-DOS/MS-DOS extras: `$G<size>`/`$P<size>` (Input/Output buffer for redirection, default 0 = console), `$D+` (device check on open), `$F15` (open-files limit, max 15).
- 16-bit builds: `$K+` (stack checking).
- CP/M-80 only: `$A+` (absolute code, recursion disabled by default; `$A-` enables recursion), `$W2` (max nested WITH), `$X+` (array optimization on).
- No `$G` (286 opcodes), `$M` (stack/heap sizing), `$P` (open strings), `$J` (typed-const writability), or TP7 diagnostics switches. `$O` only toggles overlay-safe codegen; there is no `$O unitname`.
- Command-line switches are TP3-specific; do not assume TP7 `tpc` flags.

### Directive defaults (from the TP3.0 manual)
| Directive | Default | Scope | Notes |
| --- | --- | --- | --- |
| `$B` | `+` | Global | I/O mode selection (CON vs TRM). |
| `$C` | `+` | Global | Ctrl-C/Ctrl-S handling; set `$C-` when using `KeyPressed`. |
| `$I` | `+` | Local | I/O checking; `$I-` requires `IOResult`. |
| `$R` | `-` | Local | Range checks off by default. |
| `$V` | `+` | Local | Strict var-string length checking. |
| `$U` | `-` | Local | User interrupt (Ctrl-C) disabled by default. |
| `$N` | `-` | Global | 8087 codegen off (software FP). |
| `$O` | `-` | Global | Overlay-safe codegen toggle only (no `$O unit`). |
| `$F` | `-` | Local | Far calls auto; `+` forces FAR. |
| `$L` | `+` | Global | Local symbol info (debug) when `$D+`. |
| `$D` | `+` | Global | Debug info (line tables). |
| `$G<size>` | `0` | Global | Input buffer size (0 = console). |
| `$P<size>` | `0` | Global | Output buffer size (0 = console). |
| `$D+/-` (device check) | `+` | Global | Check device status on open (PC/MS-DOS). |
| `$Fnn` (open files) | `15` | Global | Max open files (PC/MS-DOS, max 15). |
| `$K` | `+` | Global | Stack checking (16-bit). |
| `$A` | `+` | Global | Absolute code (CP/M-80; recursion off). `$A-` enables recursion. |
| `$Wn` | `2` | Global | Max nested WITH (CP/M-80). |
| `$X` | `+` | Global | Array optimization on (CP/M-80). |

## RTL/library expectations
- No units means no BGI, CRT, DOS helper units, or overlay manager. Only the built-in procedures/functions are available (basic I/O, math, strings, file handling).
- Inline machine code uses `Inline` byte literals; there is no `Asm` block.
- Error handling uses runtime error codes; the set is smaller than TP7—avoid relying on TP7-specific codes.

## Reserved words and syntax deviations (vs. Standard Pascal)
- Reserved words (TP3): `and array begin case const div do downto else end file for function goto if in label mod nil not of or packed procedure program record repeat set then to type until var while with`.
- Core types:

| Type | Size/Notes |
| --- | --- |
| `Boolean` | 1 byte (FALSE/TRUE) |
| `Char` | 1 byte |
| `Byte` | 1 byte unsigned |
| `Integer` | 2 bytes |
| `Word` | 2 bytes unsigned |
| `LongInt` | 4 bytes (availability depends on target) |
| `Real` | 6-byte Real48; software FP unless `$N+` |
| `String` | ShortString, 255 max |
| `Text`, `file of ...` | File types (typed/untyped) |
- Extensions: `string` is a TP extension; Standard Pascal packed/variant handling applies but packing is automatic.
- Deviations (Appendix D in TP3 manual):
  - Recursion: CP/M-80 default code is non-recursive (`$A+`); switch to `$A-` to allow recursion.
  - `Get`/`Put` not implemented; use `Read`/`Write` for all file I/O.
  - `goto` must stay within the current block.
  - `Page` procedure not implemented.
  - `packed` keyword has no effect; packing happens automatically when possible; `Pack/Unpack` not implemented.
  - I/O device handling and Include files (`{$I filename}`) exist but nesting is limited and statements cannot span Include boundaries.

## Memory model and limits
- Single 64KB code/data segment limit is strict; keep global data and code size minimal.
- Heap usage is limited; avoid large static arrays and prefer small buffers.
- Software floating point is the default; 8087 code requires `$N+` and appropriate hardware.

## Code generation guidelines
- Keep programs monolithic; do not emit `uses` or unit references.
- Avoid object-oriented constructs and inline assembler syntax.
- Stick to ShortString buffers, text/typed/untyped file patterns, and simple control structures.
- Keep identifiers and filenames short, and avoid features introduced after TP3 (overlays, BGI graphics, TP7 directives).

## Runtime errors (hex codes)
- `01` Floating point overflow
- `02` Division by zero
- `03` Sqrt argument negative
- `04` Ln argument error (zero/negative)
- `10` String length error (concat >255 or char from non-length-1 string)
- `11` Invalid string index (Copy/Delete/Insert outside 1..255)
- `90` Index out of range (array)
- `91` Scalar/subrange out of range
- `92` Out of integer range (Trunc/Round)
- `F0` Overlay file not found (relevant only to overlay-enabled builds)
- `FF` Heap/stack collision (insufficient space between heap and recursion stack)

## I/O errors (hex codes)
- `01` File does not exist (Reset/Erase/Rename/Exec/Chain)
- `02` File not open for input (read on unopened or write-only file)
- `03` File not open for output (write on unopened or read-only file)
- `04` File not open (BlockRead/BlockWrite)
- `10` Error in numeric format (text-to-number read)
- `20` Operation not allowed on logical device (Erase/Rename/Exec/Chain)
- `21` Not allowed in direct mode (Exec/Chain from in-memory compile)
- `22` Assign to standard files not allowed
- `90` Record length mismatch (typed file)
- `91` Seek beyond end-of-file
- `99` Unexpected end-of-file (physical EOF before Ctrl-Z or read past EOF)
- `F0` Disk write error (disk full)
- `F1` Directory is full
- `F2` File size overflow (record beyond 65535)
- `F3` Too many open files
- `FF` File disappeared (closed after disk change)

### Runtime error notes (TP3 manual wording)
- Arithmetic domain: `03` Sqrt argument <0; `04` Ln argument <=0.
- String bounds: `10` concat beyond 255 or invalid char conversion; `11` index not in 1..255 for Copy/Delete/Insert.
- Range: `90` array index out of range; `91` scalar/subrange assignment out of range; `92` Trunc/Round out of Integer range.
- Memory: `FF` heap/stack collision on New/recursive calls.
- Overlay-specific: `F0` overlay file not found (only if overlays are enabled in toolchain).
