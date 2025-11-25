# Core Compatibility

Baseline guidance (sections 1-6 from the original guide) for writing code that builds on both Turbo Pascal 7.0 and Free Pascal 3.2.2 (`{$MODE TP}`).

## Quick navigation
- Directives and TP7/FPC differences: see `compilers/FPC-REFERENCE.md` (“Directive Cheatsheet”) and `topics/DIRECTIVES-PORTABILITY.md` (“Compiler Directives”).
- Unit map TP7 vs FPC/RTL/FCL/FCL-res: see `reference/UNITS-REFERENCE.md`.
- Mode tables: `compilers/MODES-COMPARISON.md`.

## Scope
- Shared syntax and features that behave the same in TP7 and FPC TP mode.
- Areas where FPC adds extensions that should be avoided in shared code.
- Links to deeper coverage in `topics/DIRECTIVES-PORTABILITY.md` and per-compiler references.

## Target Compilers
- **TP7**: 16-bit real-mode DOS compiler, single language mode, classic Borland RTL.
- **FPC 3.2.2**: Use `{$MODE TP}` (wrap in `{$IFDEF FPC}` to avoid TP7 errors). OBJFPC/Delphi modes add modern features not usable in TP7.

## Feature Compatibility Matrix (corrected)
| Feature / Concept          | TP7                 | FPC (TP Mode)                        | FPC (OBJFPC/Delphi)                      |
|----------------------------|---------------------|--------------------------------------|------------------------------------------|
| ShortString (`String[255]`) | ✔                   | ✔                                    | ✔                                        |
| AnsiString (dynamic)       | ✘                   | ✔ when `{$H+}` (not TP7-compatible)  | ✔                                        |
| Integer size               | 16-bit              | 16-bit in TP mode                    | Platform-dependent (16/32/64-bit)        |
| `SmallInt` (16-bit)        | ✔                   | ✔                                    | ✔                                        |
| `LongInt` (32-bit)         | ✔                   | ✔                                    | ✔                                        |
| `Real` mapping             | 6-byte              | Maps to `Double` by default          | Maps to `Double` by default              |
| Inline assembler           | ✔ (Intel)           | ✔ (Intel)                            | ✔ (Intel/AT&T, depending on target)      |
| Overloading / Generics     | ✘                   | ✘                                    | ✔                                        |
| Classes (OOP)              | Limited (TP style)  | ✔                                    | ✔                                        |
| Unicode strings            | ✘                   | ✘                                    | ✔                                        |
| Dynamic arrays             | ✘                   | ✘                                    | ✔                                        |
| Units                      | ✔                   | ✔                                    | ✔                                        |
| Overlays                   | ✔                   | ✘ (no TP-style overlay support)         | ✘                                        |

> **Note:** Some TP7 releases accepted binary/octal numeric literals as extensions; do not assume they are FPC-only features.

## Essential directives (LLM-friendly defaults)
- **TP-safe block**: `{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}` plus `{$I+}`/`{$B-}`/default checks. Keep `String` as ShortString with `{$H-}`.
- **Debug checks**: enable `{$R+}{$Q+}` to surface range/overflow; disable only with justification. Use `{$I-}` + `IOResult` only if you handle errors manually.
- **Packing/alignment**: set explicitly when sharing binaries: `{$PACKRECORDS 1}` (or 2/4/8/16) and `{$PACKENUM 1}`; `{$ALIGN 16}`/`{$CODEALIGN 16}` for FPC if alignment matters.
- **Calling/ASM**: TP7 uses Intel; FPC can select with `{$ASMMODE intel|att}` and `{$CALLING cdecl|stdcall|register|pascal}`. Avoid changing defaults unless needed.
- **Strings**: `{$H-}` → ShortString, TP7-compatible. `{$H+}` → AnsiString (FPC-only).
- **Typed constants**: `{$J-}` read-only (default FPC). `{$J+}` writable (TP7-style); use sparingly.
- **FPC defaults (TP mode)**: `$B-` (short-circuit), `$I+` (IO checked), `$R-` (range off), `$Q-` (overflow off), `$V-` (lenient ShortString refs), `$X+` (extended syntax on), `$H-` (ShortString; Delphi mode implies `$H+`), default assembler reader AT&T (`{$ASMMODE intel}` for TP-style Intel syntax).

## Type mapping (sizes depend on compiler/mode/target)
| Type        | TP7 size      | FPC TP mode        | FPC OBJFPC/Delphi          | Notes                                   |
|-------------|---------------|--------------------|----------------------------|-----------------------------------------|
| Byte        | 8 bits        | 8 bits             | 8 bits                     |                                         |
| Word        | 16 bits       | 16 bits            | 16 bits                    |                                         |
| Integer     | 16 bits       | 16 bits            | 16/32/64 (platform)        | Prefer `SmallInt`/`LongInt` for clarity |
| SmallInt    | 16 bits       | 16 bits            | 16 bits                    |                                         |
| LongInt     | 32 bits       | 32 bits            | 32 bits                    |                                         |
| Real        | 6-byte        | Double by default  | Double by default          | Use `Real48` in FPC if target supports  |
| Double      | 64-bit float  | 64-bit float       | 64-bit float               |                                         |
| String      | ShortString   | ShortString with `{$H-}` | AnsiString with `{$H+}`  |                                         |
| Char        | 8-bit         | 8-bit              | 8-bit (AnsiChar) / WideChar (Unicode modes) | |

## Data Types
- Prefer `SmallInt` (portable 16-bit) and `LongInt` (portable 32-bit) instead of bare `Integer` when exact size matters.
- FPC maps `Real` to `Double` unless explicitly compiled for targets that support the 6-byte Real; TP7 always uses 6-byte Real.
- Use `String` as ShortString with `{$H-}` in FPC to mirror TP7 behavior.
- String handling demo (TP-safe ShortString vs FPC-only AnsiString):

```pascal
Compatibility: TP3+, TP7, FPC TP (`{$H-}`)
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
var
  S: string[255];
begin
  S := 'TP7';
  S := S + ' / FPC';
  Writeln(S, ' len=', Length(S));  { ShortString; length in first byte }
end.
```

```pascal
Compatibility: FPC-only (OBJFPC/Delphi)
{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}  { FPC-only }
var
  A: AnsiString;
begin
  A := 'Hello';
  A := A + ' Unicode ✓';
  Writeln(A, ' len=', Length(A));  { Reference-counted, not TP7-compatible }
end.
```

```pascal
Compatibility: TP3+, TP7, FPC TP (`{$H-}`)
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
program TypesDemo;
var
  B  : Byte;
  W  : SmallInt;
  I  : Integer;   { 16-bit in both TP7 and FPC TP mode }
  L  : LongInt;
  R  : Real;      { Double in FPC by default; 6-byte in TP7 }
  D  : Double;
begin
  Writeln('Sizes -> Integer:', SizeOf(I), ' LongInt:', SizeOf(L));
end.
```

## Control Structures
- `for`, `while`, and `repeat` behave the same in TP7 and FPC TP mode.
- Boolean evaluation: `{$B-}` is the TP default and short-circuits `and`/`or`; `{$B+}` forces complete evaluation.

## Program and Unit Structure
- Order for programs: `program ...;` → uses → global vars → routines → `begin ... end.`
- Order for units: `unit ...;` → interface → implementation → initialization → `end.`
- Programs end with `end.` (with period). Unit initialization blocks end with `end;` inside the unit but the unit itself terminates with `end.`
- Keep initialization blocks minimal for TP7 memory constraints.

## Basic I/O
- Use `ReadLn`/`WriteLn` for console; rely on `Crt` sparingly to avoid timing quirks under emulation.
- Text file template that works in both compilers:

```pascal
Compatibility: TP3+, TP7, FPC TP
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
program CopyText;
var
  Src, Dst : Text;
  Line     : string[255];
begin
  Assign(Src, 'input.txt');
  Reset(Src);
  Assign(Dst, 'output.txt');
  Rewrite(Dst);
  while not Eof(Src) do
  begin
    ReadLn(Src, Line);
    WriteLn(Dst, Line);
  end;
  Close(Src);
  Close(Dst);
end.
```

## Build commands (reference for LLM)
- TP7: `tpc program.pas` (assumes Borland environment).
- FPC TP mode: `fpc -Mtp -So -Sh- program.pas` (adds OBJFPC-style operators off, shortstrings). Add `-gl` for debug info; use `-Fu`/`-Fl` to set unit/library paths.

## Cross-References
- Compiler directives and TP/FPC differences: `topics/DIRECTIVES-PORTABILITY.md` (packing/alignment/safety) and `compilers/FPC-REFERENCE.md` (“Directive Cheatsheet”).
- Compiler-specific notes: `compilers/TP7-REFERENCE.md`, `compilers/FPC-REFERENCE.md`
- Unit catalog (TP7 vs FPC/RTL/FCL/FCL-res): `reference/UNITS-REFERENCE.md`; quick APIs: `reference/TP7-UNITS-CHEATSHEET.md` (TP7) and `reference/FPC-UNITS-CHEATSHEET.md` (FPC 3.2.2); broader summaries: `reference/TP7-RTL-SUMMARY.md`, `reference/FPC-RTL-FCL-SUMMARY.md`.
- String and file handling deep dives: `topics/STRINGS-IO.md`, `topics/FILE-HANDLING.md`
