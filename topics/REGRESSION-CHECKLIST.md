# Regression & Portability Checklist

Suggested tests and reminders to keep TP7/FPC TP mode behavior aligned.

## Regression tests
- Strings: concat/copy/delete/insert on `string[255]`; check length bounds and
  padding; avoid silent truncation.
- Overflow/Range: compile/run with `{$R+}{$Q+}` expecting RTE 201/214 on
  violations; confirm `ExitCode` parity TP7/FPC.
- File I/O: text copy with `{$I-}` + `IOResult`, typed-file CRUD, untyped block
  copy; ensure outputs match and error codes align.
- Crt timing: measure `Delay` with `Dos.GetTime` around a known duration; ensure
  loops do not assume cycle-accurate timing.
- Exec/Process: TP7 `Exec` + `SwapVectors` on short paths; optional FPC
  `RunCommand` (guarded) for FPC-only workflows.
> Run spot-checks on both TP7 and FPC builds.

## Portability checklist
- Defaults: start with `{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}` and explicit
  `{$R+}{$Q+}` during testing.
- Layout: set `{$PACKRECORDS n}`/`{$PACKENUM n}` and avoid managed types in
  shared records; align expectations across TP7/FPC.
- Separation: wrap FPC-only libraries (`SysUtils`, `Classes`, `Process`,
  `fpjson`, `Zipper`) in `{$IFDEF FPC}` blocks; TP7-only overlay/BGI code
  likewise isolated.
- Size/runtime: keep arrays/buffers under TP7 segment limits; raise TP7 stack
  with `{$M}` if recursion grows; avoid assuming long/ANSI paths on DOS.
- Verification: build/run with the TP7 toolchain and `fpc -Mtp -Sh- -So ...`;
  compare `ExitCode`/outputs for file I/O, strings, and `Delay` timing.
