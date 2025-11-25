# Porting Strategies (TP7 ↔ FPC TP mode)

Guidance for moving code between Turbo Pascal 7 and Free Pascal TP mode, plus
LLM-oriented practices to keep shared code safe.

## Porting approaches
- Isolate FPC-only code with `{$IFDEF FPC}` and favor explicit sizes
  (`LongInt`, `SmallInt`) over `Integer` when sharing.
- Replace TP7 overlays with unit splitting when targeting FPC.
- Avoid WinCrt/WinDos in TP7 notes; they belong to Turbo Pascal for Windows, not
  TP7 for DOS.
- Verify inline ASM for register preservation and calling conventions; some
  32-bit/64-bit FPC targets differ from TP7.
- DOS command execution: use `Exec` + `SwapVectors` and adjust `{$M}` to leave
  heap for the child; in FPC wrap `Exec` usage with `{$IFDEF FPC}` and avoid
  long paths for DOS targets.

## Best practices for LLM-generated code
- Always include explicit variable declarations; avoid inferred types.
- Keep snippets tagged with minimum compatibility: `TP3`, `TP5+`, `TP7+`,
  `TP7-only`, `FPC TP`, or FPC-only notes.
- Start shared sources with `{$MODE TP}{$H-}` for TP defaults; guard FPC-only
  paths with `{$IFDEF FPC}`.
- Provide positive (do) and negative (avoid) examples alongside cross-compiler
  notes.
- Keep initialization sections minimal; ensure units end with `end.` while
  initialization blocks end with `end;`.
