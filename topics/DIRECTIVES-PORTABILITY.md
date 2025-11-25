# Directives & Portability (TP7/FPC TP mode)

Deep-dive on memory model differences, compiler directives, packing/alignment,
and FPC TP mode deviations. Pair this with `topics/MEMORY-MODEL.md` for patterns
and with `compilers/FPC-REFERENCE.md` for per-compiler directive cheatsheets.

## Memory model details
- TP7 real mode uses segment:offset addressing with 64KB data/code segment
  limits; far/huge pointers are needed across segments and overlays support
  larger codebases.
- FPC TP mode runs on a flat memory model; segment concerns vanish but pointer
  size follows the host platform.
- For mixed targets, keep buffers/records within TP7 limits and prefer explicit
  sizing (`Byte`, `SmallInt`, `LongInt`) over `Integer` when sharing code.
- See `topics/MEMORY-MODEL.md` for portability patterns and overlay guidance.

## Compiler directives (corrected)
- Overlay (TP7-only): `{$F+}{$O+}` in the program, plus `{$O UnitName}` after
  `uses` to place units into the `.OVR`; call `OvrInit`. FPC uses optimizer
  flags (`-O`/`{$OPTIMIZATION}`) instead—keep overlay directives TP7-only.
- Strings: `{$H-}` keeps `String` as `ShortString`; `{$H+}` promotes to
  `AnsiString` (FPC-only behavior).
- Boolean evaluation: `{$B-}` short-circuits `and`/`or` (default); `{$B+}`
  forces full evaluation.
- Mode selection: wrap with `{$IFDEF FPC}{$MODE TP}{$ENDIF}` in shared sources.
- Packing/alignment: `{$PACKRECORDS n}`/`{$PACKENUM n}` control layout;
  `{$ALIGN n}`/`{$CODEALIGN n}` affect data/code alignment. State them to avoid
  TP7/FPC binary differences. Example: with `{$PACKRECORDS 1}`, `Byte; LongInt`
  stays 5 bytes; default padding in FPC may push to 8 bytes.
- Safety: `{$R+}` range checking, `{$Q+}` overflow, `{$I+}` I/O checking (FPC
  default). If you disable `{$I+}`, call `IOResult` explicitly.
- Calls/ASM: `{$CALLING ...}` sets calling convention; `{$ASMMODE intel|att}`
  selects syntax in FPC. TP7 always uses Intel syntax and its default calling
  convention.
- Linker/output (FPC-only): `{$SMARTLINK}`/`{$STATIC}` and `{$OUTPUT_FORMAT}`
  (ELF/PE) must be guarded in shared code.

## Known FPC TP mode deviations
- CRT timing: FPC `Delay` avoids TP7 RTE 200 but timing may drift; keyboard
  buffering differs from TP7 in modern terminals.
- I/O checking: FPC defaults to `{$I+}` (raises RTE/exception); TP7 workflows
  often use `{$I-}` + `IOResult`. Be consistent within a program.
- Integer width follows the platform outside `{$MODE TP}`; guard code that
  assumes 16-bit `Integer` with explicit `SmallInt`/`LongInt`.
- Alignment/padding: FPC may pad records unless `{$PACKRECORDS}` is set; state
  layout explicitly for shared binaries.
- Runtime error/exception style: with `SysUtils`, many RTEs become exceptions
  but `ExitCode` mirrors TP7 (see `reference/ERRORS-RUNTIME.md`).
