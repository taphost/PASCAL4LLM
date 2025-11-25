# Pascal4LLM API (LLM entry point)
Use this file as the starting point for LLMs and tooling that need to generate
code compatible with Turbo Pascal 7.0 and Free Pascal 3.2.2 in TP mode.

## Flow for code generation
1. Lock defaults: read `CORE-COMPATIBILITY.md` and set `{$MODE TP}{$H-}`, type
   sizes, and safe directives.
2. Choose compiler path:
   - TP7: `compilers/TP7-REFERENCE.md`
   - FPC TP mode: `compilers/FPC-REFERENCE.md`
   - Mode differences: `compilers/MODES-COMPARISON.md`
3. Apply patterns: use `topics/` guides for strings/I/O, files, memory, graphics
   (BGI), assembly, directives/portability, porting, and debugging; pull canonical
   snippets from `examples/BASIC-PATTERNS.md` (tagged TP-safe vs FPC-only).
4. Harden compatibility: `topics/DIRECTIVES-PORTABILITY.md` (directives,
   packing/alignment, TP7 vs FPC TP mode deviations) and
   `topics/PORTING-STRATEGIES.md` (overlays vs unit split, ASM/calling,
   Exec/SwapVectors).
5. Quick APIs: TP7 tables in `reference/TP7-UNITS-CHEATSHEET.md`; FPC tables in
   `reference/FPC-UNITS-CHEATSHEET.md`; unit index in `reference/UNITS-REFERENCE.md`.
6. Errors and terms: `reference/ERRORS-RUNTIME.md` and `reference/GLOSSARY.md`.
7. Legacy targets: `legacy/TP3-REFERENCE.md`, `legacy/TP5-REFERENCE.md`, and
   `legacy/COMPATIBILITY-MATRIX.md` for older TP releases.

## Compatibility tags (snippets)
- `TP3`, `TP5+`, `TP7+`, `TP7-only`, `FPC TP`, and FPC-only notes indicate the
  minimum supported compiler/feature set.
- Keep TP-safe defaults (`{$MODE TP}{$H-}`) at the top of shared sources; wrap
  FPC-only code with `{$IFDEF FPC}`.

## Build commands (spot-check)
- TP7: `tpc yourprog.pas` (sources under 8.3 names; overlays: compile units with
  `/$o+ /$f+` and call `OvrInit`).
- FPC TP mode: `fpc -Mtp -Sh- -So yourprog.pas` (add `-gl` for debug). Use
  `-Fu`/`-Fl` to set unit/lib paths.

## Regression checklist (thin)
- Strings: ShortString concat/copy/insert/delete should handle 255-byte limits
  without silent truncation.
- Overflow/Range: run with `{$R+}{$Q+}` and expect RTE 201/214 on overflow to
  prove checks are active and TP7/FPC parity holds.
- File I/O: text copy with `IOResult`, typed-file CRUD, untyped block copy; compare
  outputs across TP7/FPC.
- Crt timing: measure `Delay` with `Dos.GetTime`; avoid busy loops to catch CRT
  timing drift or RTE200 on TP7.
> Run spot-checks on both TP7 and FPC builds.
Full list: `topics/REGRESSION-CHECKLIST.md`.

## Quick index
- Full index: `MAP.md`
- Compiler refs: `compilers/TP7-REFERENCE.md`, `compilers/FPC-REFERENCE.md`,
  `compilers/MODES-COMPARISON.md`
- Patterns: `topics/` + `examples/BASIC-PATTERNS.md`
- Portability/deep dives: `topics/DIRECTIVES-PORTABILITY.md`,
  `topics/PORTING-STRATEGIES.md`, `topics/DEBUGGING-PROFILING.md`,
  `topics/REGRESSION-CHECKLIST.md`
- Cheat sheets: `reference/TP7-UNITS-CHEATSHEET.md`,
  `reference/FPC-UNITS-CHEATSHEET.md`, `reference/UNITS-REFERENCE.md`
- Errors/terms: `reference/ERRORS-RUNTIME.md`, `reference/GLOSSARY.md`
- Legacy: `legacy/` directory

## Notes
- BGI/overlay artifacts (`.BGI`/`.CHR`/`.OVR`) are not included; use BINOBJ for
  embedding or provide your own drivers/fonts and TP linker artifacts.
- TP7 CRT patch (RTE200 fix) is not bundled; supply your own `CRT.TPU` and place
  it earlier in the unit search order if needed.
