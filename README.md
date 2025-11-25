# Pascal4LLM: Turbo & Free Pascal docs 

Documentation set to help humans and tools produce Turbo Pascal 7.0 and Free
Pascal 3.2.2 (TP mode) code. This README is for human readers. LLMs and
automation should start from `API.md`.

## Start here
- LLM/tooling entry point: `API.md`
- Human index: `MAP.md`
- TP7/FPC quick APIs: `reference/TP7-UNITS-CHEATSHEET.md`,
  `reference/FPC-UNITS-CHEATSHEET.md`
- Legacy notes: `legacy/` for TP3/TP5 compatibility

## What's inside
- `compilers/`: TP7 and FPC TP mode references, plus modes comparison.
- `topics/`: guides on strings/I/O, file handling, memory model, graphics/BGI,
  assembly, directives/compatibility, porting, debugging/profiling, and
  regression checks.
- `examples/`: `BASIC-PATTERNS.md` with TP-safe snippets.
- `reference/`: cheat sheets, RTL/FCL summaries, glossary, runtime errors,
  DOSBox configuration.
- `legacy/`: TP3/TP5 reference and compatibility matrix.
- Core files: `CORE-COMPATIBILITY.md`, `MAP.md`, `LICENSE`.

## Project structure

```
.
|- README.md
|- API.md
|- MAP.md
|- CORE-COMPATIBILITY.md
|- compilers/
|  |- FPC-REFERENCE.md
|  |- TP7-REFERENCE.md
|  \- MODES-COMPARISON.md
|- topics/
|  |- STRINGS-IO.md
|  |- FILE-HANDLING.md
|  |- MEMORY-MODEL.md
|  |- GRAPHICS-BGI.md
|  |- ASSEMBLY.md
|  |- DIRECTIVES-PORTABILITY.md
|  |- PORTING-STRATEGIES.md
|  |- DEBUGGING-PROFILING.md
|  \- REGRESSION-CHECKLIST.md
|- examples/
|  \- BASIC-PATTERNS.md
|- reference/
|  |- TP7-UNITS-CHEATSHEET.md
|  |- TP7-RTL-SUMMARY.md
|  |- FPC-UNITS-CHEATSHEET.md
|  |- FPC-RTL-FCL-SUMMARY.md
|  |- UNITS-REFERENCE.md
|  |- ERRORS-RUNTIME.md
|  |- GLOSSARY.md
|  \- DOSBOX-CONFIGURATION.md
|- legacy/
|  |- TP3-REFERENCE.md
|  |- TP5-REFERENCE.md
|  |- COMPATIBILITY-MATRIX.md
|  \- COMMAND-LINE-CHEATSHEET.md
\- LICENSE
```

## Quick usage (humans)
- Portability defaults: `CORE-COMPATIBILITY.md` (TP7/FPC shared directives and
  type sizes).
- Target TP7: `compilers/TP7-REFERENCE.md`; target FPC TP mode:
  `compilers/FPC-REFERENCE.md`; compare modes via
  `compilers/MODES-COMPARISON.md`.
- Patterns and recipes: `topics/` guides plus `examples/BASIC-PATTERNS.md`.
- Advanced compatibility/debug: `topics/DIRECTIVES-PORTABILITY.md`,
  `topics/PORTING-STRATEGIES.md`, `topics/DEBUGGING-PROFILING.md`,
  `topics/REGRESSION-CHECKLIST.md`.
- Fast lookups: `reference/UNITS-REFERENCE.md`, TP7/FPC cheat sheets, runtime
  errors, and glossary in `reference/`.
- Legacy targets: use `legacy/` for TP3/TP5 constraints and CLI notes.

## Build/verify snippets
- TP7: `tpc yourprog.pas` (keep sources under 8.3 names).
- FPC TP mode: `fpc -Mtp -Sh- -So yourprog.pas` (add `-gl` for debug). Use
  `-Fu`/`-Fl` to set unit/lib paths.
- BGI/overlay artifacts (`.BGI`/`.CHR`/`.OVR`) are not bundled; see
  `topics/GRAPHICS-BGI.md` and `topics/MEMORY-MODEL.md` plus the overlay note in
  `examples/BASIC-PATTERNS.md`.
- TP7 CRT patch for RTE200 is not included; supply your own `CRT.TPU` if needed.

## Contributing
- Keep shared defaults TP-safe (`{$MODE TP}{$H-}`) and guard FPC-only code with
  `{$IFDEF FPC}`.
- Prefer 2-space indents and line length near 80 characters.
- Use relative links and note when behavior needs verification.
- Align cheat sheets with official TP7/FPC docs when updating.

## License
- MIT License - See individual files for details.

---
