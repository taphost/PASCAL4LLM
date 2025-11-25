# Command-Line Cheat Sheet (TP3/TP5/TP7/FPC)

Concise invocation notes for each toolchain. Use inline directives in source when possible; command-line switches mirror the same defaults but differ per version. Keep 8.3 filenames for TP targets.

## TP3.0
- Compiler invocation: `TURBO` (interactive IDE/editor). There is no standalone `tpc` for TP3. All options are set via the TURBO menu; keep `TURBO.OVR` (overlays for the IDE) and optionally `TURBO.MSG` (compile-time messages) on the work disk.
- Command-line flags for the compiler are not exposed like later `tpc` builds; rely on in-source directives for checks (`{$B/C/I/R/V/U}` etc.) and keep code monolithic (no units).
- Reserved words only include the ANSI set (no `string` keyword); use ShortString.

## TP5.0
- Command-line compiler: `TPC` (plus `TURBO` IDE). Syntax: `TPC [switches] file.pas`
- Switch style: `/` prefixes; inline directives can be prefixed globally via `/$<letter>{+|-}` (e.g., `/$R-/$I-/$O+/$F+`).
- Paths: `/Ipath1;path2` (include), `/Upath1;path2` (units), `/Opath1;path2` (object files). Uses `TPC.CFG` for defaults if present. 
- Build output: compile to memory (`TURBO` IDE) or EXE; overlays require units compiled with `{$O+}` and program directives `{$O Unit}`.
- Use TP5’s directive set: no `$H/$J/$P/$G`; `$M stack,heapmin,heapmax` is supported in programs.

## TP7.0
- Command-line compiler: `tpc [switches] file.pas`
- Switch grouping: `/$(letter){+|-}` to set default directives (e.g., `/$R-,I-,V-,F+`); `$M` stands alone (`/$Mstack,heapmin,heapmax`).
- Paths: `/Tpath` (TPL/CFG search, must be first), `/Epath` (EXE/TPU output), `/Ipath1;path2` (includes), `/Upath1;path2` (units), `/Opath1;path2` (OBJ search).
- Build: `/M` (make), `/B` (build all), `/Q` (quiet), `/L` (link buffer on disk). Map/debug: `/G[S|P|O]` (map variants), `/N` (embed debugger info), `/Fseg:ofs` (find-error lookup; needs `$D+/$L+`).
- Overlays: compile units with `/$O+ /$F+`; in program use `{$O Unit}` and call `OvrInit`. Keep 8.3 filenames.

## FPC 3.2.2 (TP mode)
- Compiler: `fpc -Mtp -Sh- -So file.pas` (add `-gl` for debug; `-Fu`/`-Fl` for unit/lib paths).
- Useful flags: `-Sg-` (disable goto checks if needed), `-Cr`/`-Co` (range/overflow), `-CX`/`-XX` (smartlinking), `-dSYM` macros with `-dNAME`.
- Config: `fpc.cfg` supports `-d` (defines), `-Fu` (units), `-Fl` (libs); per-project config possible. Guard FPC-only units with `{$IFDEF FPC}` and keep `{$H-}` for ShortString parity.
