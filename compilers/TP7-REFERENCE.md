# Turbo Pascal 7 Reference
TP7-specific behavior, limits, and tooling for DOS real-mode development.

## Platform Profile
- 16-bit real-mode compiler and RTL; 64KB segment limits apply to code and data.
- Uses single language mode; no `{$MODE}` directive support.
- Overlay system available via `{$O filename}`; `{$O+}`/`{$O-}` toggle overlay-safe codegen (not optimization).

## Compiler Directives (defaults)
- Command-line `tpc /$...` toggles the same switches as inline directives (except `$M` cannot be comma-joined). Defaults from the TP7 guides:
| Dir | Default | Scope | Notes |
| --- | --- | --- | --- |
| `$A` | `+` | Global | Word-align vars/typed consts; `-` packs on byte boundaries. |
| `$B` | `-` | Local | `-` short-circuits Boolean `and`/`or`; `+` evaluates fully. |
| `$D` | `+` | Global | Emit line number tables (needed for TD/find-error). |
| `$E` | `+` | Global | Link 80x87 emulator when `$N+` in a program. Ignored in units. |
| `$F` | `-` | Local | `+` forces far calls; `-` auto (far in unit interfaces, else near). |
| `$G` | `-` | Global | `+` allows 80286-only opcodes (ENTER/LEAVE, etc.). |
| `$I` | `+` | Local | I/O checking `+` raises RTEs; `-` leaves `IOResult` pending. |
| `$L` | `+` | Global | Emit local symbol info (pairs with `$D`, `$Y`). Ignored if `$D-`. |
| `$M` | `16384,0,655360` | Global | Stack bytes, heap min, heap max. Stack range 1024–65520. |
| `$N` | `-` | Global | `+` generate 80x87 code, `-` uses software FP. |
| `$O` | `-` | Global | Overlay-safe codegen; pair with `$F+` for overlaid units. |
| `$O unit` | — | Local | Mark a unit to go into `.OVR`; only in programs, after `uses`. |
| `$P` | `-` | Local | `+` treats `var string` params as open strings. |
| `$Q` | `-` | Local | Overflow checks on `+` (`+,-,*,Abs,Sqr,Succ,Pred`). |
| `$R` | `-` | Local | Range checks on (`array`, `string`, subrange, virtual calls). |
| `$S` | `+` | Local | Stack overflow checks injected at routine entry. |
| `$T` | `-` | Global | `@` yields untyped `Pointer`; `+` makes typed pointers. |
| `$V` | `+` | Local | Strict `var string` type check; `-` relaxed/unsafe. |
| `$X` | `+` | Global | Extended syntax (discard function result, PChar rules). |
| `$Y` | `+` | Global | Browser symbol refs; needs `$D+` and `$L+`. |
Defaults confirmed against Turbo Pascal 7 Programmer's Reference (1992).

## TPC Command-Line Options (checked vs. TP7 manuals)
- Switch sets: `/$R-/$I-/$V-/$F+` etc. behave like leading in-file directives. `/$M` stands alone (`/$Mstack,heapmin,heapmax` in bytes/hex). Defaults mirror the table above.
- Paths: `/Tpath` (TPL+CFG search, must be first), `/Epath` (EXE/TPU output dir, single), `/Ipath1;path2` (include), `/Upath1;path2` (units), `/Opath1;path2` (OBJ).
- Build: `/M` (Make modified units), `/B` (Build all deps), `/Q` (quiet), `/L` (link buffer on disk for large builds).
- Debug/map: `/G[S|P|O]` map file (segments | +publics | +line numbers), `/N` embed standalone debugger info in EXE (pair with `$D+/$L+`), `/Fseg:ofs` find-error lookup (requires debug info).
- Directives via `/$(letter){+|-}`: sets initial state for all switch directives (A,B,D,E,F,G,I,L,N,O,P,Q,R,S,T,V,X,Y). Can be comma-joined except `$M` (e.g., `/$R-,I-,V-,F+`).
- Defines: `/DNAME1;NAME2` sets conditional symbols (like `{$DEFINE NAME1}` etc.).
- Quick APIs: see `reference/TP7-UNITS-CHEATSHEET.md` for TP7 unit tables (no condensed FPC equivalent; use RTL/FCL docs).

### TPC usage examples
- Range checks off, build all, map with publics: `tpc /$R- /B /GP myprog.pas`
- Find run-time error `seg:ofs` (requires `$D+/$L+`): `tpc myprog /F0:3D`
- Set stack/heap, overlay-ready units, and debug info: `tpc /$M16384,0,655360 /$O+ /$F+ /N myprog.pas`

## Memory Model
- Segment:offset addressing; near pointers stay within a segment, far/huge cross segments.
- Heap managed in conventional memory; fragmentation matters. Hand-tune allocation sizes.
- See `topics/MEMORY-MODEL.md` for near/far/huge patterns and overlay guidance.

## TP7-Only Units
- `Graph`, `Overlay`, `Crt`, `Dos`, `Printer`; DOS-exclusive behavior.
- `WinCrt` and `WinDos` belong to **Turbo Pascal for Windows (TPW)**, not TP7 for DOS. Avoid citing them as TP7 units.

## DOS Considerations
- File paths limited to 8.3; use uppercase and short names.
- CRT timing can be hardware/emulator dependent; validate `Delay` under DOSBox variants.
- Use `Assign`/`Reset`/`Rewrite` patterns; check `IOResult` after operations when `{$I-}` is active.

## Runtime Behavior Notes
- Runtime errors follow TP7 numbering (e.g., RTE 200 for `Delay` overflow). Mapping goes in `reference/ERRORS-RUNTIME.md`.
- No AnsiString or dynamic arrays; rely on ShortString and static arrays.

## Installation / Setup
- Recommended: DOSBox or DOSBox-X. Configure XMS/EMS if using overlays or large heaps.
- Keep source under 8.3 paths; mount project directory via DOSBox `MOUNT` command.
- Use `TINST` for CRT speed calibration when needed.
