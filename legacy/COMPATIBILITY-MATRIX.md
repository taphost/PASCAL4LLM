# Turbo Pascal Compatibility Matrix (TP3.0 vs TP5.0 vs TP7/FPC TP mode)

Quick feature/unit comparison to guide LLM code generation and porting. Assume 16-bit DOS targets for TP3/TP5/TP7; FPC TP mode defaults to flat memory unless targeting DOS.

| Area | TP3.0 (1986) | TP5.0 (1989) | TP7.0 | FPC 3.2.2 (`{$MODE TP}{$H-}`) |
| --- | --- | --- | --- | --- |
| Units / `uses` | ❌ (single source only) | ✔ Units; smaller RTL (`System`, `Crt`, `Dos`, `Graph`, `Printer`, `Turbo3`, `Graph3`, `Overlay`) | ✔ Full TP7 RTL (incl. Overlay/BGI) | ✔ Wide RTL/FCL; guard FPC-only units |
| Object model (`object`, virtual/dynamic) | ❌ | ❌ (arrives in 5.5) | ✔ | ✔ (Delphi-style classes in other modes) |
| Inline asm | ❌ (`Inline(...)` bytes only) | ❌ (`Inline(...)` bytes only) | ✔ `asm ... end` | ✔ `asm ... end` (Intel; AT&T on some targets) |
| Overlays | ❌ | ✔ (`$O+` + `{$O Unit}` in program; `Overlay` unit) | ✔ (`$O+` + `{$O Unit}`) | ✘ No TP-style overlays; split units/plugins |
| BGI / Graph | ❌ | ✔ BGI (`Graph` unit; fewer drivers/modes) | ✔ BGI (`Graph` unit) | ⚠️ Depends on target; consider SDL/WinBGI |
| Default string kind | ShortString (255) | ShortString (255) | ShortString (255) | ShortString with `{$H-}`; AnsiString with `{$H+}` |
| Default `Real` mapping | 6-byte `Real` (soft FP) | 6-byte `Real`; 8087 with `$N+`/`$E+` | 6-byte `Real`; 8087 with `$N+` | Maps `Real` to `Double`; `Real48` optional |
| Directives present | `$A,B,C,D,F,I,L,N,O,R,S,U,V,X` (CP/M extras `$W,$X`); no `$M,$G,$P,$H,$J` | `$A,B,D,E,F,I,L,N,O,R,S,V,X` + params `$I,$L,$M,$O unit`; no `$G,$H,$J,$P` | Full TP7 set (`$A..$Y`, `$M,$O unit,$G,$P,$J,$H`) | FPC set (superset; guard FPC-only) |
| Defaults (notable) | `$B+,$C+,$I+,$R-,$V+,$U-`; recursion off on CP/M unless `$A-`; no units | `$B-,$D+,$E+ (when $N+),$I+,$L+,$N-,$O-,$R-,$S+,$V+`; stack/heap `$M 16384,0,655360` | `$B-,$D+,$I+,$L+,$N-,$O-,$R-,$S+,$V+,$H-` (TP) | `$B-,$I+,$H-` (if set), `Integer` can be 16/32/64; many FPC-only switches |
| Reserved words (extras) | ANSI set; no `string`; `packed` is ignored (auto-pack) | ANSI set + `string` | ANSI set + `string` + TP7 extensions | FPC adds Delphi/ObjFPC keywords; guard with modes |
| Memory model | 64KB code/data; no overlays; recursion limited (CP/M) | 64KB segments; overlays available; 15 open files max | 64KB segments; overlays; 15 open files max | Flat (host- sized); DOS targets reintroduce 16-bit limits |
| Runtime error set | Small (hex-coded 01/02/03/04/10/11/90/91/92/F0/FF) | TP5 list (1–17, 100–106, 150–162, 200–209) | TP7 list (1–231) | TP7-compatible codes; exceptions when `SysUtils` used |
| File I/O | Text/typed/untyped; no FileMode; no device files | Text/typed/untyped; `FileMode`, DOS devices; `IOResult`/`{$I}` | Full TP7 file/device behavior | Richer; `FileMode` honored on DOS; SysUtils exceptions |
| Build tooling | TP3 `TURBO` menu/CLI; no `tpc` | `TURBO` IDE + `TPC` CLI; `.OVR` via `$O unit` | `tpc` with `/` switches; IDE | `fpc -Mtp -Sh- -So` (+ `$MODE TP{$H-}`) |

Legend: ✔ supported; ❌ not supported; ⚠️ available with caveats/target-specific behavior.
