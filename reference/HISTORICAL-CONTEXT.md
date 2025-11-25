# Historical Context

Timeline and influence of Pascal variants relevant to TP7 and FPC.

## Turbo Pascal and Borland Pascal
- **1983-1992**: TP 1.0 → 7.0 adds units, OOP (TP5.5+), overlay system, BGI, `{$O}` directives. TP7 (1992) is the DOS baseline in this guide.
- **1992**: Borland Pascal 7.0 adds protected-mode tooling; TP7 syntax/RTL remains core.
- **Turbo Pascal for Windows**: separate product; units like WinCrt/WinDos are TPW-only (not TP7 DOS).

## Delphi influence
- **1995**: Delphi 1 introduces Object Pascal extensions (classes, exceptions, RTTI, units like SysUtils/Classes).
- FPC adopts Delphi-compatible modes (OBJFPC/Delphi) while also offering `{$MODE TP}` to mirror TP7 syntax; many modern FPC units mirror Delphi RTL/FCL design.

## Free Pascal
- **1993-**: FPC starts as Turbo Pascal compatible, grows cross-platform; current guide targets 3.2.2.
- Adds managed strings (`{$H+}`), generics, variants, wide chars, resource support, and extensive RTL/FCL/FCL-res beyond TP7 scope.

## Notes
- WinCrt/WinDos belong to TPW/Delphi1; avoid in TP7 DOS guidance.
- For API-level differences, see `compilers/MODES-COMPARISON.md` and `reference/UNITS-REFERENCE.md`.
