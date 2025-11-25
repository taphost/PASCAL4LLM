# FPC Modes Comparison
Quick reference for selecting the right FPC mode and understanding feature availability.

| Mode          | Goal/Compatibility              | String Default | Integer Size         | OOP/Generics | Notes                                  |
|---------------|---------------------------------|----------------|----------------------|--------------|-----------------------------------------|
| TP            | Turbo Pascal syntax fidelity    | ShortString    | 16-bit (always in TP mode)   | TP-style OOP  | Best for TP7 ports; wrap directive for TP7 builds. |
| FPC           | Modern baseline                  | AnsiString     | Platform-dependent   | ✔            | Default mode; not TP7-compatible.       |
| OBJFPC        | Delphi-like (pre-Unicode)        | AnsiString     | Platform-dependent   | ✔            | Classes, overloading, generics.         |
| DELPHI        | Delphi syntax compatibility      | AnsiString     | Platform-dependent   | ✔            | Use for newer Delphi code migration.    |

## When to Use Each Mode
- **TP**: Cross-compat with TP7; disable features like overloading/generics.
- **FPC/OBJFPC/DELPHI**: Modern features; isolate from TP7 builds.

## Migration Paths
- Start in `TP` mode to secure baseline, then raise to `OBJFPC` or `DELPHI` behind `{$IFDEF FPC}` for new features.
- Re-test integer widths and `String` type expectations after switching modes.
