# Free Pascal Reference (3.2.2)
FPC-specific behavior, extensions, and setup notes with emphasis on TP compatibility.
See also the baseline matrix in `../CORE-COMPATIBILITY.md` for shared TP7/FPC guidance.
For quick RTL/FCL APIs, see `reference/FPC-UNITS-CHEATSHEET.md` (3.2.2).

## Installation and Configuration
- **Version pin**: target FPC **3.2.2** so wording matches shipped RTL/FCL docs.
- **Install**: use platform packages/installer; add the compiler `bin` folder to `PATH`.
- **Config file**: `fpc.cfg` (global) plus project-local `fpc.cfg` if present. Keep TP-oriented defaults: `-Mtp -Sh- -So`.
- **Unit search order**: current directory, paths from `-Fu`, then defaults in `fpc.cfg`. Keep priority predictable when mixing TP7 source and FPC-specific units.
- **Testing**: run `fpc -vt` to verify config paths; enable debug info with `-gl` when comparing behaviors against TP7.

## Modes and Compatibility
- Use `{$IFDEF FPC}{$MODE TP}{$ENDIF}` in shared code. TP mode aligns syntax but keeps modern backend defaults.
- OBJFPC/Delphi modes introduce classes, generics, AnsiString by default; not TP7-compatible.
- `Integer` size follows the platform in OBJFPC/Delphi (16/32/64-bit). Avoid assuming 32-bit universally.

## FPC-Only Features
- AnsiString, UnicodeString, dynamic arrays, generics, overloading, exceptions.
- Extended unit set: `SysUtils`, `Classes`, `Math`, `StrUtils`, `CustApp`, etc.
- Inline ASM supports Intel on most targets; AT&T syntax available on some Unix targets.
- Additional directives not in TP7: `$BITPACKING`, `$CODEALIGN`, `$CALLING`, `$COPERATORS`, `$CHECKPOINTER`, `$PACKENUM`, `$PACKRECORDS`, `$MAXFPUREGISTER`, `$SATURATION`, `$OUTPUT_FORMAT`, `$TYPEINFO`, `$STATIC`, `$SMARTLINK`, `$REFERENCEINFO` (`$Y`), and message controls (`$NOTE/$HINT/$WARNINGS`).

## Cross-Platform Notes
- Flat memory model; no segment:offset considerations.
- File paths support long names; still prefer portable separators and case-insensitive handling when sharing with TP7 paths.
- Console I/O is buffered differently than TP7; CRT timing can differ on fast hosts.

## Directive Cheatsheet (TP mode defaults vs. overrides)
- **Boolean eval**: default `$B-` short-circuits `and`/`or`; `$B+` forces complete evaluation (no short-circuit).
- **Overflow/Range**: defaults are `$Q-` and `$R-`; turn on `$Q+`/`$R+` for debug and parity checks, disable only when matching TP7 behavior.
- **I/O**: default `$I+` raises runtime errors on IO failures; `$I-` returns status in `IOResult` TP-style.
- **Var-string checks**: default `$V-` (ShortString refs considered compatible); `$V+` enforces exact ShortString lengths.
- **Extended syntax**: default `$X+` (drop function results, pointer arithmetic); `$X-` disables.
- **Strings**: default `$H-` keeps `String` as `ShortString`; `$MODE DELPHI` implies `$H+` (AnsiString), otherwise enable `$H+` explicitly when needed.
- **Packing/Alignment**: `$PACKRECORDS n` and `$PACKENUM n` set record/enum sizes; `$ALIGN`/`$CODEALIGN` tune code/data alignment.
- **Calling conventions**: `$CALLING` sets default (e.g., `cdecl`, `stdcall`); `$ASMMODE` picks assembler dialect (default AT&T reader—set `{$ASMMODE intel}` for TP-style Intel syntax).
- **Linking/format**: `$SMARTLINK` and `$STATIC` influence linking; `$OUTPUT_FORMAT` selects target (ELF/PE); `$APPID/$APPTYPE` for Win/Palm targets.
- **Info messages**: `$HINTS/$NOTES/$WARNINGS/$REFERENCEINFO` toggle emission of diagnostics.

## Language Extensions vs TP7
- **Generics and operator overloading** live only in OBJFPC/Delphi modes. If needed, isolate them in FPC-only units and specialize with explicit types; errors surface when generic params are invalid during specialization.
- **Set/record/class size**: packing directives affect layout; keep `$PACKRECORDS 1/2/4/8/16` explicit when sharing binaries.
- **Strings**: resource strings and longstring types exist; guard them behind `{$IFDEF FPC}` in shared code.
- **Typed constants** may be writable in TP7 but read-only in FPC by default; use `{$J+}` only when required.
- **Runtime errors**: consult `reference/ERRORS-RUNTIME.md` and map TP7 error numbers to FPC equivalents; `{$R+}` can elevate silent wraparound to run-time failures.

## Libraries to Expect (RTL/FCL/FCL-res)
- **RTL**: `System`, `SysUtils`, `Classes`, `StrUtils`, `Math`, `DOS`, `CRT`, `BaseUnix`/`Unix`. Behavior (e.g., timezone/locale, `Delay`, path case-sensitivity) can differ from TP7—note deviations in examples.
- **FCL**: utility units such as `Process`/`Pipes` (spawning), `CustApp` (console app framework), `fpjson` (JSON DOM), `URIParser`, `Zipper`/`ZStream` (archive/compression), `SQLDB` connectors. Avoid them in TP7 targets; document when an example is FPC-only.
- **FCL-res**: resource readers/writers for COFF/PE, version info (`versionresource`, `versiontypes`), and resource classes (`bitmapresource`, `acceleratorsresource`). Use for Windows resource embedding in FPC; TP7 lacks these.

## Documentation Tooling (fpdoc)
- `fpdoc` builds API docs; key options: `--auto-index`, `--auto-toc`, `--charset=utf-8`, `--descr=...` to include descriptive files, `--content=...` to add extra pages.
- Generate CHM/HTML by setting `--format=chm` or `--format=html`; supply `--css-file` to style output and `--output` to control destination folder.
- Keep contributor instructions in sync with `UPDATE` files when fpdoc options change.
- Example (HTML): `fpdoc --project=myproj.xml --format=html --auto-index --auto-toc --descr=README.md --output=docs/html`

## References
- Mode comparison table: `compilers/MODES-COMPARISON.md`
- TP-mode deviations and directives: `topics/DIRECTIVES-PORTABILITY.md`
- Cross-platform porting tips: `topics/FILE-HANDLING.md`, `topics/ASSEMBLY.md`
