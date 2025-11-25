# Units Reference

Catalog of standard units with compatibility notes and sample signatures.
For cross-compiler basics, start from `../CORE-COMPATIBILITY.md`.
Compatibility tags: TP5+, TP7, FPC TP. TP3 lacks units; overlay/BGI sections are TP7-only.

## Core Units (shared)
Compatibility: TP5+, TP7, FPC TP (TP3 has no units)
- `System`: base types, memory management, arithmetic helpers. Beware `Real` = Double in FPC.
- `Dos`: file/path handling and environment access; long paths behave differently in FPC.
- `Crt`: console + keyboard; timing differs on fast hosts/emulators.
- `Printer`: text output to LPT/printer; mostly legacy. In FPC maps to modern devices.
- TP7 extras: `Overlay`, `Graph` for BGI graphics (see `topics/GRAPHICS-BGI.md`).
- See also: `TP7-UNITS-CHEATSHEET.md` for compact TP7 APIs and `FPC-UNITS-CHEATSHEET.md` for FPC 3.2.2 RTL/FCL quick refs.
- Summaries: `TP7-RTL-SUMMARY.md` (broader TP7 RTL) and `FPC-RTL-FCL-SUMMARY.md` (broader FPC RTL/FCL).

## TP7 Unit specifics (from TP7 guides)
Compatibility: TP7-only (FPC requires alternatives; TP3/TP5 may lack some units)
- **Crt** — `Delay(ms)` uses BIOS timer calibration; keep `{$S+}` and expect RTE 200 on very fast CPUs (TP7 patches/dosbox help). `CheckBreak` guards Ctrl+Break; `WhereX/WhereY` respect `WindMin/WindMax`; `Keep` exits to DOS without closing the program window. Calibrate timing with `TINST` if available; prefer `Delay` over busy loops.
- **Dos** — `Exec` should be wrapped with `SwapVectors` and checked via `DosError`; `GetIntVec/SetIntVec`/`MsDos`/`Intr` give direct INT access. `PrefixSeg` points at the PSP (command tail at offset $80). Only 8.3 filenames; stay short/uppercase.
- **Overlay** — compile units with `{$O+}` + `{$F+}`, mark overlays in the program with `{$O UnitName}` after `uses`, call `OvrInit('prog.ovr')` and check `OvrResult`. `OvrSetBuf` grows the overlay buffer at the expense of heap space. (No overlay demo/OVR is shipped; treat this as a template—see `examples/BASIC-PATTERNS.md` for the skeleton header.)
- **Graph** — `InitGraph(Drv,Mode,Path)` loads `.BGI` from `Path` ('' = current dir, or `BGI` env var). Register embedded drivers/fonts (`RegisterBGIdriver`/`RegisterBGIfont`) after converting `.BGI`/`.CHR` via `BINOBJ` and linking the `.OBJ`. `CloseGraph` releases driver memory; see `topics/GRAPHICS-BGI.md` for tables/embedding steps.

### TP7 unit cheat sheets
- **Crt** (timing/console): `Delay(ms)` (RTE200 risk on very fast CPUs; use patched CRT or DOSBox cycle clamp), `Gotoxy/WhereX/WhereY`, `TextColor/TextBackground`, `ClrScr/ClrEol`, `KeyPressed/ReadKey`, `CheckBreak`, `Window/WindMin/WindMax`, `Keep` (exit to DOS without cleanup), `HighVideo/LowVideo/NormVideo`.
- **Dos** (INT/DOS helpers): `Exec` + `SwapVectors` + `DosError` check; `GetIntVec/SetIntVec`; `MsDos`/`Intr` for INT calls; `GetDate/GetTime` (DOS date/time); `EnvCount/GetEnv/PrefixSeg/SwapVectors`; `FindFirst/FindNext` (8.3 only); `GetFTime/SetFTime`; `GetEnv/EnvStr`.
- **Overlay** (OVR flow): `OvrInit`/`OvrInitEMS`, `OvrResult` (`ovrOk=0`, `ovrNotFound=-2`, `ovrNoMemory=-3`, `ovrIOError=-4`, `ovrNoEMSDriver=-5`, `ovrNoEMSMemory=-6`), `OvrSetBuf` (resizes buffer, shrinks heap), `OvrGetBuf`.
- **Strings/Printer**: `Str`/`Val`/`Copy`/`Delete`/`Insert`; PChar helpers (`StrCopy/StrLen/StrComp`) with `{$X+}`; `AssignPrn` + `WriteLn` + `Flush` for LPT output.

#### Patching CRT for Delay (RTE 200)
- Known fix: apply Borland’s CRT patch (`TPPATCH` or equivalent) to `CRT.TPU`/`CRT.PAS` in your TP7 install. Place the patched `CRT.TPU` in the project directory (or ahead in the unit search path) so TP7 links it instead of the original in `TURBO.TPL`.
- Alternative: throttle DOSBox cycles to avoid triggering the overflow; in FPC, `Delay` is safe but not cycle-accurate.

## FPC RTL Highlights (3.2.2)
Compatibility: FPC-only
- `SysUtils`: exceptions, file utilities, formatting (`FormatDateTime`, `SysErrorMessage`); TP7 lacks these—guard with `{$IFDEF FPC}`.
- `Classes`: streams (`TFileStream`, `TStringStream`), component model; use for buffered I/O instead of manual block reads when not targeting TP7.
- `StrUtils`: string helpers (e.g., `AnsiStartsText`, `ReplaceText`).
- `Math`: extended math functions; check range/overflow expectations if mixing with TP7 `Real`.
- `BaseUnix`/`Unix`: POSIX bindings; not portable to TP7/DOS.
- `SysUtils.AssertErrorProc`: maps `Assert` to RTE 227 (`EAssertionFailed` with SysUtils); see `reference/ERRORS-RUNTIME.md` and `reference/GLOSSARY.md` for exception class names.

## FCL Highlights
Compatibility: FPC-only
- Processes & IPC: `Process`, `Pipes`, `CustApp`, `DaemonApp` for daemon/CLI scaffolding.
- Data & serialization: `fpjson`, `fpmimetypes`, `Zipper`/`ZStream` (zip), `libtar`.
- Networking/DB: `fphttpclient` (if present in install), `SQLDB` connectors (`IBConnection`, `MSSQLConn`, `SQLite3Conn`, `PQConnection`, `ODBCConn`, etc.).
- Utilities: `URIParser`, `IniFiles`, `BufDataset`/`MemDS` for in-memory datasets, `SyncObjs` for threading primitives.
- Usage: mark examples as **FPC-only**; TP7 cannot link these units.

## FCL-res (Resources)
Compatibility: FPC-only
- Units like `resource`, `coffreader`/`coffwriter`, `winpeimagereader`, `bitmapresource`, `versionresource`, `acceleratorsresource`.
- Purpose: read/write Windows resource sections (COFF/PE), embed bitmaps/strings/version info. TP7 has no equivalent; wrap behind `{$IFDEF FPC}`.
- Key types: `TResources`, `TCoffResourceReader/Writer`, `TWinPEImageResourceReader`, `TVersionResource` with `TVersionStringTable`/`TVersionVarFileInfo`.

## Documentation tooling
- `fpdoc` can parse units above to generate API docs (`fpdoc --auto-index --auto-toc --descr=...`), producing HTML/CHM. Keep CSS/output paths documented in contributor guides.

## Example snippets
Compatibility: TP7, FPC TP (guard FPC-only units); TP3 lacks units
- **Process (TP-safe)** — runs an external command via `Dos.Exec`, valid for TP7/FPC TP:

Compatibility: TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$ENDIF}
uses Dos;
begin
  SwapVectors;
  Exec(GetEnv('COMSPEC'), '/C DIR');
  SwapVectors;
  if DosError <> 0 then
    Writeln('Exec failed: ', DosError);
end.
```

- **File I/O (TP-safe)** — manual error handling with `{$I-}` + `IOResult`:

Compatibility: TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$ENDIF}
{$I-}
var
  F: Text;
  Line: string[255];
begin
  Assign(F, 'input.txt');
  Reset(F);
  if IOResult <> 0 then Halt(2);
  while not Eof(F) do
  begin
    ReadLn(F, Line);
    WriteLn(Line);
    if IOResult <> 0 then Halt(100);
  end;
  Close(F);
end.
{$I+}
```

- **Process (FPC-only)** — run a command and capture output via `RunCommand` (unit `Process`):

Compatibility: FPC-only
```pascal
uses Process;
var
  OutStr: string;
begin
  if RunCommand('/bin/echo', ['hello'], OutStr) then
    WriteLn(OutStr);
end.
```

- **Thread (FPC-only)** — minimal `TThread` (TP7 has no threads):

Compatibility: FPC-only
```pascal
uses Classes, SysUtils;

type
  TWorker = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TWorker.Execute;
begin
  while not Terminated do
    Sleep(10);
end;

begin
  with TWorker.Create(False) do
  begin
    FreeOnTerminate := True;
  end;
  ReadLn; { Keep alive }
end.
```

- **Resource (FPC-only)** — include a `.res` generated from an `.rc` file (via `fpcres`):

Compatibility: FPC-only
```pascal
{$IFDEF FPC}{$R version.res}{$ENDIF}
begin
  Writeln('Version info linked via resource.');
end.
```

`version.rc` example:

```
1 VERSIONINFO
FILEVERSION 1,0,0,0
FILEOS 0x40004
FILETYPE 0x1
```

- **JSON (FPC-only)** — parse and read fields with `fpjson` / `jsonparser`:

Compatibility: FPC-only
```pascal
uses fpjson, jsonparser;
var
  Obj: TJSONObject;
begin
  Obj := TJSONObject(GetJSON('{"name":"tpfp","v":1}'));
  try
    WriteLn(Obj.Strings['name'], ' ', Obj.Integers['v']);
  finally
    Obj.Free;
  end;
end.
```

- **Zip (FPC-only)** — create a zip archive with `Zipper`:

Compatibility: FPC-only
```pascal
uses Zipper;
var
  Z: TZipper;
begin
  Z := TZipper.Create;
  try
    Z.FileName := 'out.zip';
    Z.Entries.AddFileEntry('hello.txt', 'hello.txt');
    Z.ZipAllFiles;
  finally
    Z.Free;
  end;
end.
```

## Index maintenance
- Keep signatures/behaviors in sync with this breviary when source content changes.
- Add brief, tested snippets per unit (TP7-safe vs. FPC-only) as coverage expands; avoid embedding full reference texts.
