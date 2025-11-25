# TP7 RTL Summary (token-optimized)

Compact reference for Turbo Pascal 7.0 RTL/units. Use official TP7 manuals for full signatures/edge cases.

## System (core)
- Types/limits: 16-bit `Integer`, 6-byte `Real`, ShortString (`string[255]`).
- Memory: `New/Dispose`, `GetMem/FreeMem`, `Mark/Release`, `MemAvail/MaxAvail`, `HeapError`.
- Addresses: `Addr/Ofs/Seg/Ptr`, `Mem/Port` arrays (16-bit).
- Control: `Halt`, `ExitCode`, `ParamCount/ParamStr`, `Randomize/Random`.
- Directives (defaults): `$A+` align, `$B-` short-circuit, `$D+` debug info, `$E+` 8087 emu, `$F-` auto near/far, `$G-` 286 opcodes, `$I+` IO checks, `$L+` locals, `$M 16384,0,655360` stack/heap, `$N-` software FP, `$O-` overlays off, `$P-` open strings off, `$Q-` overflow off, `$R-` range off, `$S+` stack check, `$T-` untyped @, `$V+` strict var-string, `$X+` extended syntax, `$Y+` browser info.
- Numbers: `Succ/Pred`, `Inc/Dec`, `Odd`, `Abs/Sqr/Sqrt`, `Round/Trunc`, `Sin/Cos/ArcTan`, `Exp/Ln`.
- Sets: `Include/Exclude`, set ops.
- File types: `Assign/Reset/Rewrite/Close`, `Erase/Rename`, `Eof/Eoln`, `BlockRead/BlockWrite` (untyped), typed files (`file of`), `IOResult` (if `{$I-}`).
- Text: `Read/ReadLn/Write/WriteLn`, `Seek/SeekEof/SeekEoln`, `FilePos/FileSize`.
- Objects: `object` type, `constructor`/`destructor` (plain TP OOP), `New/Dispose` on objects.
- Pointers: near/far/huge pointers; `GetMem` returns near pointer; far/huge in specific contexts.
- Errors: runtime error numbers in `reference/ERRORS-RUNTIME.md`; `IOResult`/`InOutRes` for I/O; `DosError` for DOS/Exec calls.

## Crt
- Screen: `Window/WindMin/WindMax`, `ClrScr/ClrEol/DelLine/InsLine`, `GotoXY/WhereX/WhereY`, `TextColor/TextBackground`, `HighVideo/LowVideo/NormVideo`.
- Input: `KeyPressed/ReadKey`, `CheckBreak` (Ctrl+Break), `Delay(ms)` (RTE200 on fast CPUs—patch CRT/TINST or throttle DOSBox).
- Sound: `Sound/NoSound`.
- Exit: `Keep(ExitCode)` leaves to DOS without cleanup.

## Dos
- Exec: `Exec(Path, CmdLine)` with `SwapVectors`; check `DosError`.
- INT access: `MsDos/Intr`, `GetIntVec/SetIntVec`.
- Files/dirs: `FindFirst/FindNext/DoneFind` (8.3 names), `GetFTime/SetFTime`.
- Env/PSP: `GetEnv/EnvStr/EnvCount`; `PrefixSeg` points to PSP (cmd tail at $80).
- Date/time: `GetDate/GetTime/SetDate/SetTime`.
- Disk/dirs: `MkDir/RmDir/ChDir/GetDir`, `DiskFree/DiskSize`.
- Errors: `DosError` codes; map to RTE via `IOResult`/runtime.

## Overlay
- Init: `OvrInit('prog.ovr')`, `OvrInitEMS` (EMS buffer), `OvrSetBuf(Size)`, `OvrGetBuf`.
- Status: `OvrResult` codes (`ovrOk=0`, `ovrError=-1`, `ovrNotFound=-2`, `ovrNoMemory=-3`, `ovrIOError=-4`, `ovrNoEMSDriver=-5`, `ovrNoEMSMemory=-6`).
- Build: compile units with `{$O+}{$F+}`; in program add `{$O UnitName}` after `uses`; link to produce `.EXE + .OVR`; call `OvrInit` before overlaid routines.

## Graph (BGI)
- Init: `DetectGraph/Detect`, `InitGraph(Drv,Mode,Path)`, check `GraphResult/GraphErrorMsg`.
- Styles: `SetTextStyle/SetLineStyle/SetFillStyle/SetColor/SetBkColor`.
- Drawing: `Line/Rectangle/Circle/Arc/Bar/OutText`.
- Buffers: `SetGraphBufSize` before `InitGraph`.
- Embedding: convert `.BGI`/`.CHR` via `BINOBJ foo.bgi foo.obj`; link; `RegisterBGIdriver/RegisterBGIfont` before `InitGraph`/`SetTextStyle`.

## Strings / Strings unit
- Built-ins: `Str/Val`, `Copy/Pos/Delete/Insert`, `Concat`.
- Buffer ops: `Move`, `FillChar`.
- PChar helpers (with `{$X+}` or Strings unit): `StrCopy/StrLen/StrComp`.
- Note: default ShortString (`string[255]`); no AnsiString/dynamic arrays.
- String comparison: `CompareStr` (if available), else manual; `UpCase` for char.
- Padding/format: use `Write/WriteLn` with field widths for formatting.

## Printer
- `AssignPrn(Text)`; `WriteLn` to LPT; call `Flush`; check `IOResult` if `{$I-}`.

## Legacy units
- `Turbo3`, `Graph3` for backward compatibility; prefer modern `Crt/Graph`.

## Objects/OOP specifics
- Declaration: `type TObj = object ... end;` Methods may be `virtual`/`dynamic`; no `class` type. No `inherited` keyword—call ancestor explicitly (`Ancestor.Method`).
- Constructors/destructors: `constructor Init(...)` often returns `Boolean` success; `destructor Done;` usually void. Allocate with `New(PObj, Init(...))`; free with `Dispose(PObj, Done)`.
- Method tables: virtual/dynamic dispatch only; no interfaces; no exception support built-in (exceptions are FPC/Delphi features).

## Inline ASM / calling
- `asm ... end;` Intel syntax, 16-bit; pascal calling convention preserves BP/SI/DI/DS/ES; caller-saves AX/CX/DX.
- Use segment registers cautiously; restore on exit. No AT&T mode. No inline machine ops beyond 16-bit x86 set supported by TP7.

## Overlay/tooling notes
- Overlays rely on TP linker; no external `OVRMERGE` step needed—`tpc` emits `.EXE` + `.OVR` when `{$O unit}` is present in the program.
- BINOBJ use is for BGI drivers/fonts; overlay manager is built-in to `Overlay` unit. No overlay artifacts included here; build with TP toolchain.

## Other RTL units (not detailed)
- `Printer` variations, `Turbo3/Graph3` legacy behaviors, `WinCrt/WinDos` (TPW-only, not TP7 DOS; see `reference/HISTORICAL-CONTEXT.md`), any TPW-specific units. Rare/edge symbols are best checked against TP7 manuals if needed.
