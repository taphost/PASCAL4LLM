# Basic Patterns

Starter code snippets that compile on TP7 and FPC TP mode. Each sample starts
with `{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}` to lock in TP syntax and
ShortString defaults. When doing file I/O, use `{$I-}` + `IOResult` to keep
error handling explicit and TP-compatible.

**Compatibility tags** (minimum version): `TP3+`, `TP5+`, `TP7+`, `TP7-only`, `FPC TP`. TP3 cannot use units/BGI/overlays; overlay code remains TP7-only and is unsupported in FPC.

## Template skeleton (copy/paste)
Compatibility: TP3+ (no units in TP3), TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
program Demo;
{ Avoid managed types; add {$I-} and IOResult checks if doing file I/O. }
begin
  { code here }
end.
```

## Covered Patterns
- All snippets are TP-safe (no managed types); keep the directive prologue and
  `{$I-}` + `IOResult` style for I/O parity between TP7 and FPC TP mode.
- Procedural variables (`ProcVar`): switch call targets at runtime and reuse the same call site.
- Quicksort on a static array (`QuickSortDemo`): sorts 1..1000 ints; prints first 20 sorted values.
- Input validation (`InputCheck`): loops until a 1–10 value is entered.
- Text config read/write (`ConfigDemo`): writes two lines, re-reads, and echoes them.
- TP-safe `Exec` (`RunDir`): `SwapVectors` + `Exec(COMSPEC, '/C DIR')`; shows `DosError` on failure.
- Typed-file CRUD template (`TypedCrud`): create/read/update a small record with explicit `IOResult` checks.
- Untyped block copy (`CopyBin`): copy a binary file in chunks with `BlockRead`/`BlockWrite`.
- ShortString boundary guard (`ShortStringGuard`): show truncation behavior and safe slicing.
- TP7 overlay skeleton (`OvrTemplate`): template for placing units in an `.OVR` (no overlay units/artifacts are shipped here).

## Procedural variables
Compatibility: TP3+ (no units in TP3), TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
program ProcVar;
type
  TIntFunc = function(x, y: Integer): Integer;
var
  F: TIntFunc;
function AddEm(x, y: Integer): Integer; begin AddEm := x + y; end;
function SubEm(x, y: Integer): Integer; begin SubEm := x - y; end;
procedure DoIt(Func: TIntFunc; a, b: Integer);
begin
  Writeln(Func(a, b):5);
end;
begin
  DoIt(AddEm, 1, 2); DoIt(SubEm, 1, 2);
  F := AddEm; DoIt(F, 3, 4);
  F := SubEm; DoIt(F, 3, 4);
end.
```

## In-memory quicksort
Compatibility: TP3+ (no units in TP3), TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
program QuickSortDemo;
const Max = 1000;
type TList = array[1..Max] of Integer;
var Data: TList; i: Integer;
procedure QuickSort(var A: TList; Lo, Hi: Integer);
  procedure Sort(l, r: Integer);
  var i, j, x, y: Integer;
  begin
    i := l; j := r; x := A[(l + r) div 2];
    repeat
      while A[i] < x do Inc(i);
      while x < A[j] do Dec(j);
      if i <= j then
      begin y := A[i]; A[i] := A[j]; A[j] := y; Inc(i); Dec(j); end;
    until i > j;
    if l < j then Sort(l, j);
    if i < r then Sort(i, r);
  end;
begin Sort(Lo, Hi); end;
begin
  Randomize;
  for i := 1 to Max do Data[i] := Random(30000);
  QuickSort(Data, 1, Max);
  for i := 1 to 20 do Write(Data[i]:6); Writeln;
end.
```

## Input validation loop
Compatibility: TP3+ (no units in TP3), TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
program InputCheck;
var
  N, Code: Integer;
  S: string[8];
begin
  repeat
    Write('Enter a number 1-10: ');
    ReadLn(S);
    Val(S, N, Code);
  until (Code = 0) and (N >= 1) and (N <= 10);
  WriteLn('OK: ', N);
end.
```

## Config file read/write (text, TP-safe)
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
{$I-}
program ConfigDemo;
var
  F    : Text;
  Name : string[32];
  Val  : Integer;
  Err  : Integer;
begin
  Assign(F, 'config.ini');
  Rewrite(F);
  Err := IOResult; if Err <> 0 then Halt(Err);
  WriteLn(F, 'name=demo');
  WriteLn(F, 'value=42');
  Close(F);
  Err := IOResult; if Err <> 0 then Halt(Err);

  Reset(F);
  Err := IOResult; if Err <> 0 then Halt(Err);
  while not Eof(F) do
  begin
    ReadLn(F, Name);
    Err := IOResult; if Err <> 0 then Halt(Err);
    WriteLn('Line: ', Name);
  end;
  Close(F);
  Err := IOResult; if Err <> 0 then Halt(Err);
end.
```
- `{$I-}` keeps TP7/FPC error handling aligned: failures leave an error code in
  `IOResult` instead of raising an exception (which FPC would do with `SysUtils`).

## Exec TP-safe (DOS)
Compatibility: TP5+, TP7, FPC TP (TP3: no units; requires inline DOS calls)
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
program RunDir;
uses Dos;
begin
  SwapVectors;
  Exec(GetEnv('COMSPEC'), '/C DIR');
  SwapVectors;
  if DosError <> 0 then
    WriteLn('Exec failed: ', DosError);
end.
```
- Assumes `COMSPEC` is set (DOS); prefer short 8.3 paths. FPC on non-DOS hosts
  can swap in `GetEnv('SHELL')` and a compatible command, but keep this pattern
  TP7-focused.

## TP7 Overlay skeleton (template)
Compatibility: TP7-only (FPC: split units; TP3 has no overlays/units)
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
{$F+}
{$O+}
program OvrTemplate;
uses Overlay, OvrUnit1, OvrUnit2;  { these units must be compiled with $O+ $F+ }
{$O OvrUnit1}
{$O OvrUnit2}
begin
  OvrInit('OVRTEMPL.OVR');
  if OvrResult <> ovrOk then Halt(OvrResult);
  Write1;  { routines from overlaid units }
  Write2;
end.
```
- This is a skeleton only: you must supply `OvrUnit1/2` (compiled with `/$o+ /$f+`)
  and link with TP7 to produce `OVRTEMPL.EXE/OVRTEMPL.OVR`. FPC does not support
  TP7 overlays; for FPC, split large units instead.
- Build flow (LLM-friendly, no shipped assets): keep 8.3 filenames, compile overlay
  units with `tpc /$o+ /$f+ ovrunit1.pas` (repeat per unit), then compile the
  program containing `{$O OvrUnit1}` directives to emit `.EXE + .OVR`. Put the
  resulting `.OVR` beside the `.EXE` before running. If you want to embed BGI
  drivers/fonts alongside overlays, convert `.BGI/.CHR` with `BINOBJ` first and
  call `RegisterBGIdriver/RegisterBGIfont` before `InitGraph` in your own code.

## Typed-file CRUD (TP-safe)
Compatibility: TP3+ (no units in TP3), TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
{$I-}
program TypedCrud;
type
  TItem = record
    Id   : Integer;
    Name : string[16];
  end;
var
  F    : file of TItem;
  R    : TItem;
  Err  : Integer;
begin
  Assign(F, 'items.dat');
  Rewrite(F); Err := IOResult; if Err <> 0 then Halt(Err);

  R.Id := 1; R.Name := 'First'; Write(F, R);
  R.Id := 2; R.Name := 'Second'; Write(F, R);
  Err := IOResult; if Err <> 0 then Halt(Err);

  Reset(F); Err := IOResult; if Err <> 0 then Halt(Err);
  Seek(F, 1); Err := IOResult; if Err <> 0 then Halt(Err);
  Read(F, R); Err := IOResult; if Err <> 0 then Halt(Err);
  R.Name := 'Second+';
  Seek(F, 1); Err := IOResult; if Err <> 0 then Halt(Err);
  Write(F, R); Err := IOResult; if Err <> 0 then Halt(Err);

  Seek(F, 0); Err := IOResult; if Err <> 0 then Halt(Err);
  while not Eof(F) do
  begin
    Read(F, R); Err := IOResult; if Err <> 0 then Halt(Err);
    WriteLn(R.Id:2, ' ', R.Name);
  end;
  Close(F); Err := IOResult; if Err <> 0 then Halt(Err);
end.
```

## Untyped block copy (TP-safe)
Compatibility: TP3+ (no units in TP3), TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
{$I-}
program CopyBin;
const BufSize = 8192;
var
  Src, Dst : file;
  Buf      : array[1..BufSize] of Byte;
  ReadCnt, WriteCnt : Word;
  Err      : Integer;
begin
  Assign(Src, 'a.bin'); Reset(Src, 1); Err := IOResult; if Err <> 0 then Halt(Err);
  Assign(Dst, 'b.bin'); Rewrite(Dst, 1); Err := IOResult; if Err <> 0 then Halt(Err);
  repeat
    BlockRead(Src, Buf, BufSize, ReadCnt); Err := IOResult; if Err <> 0 then Halt(Err);
    if ReadCnt > 0 then
    begin
      BlockWrite(Dst, Buf, ReadCnt, WriteCnt); Err := IOResult; if Err <> 0 then Halt(Err);
      if WriteCnt <> ReadCnt then Halt(2);
    end;
  until ReadCnt = 0;
  Close(Src); Err := IOResult; if Err <> 0 then Halt(Err);
  Close(Dst); Err := IOResult; if Err <> 0 then Halt(Err);
end.
```

## ShortString boundary guard
Compatibility: TP3+ (no units in TP3), TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
program ShortStringGuard;
var
  S: string[8];
begin
  S := '1234567890'; { truncates to 8 chars }
  WriteLn('Stored="', S, '" len=', Length(S)); { outputs 8 }
  { Safe slice before concatenating }
  S := Copy(S, 1, 6); { ensure room for suffix }
  S := S + 'OK';
  WriteLn('After slice+suffix: "', S, '" len=', Length(S));
end.
```

## Notes

## FileMode and file sharing (TP-safe)
Compatibility: TP5+, TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
{$I-}
program FileModeDemo;
uses Dos;
var
  F    : file of Byte;
  B    : Byte;
  Err  : Integer;
begin
  { Set FileMode before Reset }
  FileMode := fmReadOnly;  { or 0 }
  Assign(F, 'data.bin');
  Reset(F);
  Err := IOResult;
  if Err <> 0 then
  begin
    WriteLn('Cannot open file, error: ', Err);
    Halt(1);
  end;
  if not Eof(F) then
  begin
    Read(F, B);
    WriteLn('First byte: ', B);
  end;
  Close(F);
end.
```
- `FileMode` values: `fmReadOnly` (0), `fmWriteOnly` (1), `fmReadWrite` (2).
- Set before `Reset`; text files ignore `FileMode`.
- Share modes (DOS): use `DosError` after file operations for sharing violations.

- All examples include `{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}` at the top.
- Run by saving a snippet to `foo.pas`, then compile with `tpc foo.pas` or `fpc -Mtp -Sh- -So foo.pas`.
- Keep line length near 80 characters and avoid managed types.
