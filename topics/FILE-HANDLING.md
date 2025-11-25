# File Handling

Cross-compatible patterns for text, typed, and untyped files on TP7 and FPC.
Compatibility: TP3+ (no units in TP3; adjust uses accordingly), TP7, FPC TP.

## Text Files (Sequential)
- Use `Assign` → `Reset`/`Rewrite` → I/O → `Close`.
- Check `IOResult` when `{$I-}` is enabled; TP7 and FPC share semantics.
- Example with error checking and line buffer:

Compatibility: TP3+, TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
program CopyTextSafe;
{$I-}
var
  InFile, OutFile : Text;
  Line            : string[255];
begin
  Assign(InFile, 'input.txt'); Reset(InFile);
  if IOResult <> 0 then Halt(1);
  Assign(OutFile, 'output.txt'); Rewrite(OutFile);
  if IOResult <> 0 then Halt(1);
  while not Eof(InFile) do
  begin
    ReadLn(InFile, Line);
    if IOResult <> 0 then Halt(1);
    WriteLn(OutFile, Line);
  end;
  Close(InFile); Close(OutFile);
end.
```

## Typed Files (Records)
- Declare fixed-size records; avoid managed types inside records.
- Use `BlockRead`/`BlockWrite` for performance and to control record counts.
- Minimal CRUD schema:

Compatibility: TP3+, TP7, FPC TP
```pascal
type
  TItem = record
    Id   : Integer;
    Name : string[32];
  end;
var f: file of TItem; rec: TItem;
begin
  Assign(f, 'items.dat'); Rewrite(f);
  rec.Id := 1; rec.Name := 'First'; Write(f, rec);
  Reset(f); Read(f, rec); { retrieve }
  Close(f);
end;
```

## Untyped Files (Binary Blocks)
- Open with `Reset(F, BlockSize)`; choose block sizes that align with TP7 memory limits.
- Random access via `Seek` and `FilePos`; watch 16-bit offsets in TP7.
- Blockwise binary copy (pattern derived from TP7 examples, simplified):

Compatibility: TP3+, TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$ENDIF}
program CopyBin;
const BufSize = 8192;
var
  Src, Dst : file;
  Buf      : array[1..BufSize] of Byte;
  ReadCnt, WriteCnt : Word;
begin
  Assign(Src, 'a.bin'); Reset(Src, 1);
  Assign(Dst, 'b.bin'); Rewrite(Dst, 1);
  repeat
    BlockRead(Src, Buf, BufSize, ReadCnt);
    if ReadCnt > 0 then BlockWrite(Dst, Buf, ReadCnt, WriteCnt);
    if WriteCnt <> ReadCnt then Halt(2);
  until ReadCnt = 0;
  Close(Src); Close(Dst);
end.
```

## Random Access Techniques
- Store record counts at file head; use `Seek` with record size arithmetic.
- Maintain index tables in memory carefully to stay under 64KB in TP7.

## Record CRUD (typed file example)
Compatibility: TP3+, TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
program CrudDemo;
type
  TItem = record
    Id   : Integer;
    Name : string[32];
  end;
var
  F   : file of TItem;
  R   : TItem;
  Pos : LongInt;
begin
  Assign(F, 'items.dat'); Rewrite(F);
  R.Id := 1; R.Name := 'Foo'; Write(F, R);
  R.Id := 2; R.Name := 'Bar'; Write(F, R);
  { update record 2 }
  Reset(F); Pos := 1; Seek(F, Pos); Read(F, R);
  R.Name := 'Bar2'; Seek(F, Pos); Write(F, R);
  { read back }
  Seek(F, 0); while not Eof(F) do begin Read(F, R); Writeln(R.Id, ':', R.Name); end;
  Close(F);
end.
```

## Platform-Specific Behaviors
- DOS timestamp rounding and attribute handling can differ from modern OS targets; normalize in tests.
- Long filenames are FPC-only; keep 8.3 for TP7 compatibility.

## Buffering and EOL notes
- `{$I+}` (default in FPC) raises runtime errors on I/O failure; `{$I-}` requires `IOResult` checks (TP7-style). Pick one style per program to avoid mixed error handling.
- Text file buffering and `EOL` translation differ across OSes; normalize line endings to CRLF if targeting TP7 DOS. In FPC, `TextRec(LineEnding)` follows host defaults; call `SetTextBuf` for custom buffering.

## FileMode Constants
Use the `FileMode` global variable (defined in the `System` unit) to control file access modes:

| Constant | Value | Usage |
|----------|-------|-------|
| `fmReadOnly` (or `0`) | 0 | Open file for reading only |
| `fmWriteOnly` (or `1`) | 1 | Open file for writing only |  
| `fmReadWrite` (or `2`) | 2 | Open file for both reading and writing |

**Note**: Set `FileMode` before calling `Reset` on untyped or typed files. Text files ignore `FileMode`.

Compatibility: TP5+, TP7, FPC (DOS and compatible targets)
