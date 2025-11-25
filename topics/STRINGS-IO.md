# Strings and I/O

Handling ShortString safely across TP7 and FPC TP mode, plus console and text I/O patterns.
Compatibility: TP3+ (ShortString-only), TP7, FPC TP (`{$H-}`).

## ShortString vs AnsiString
- TP7 supports only ShortString (`string[255]`).
- FPC TP mode keeps `String` as ShortString when `{$H-}`. `{$H+}` switches `String` to AnsiString (FPC-only).
- Prefer explicit `string[<len>]` for buffer-sensitive code; avoid AnsiString in shared units.
- Boundary: ShortString length sits in the first byte; concat/copy truncate at 255. Guard input lengths to avoid silent truncation.

## Basic String Operations
Compatibility: TP3+, TP7, FPC TP (`{$H-}`)
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
program StringOps;
var
  S : string[64];
begin
  S := 'Hello';
  Writeln('Len=', Length(S));
  Delete(S, 3, 1);
  Insert('p', S, 3);
  Writeln(S);
end.
```

## Console I/O (Crt)
- `ReadLn`/`WriteLn` are portable. `Crt` adds `TextColor`, `GotoXY`, etc., but timer routines may behave differently under emulation.
- Avoid hard-coded delay loops; prefer `Delay` and validate on both compilers.
- Keyboard handling with `KeyPressed`/`ReadKey`:

Compatibility: TP5+, TP7, FPC TP (TP3 lacks `Crt` unit)
```pascal
uses Crt;
var ch: Char;
begin
  TextColor(LightGreen);
  Write('Press keys, ESC to exit: ');
  repeat
    if KeyPressed then
    begin
      ch := ReadKey;
      Write('[', Ord(ch):3, ']');
    end;
  until ch = #27;
end.
```

## Text File I/O Template
Compatibility: TP3+, TP7, FPC TP (`{$H-}`)
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
program TextCopy;
var
  InFile, OutFile : Text;
  Line            : string[255];
begin
  Assign(InFile, 'input.txt');
  Reset(InFile);
  Assign(OutFile, 'output.txt');
  Rewrite(OutFile);
  while not Eof(InFile) do
  begin
    ReadLn(InFile, Line);
    WriteLn(OutFile, Line);
  end;
  Close(InFile);
  Close(OutFile);
end.
```

## Binary and Octal Literals
- **Not standard TP7**: Binary (`%`) and octal (`&`) literals are non-standard extensions in some TP7 implementations; avoid for maximum portability.
- FPC supports binary (`%1010`) and octal (`&17`) literals in all modes.
- **Recommendation**: Use decimal or hexadecimal (`$`) notation for guaranteed TP7/FPC compatibility.

## Cross-References
- Memory considerations for string buffers: `topics/MEMORY-MODEL.md`
- File handling patterns: `topics/FILE-HANDLING.md`
