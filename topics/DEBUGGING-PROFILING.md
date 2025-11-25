# Debugging & Profiling (TP7/FPC TP mode)

Tips for tracing, debugging, and profiling across TP7 and FPC TP mode.

## Debugging tools and flags
- TP7: use DOSBox/DOSBox-X with debugger (`dosbox-x -startdebug`) or Turbo
  Debugger if available. Enable CPU/tick display to spot Crt timing issues.
- FPC: compile with `-gl` (line info) and `-gw2`/`-gw3` for DWARF; debug with
  gdb (`gdb a.out` or `gdb.exe program.exe`). Lazarus/fpdebug works with
  `-gw3`/`-gw2` and does not require gdb.

## Logging and timing
- Logging (portable): write to a text log and `Flush` periodically to avoid
  buffered output loss. Example:

```pascal
Compatibility: TP7, FPC TP
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
var Log: Text;
begin
  Assign(Log, 'app.log'); Rewrite(Log);
  WriteLn(Log, 'Starting...'); Flush(Log);
  { ... }
  Close(Log);
end.
```

- Timing Crt/Delay:
  - TP7 `Delay` can trigger RTE 200 on very fast CPUs; in FPC it is safe but
    accuracy depends on the host timer.
  - Use `Dos.GetTime` to measure elapsed time if `Delay` jitter matters.
  - Avoid busy-wait loops; prefer `Delay` and calibrate tick size in both
    compilers.

## Assertions
- FPC supports `Assert`, raising RTE 227/`EAssertionFailed` when compiled with
  `{$C+}`/`{$ASSERTIONS ON}`.
- TP7 has no `Assert`; guard any assertions with `{$IFDEF FPC}` when sharing
  code.

## Lightweight profiling
- Insert timestamped log lines with `GetTime` (Dos unit) to bracket slow
  sections.
- Keep measurements coarse (ms resolution is typical); avoid large allocations
  in measurement code to stay within TP7 constraints.
