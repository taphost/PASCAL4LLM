# Debugging & Profiling (TP7/FPC TP mode)

Tips for tracing, debugging, and profiling across TP7 and FPC TP mode.

## Debug Directives and Compilation

### TP7 Debug Directives
Enable debug info in TP7 with:
- `{$D+}` - Line number tables (required for Turbo Debugger and find-error)
- `{$L+}` - Local symbol info (variables, procedures)
- `{$Y+}` - Browser symbol references

Compile with debug info:
```
tpc /N /GS yourprog.pas
```
- `/N` - Embed standalone debug info in EXE
- `/GS` - Generate map file with segments

### FPC Debug Flags
Compile with:
```
fpc -gl -gw3 yourprog.pas
```
- `-gl` - Line info (basic debugging)
- `-gw2` - DWARF2 debug info (gdb, Lazarus)
- `-gw3` - DWARF3 debug info (recommended)

## Debugging Tools

### TP7: Turbo Debugger (TD)
Launch in DOSBox: `td yourprog.exe`

**Essential Commands:**
- `F7` - Trace into (step into procedures)
- `F8` - Step over (skip procedure calls)
- `F4` - Run to cursor
- `F9` - Run/continue
- `Ctrl+F2` - Reset program
- `Ctrl+F7` - Add watch variable
- `Ctrl+F9` - Evaluate/modify expression
- `F2` - Toggle breakpoint at current line

**Views:**
- `Alt+V W` - Watch window
- `Alt+V B` - Breakpoints
- `Alt+V S` - Call stack
- `Alt+V D` - Memory dump

**Conditional Breakpoints:**
1. `Alt+B A` - Add breakpoint
2. Enter condition: `i > 100`
3. Debugger stops only when condition is true

### FPC: GDB
Launch: `gdb ./yourprog`

**Essential Commands:**
```
(gdb) break main           # Breakpoint at main
(gdb) break 42             # Breakpoint at line 42
(gdb) run                  # Start program
(gdb) next                 # Step over (F8)
(gdb) step                 # Step into (F7)
(gdb) continue             # Continue execution
(gdb) print variable       # Print variable value
(gdb) backtrace            # Show call stack
(gdb) info locals          # Show local variables
(gdb) quit                 # Exit
```

**Conditional Breakpoints:**
```
(gdb) break myproc.pas:42 if i > 100
```

**Watch Variables:**
```
(gdb) watch myvar          # Break when myvar changes
(gdb) display myvar        # Show myvar after each step
```

## Debugging Runtime Errors

### TP7: Find Error Location
When program crashes with RTE (e.g., "Runtime error 201 at 0040:1234"):

1. Compile with debug info: `tpc /N yourprog.pas`
2. Use find-error feature:
   ```
   tpc /F0040:1234 yourprog.pas
   ```
3. TP7 shows the exact source line that caused the error

**Alternative:** Load in TD and run until crash; TD will stop at the error line.

### FPC: Backtrace on Crash
Compile with: `fpc -gl -gw3 yourprog.pas`

On crash, FPC shows backtrace automatically:
```
Runtime error 201 at $00401234
  $00401234  MYPROC,  line 42 of myunit.pas
  $00401500  main,  line 10 of myprog.pas
```

**With gdb:**
```bash
gdb ./yourprog
(gdb) run
# Program crashes
(gdb) backtrace  # Shows full call stack
(gdb) frame 0    # Inspect crash frame
(gdb) list       # Show source around crash
```

## Common Debug Scenarios

### Pointer Errors (RTE 204, 215, 216)
Compatibility: TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$ENDIF}
var P: ^Integer;
begin
  New(P);
  if P = nil then
  begin
    WriteLn('ERROR: Allocation failed');
    Halt(1);
  end;
  P^ := 42;      { Safe: P checked }
  Dispose(P);
  P := nil;      { Prevent double-free }
end.
```

**Debug checklist:**
- Always check `P <> nil` before dereferencing
- Use `{$CHECKPOINTER ON}` in FPC for extra validation
- Watch pointer value in debugger
- Set breakpoint before `Dispose` to verify pointer is valid

### Array Index Errors (RTE 201)
Compatibility: TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$ENDIF}
{$R+}  { Enable range checking during development }
var
  A: array[1..10] of Integer;
  I: Integer;
begin
  I := 11;
  A[I] := 0;  { RTE 201 with $R+ enabled }
end.
```

**Debug checklist:**
- Compile with `{$R+}` during development
- Check array bounds before access: `if (I >= Low(A)) and (I <= High(A)) then`
- Use debugger to inspect index values
- Watch loop counters

### Stack Overflow (RTE 202)
Compatibility: TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$ENDIF}
{$M 32768,0,655360}  { Increase stack to 32KB }
program StackTest;

procedure DeepRecursion(N: Integer);
begin
  if N > 0 then
    DeepRecursion(N - 1);
end;

begin
  DeepRecursion(1000);  { May overflow with default stack }
end.
```

**Debug checklist:**
- Increase stack size with `{$M Stack,HeapMin,HeapMax}`
- Reduce recursion depth or convert to iteration
- Move large local arrays to heap (`New`/`GetMem`)
- Check call stack depth in debugger

### Memory Leaks
Compatibility: TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$ENDIF}
uses Dos;
var
  P: ^Integer;
begin
  WriteLn('Before: ', MemAvail, ' bytes free');
  New(P);
  { ... use P ... }
  Dispose(P);  { Don't forget! }
  WriteLn('After: ', MemAvail, ' bytes free');
end.
```

**Debug checklist:**
- Track `MemAvail` before/after allocations
- Ensure every `New`/`GetMem` has matching `Dispose`/`FreeMem`
- Use FPC's `-gh` (heap trace) to detect leaks
- Set breakpoints at allocation/deallocation points

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

## Profiling

### Timestamp-Based Profiling
Compatibility: TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
uses Dos;

var
  H1, M1, S1, S100_1: Word;
  H2, M2, S2, S100_2: Word;
  ElapsedMs: LongInt;
  I: Integer;

begin
  { Start timing }
  GetTime(H1, M1, S1, S100_1);
  
  { Code to profile }
  for I := 1 to 10000 do
    { Some operation };
  
  { End timing }
  GetTime(H2, M2, S2, S100_2);
  
  { Calculate elapsed time in milliseconds }
  ElapsedMs := (H2 - H1) * 3600000 +
               (M2 - M1) * 60000 +
               (S2 - S1) * 1000 +
               (S100_2 - S100_1) * 10;
  
  WriteLn('Elapsed: ', ElapsedMs, ' ms');
end.
```

**Notes:**
- Resolution: ~10ms (hundredths of a second)
- For sub-millisecond timing, use platform-specific APIs
- Keep measurements coarse; avoid large allocations in measurement code
- Profile multiple iterations to reduce timer granularity effects

### Profiling with Logging
Compatibility: TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$H-}{$ENDIF}
uses Dos;

var
  LogFile: Text;

procedure LogTime(const Msg: string);
var H, M, S, S100: Word;
begin
  GetTime(H, M, S, S100);
  WriteLn(LogFile, H:2, ':', M:2, ':', S:2, '.', S100:2, ' - ', Msg);
  Flush(LogFile);
end;

begin
  Assign(LogFile, 'profile.log');
  Rewrite(LogFile);
  
  LogTime('Start');
  { ... operation 1 ... }
  LogTime('After operation 1');
  { ... operation 2 ... }
  LogTime('After operation 2');
  
  Close(LogFile);
end.
```

**Best practices:**
- Use `Flush` after each log line to prevent buffering issues
- Keep log messages short to minimize I/O overhead
- Disable profiling in production builds with conditional compilation
- Compare TP7 vs FPC timings on same hardware when possible

