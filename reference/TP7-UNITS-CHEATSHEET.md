# TP7 Unit Cheat Sheet

Compact, TP7-focused API tables. For FPC/RTL/FCL coverage, use the official `rtl`/`fcl` docs or `fpdoc` output—there is no equivalent condensed table here.

## Crt
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `Delay(ms)` | Millisecond sleep | TP7 RTE200 on very fast CPUs; patch CRT/TINST or throttle DOSBox; timing jitter varies by host |
| `CheckBreak` | Ctrl+Break handling | Set `False` to ignore; restores on exit |
| `KeyPressed/ReadKey` | Non-blocking/blocked key read | `ReadKey` consumes extended keys as two chars |
| `GotoXY/WhereX/WhereY` | Cursor positioning | Respects `Window/WindMin/WindMax` |
| `TextColor/TextBackground` | Text attributes | Use `HighVideo/LowVideo/NormVideo` helpers |
| `ClrScr/ClrEol/DelLine/InsLine` | Screen edits | Work inside current window |
| `Sound/NoSound/Delay` | PC speaker beep | Avoid long tones in emulators |
| `Keep(ExitCode)` | Exit to DOS without cleanup | Leaves screen; TP7-only |
| `Window/WindMin/WindMax` | Define text window | Constrains cursor/clears |
| `HighVideo/LowVideo/NormVideo` | Quick attribute toggles | Affects subsequent text |

## Dos
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `Exec(Path, CmdLine)` | Spawn DOS command | Wrap with `SwapVectors`; check `DosError`; 8.3 paths only under TP7 |
| `GetIntVec/SetIntVec` | Read/patch INT vectors | TP7/DOS only; save/restore vectors yourself |
| `SwapVectors` | Swap INT vectors | Call before/after `Exec` |
| `MsDos/Intr` | Call DOS/INT | Requires register setup; 16-bit |
| `FindFirst/FindNext/DoneFind` | Directory search | 8.3 names only |
| `GetDate/GetTime/SetDate/SetTime` | Date/time | DOS local time only |
| `GetEnv/EnvCount/EnvStr` | Environment vars | `PrefixSeg` -> PSP; cmd tail at offset $80 |
| `GetFTime/SetFTime` | File timestamps | FAT semantics |

## Overlay
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `OvrInit('prog.ovr')` | Initialize overlays | Call before overlaid routines |
| `OvrInitEMS` | Overlay buffer in EMS | Falls back to disk |
| `OvrResult` | Last overlay status | `ovrOk=0`, `ovrNotFound=-2`, `ovrNoMemory=-3`, `ovrIOError=-4`, `ovrNoEMSDriver=-5`, `ovrNoEMSMemory=-6` |
| `OvrSetBuf(Size)` | Resize overlay buffer | Shrinks heap; call early |
| `OvrGetBuf` | Query buffer pointer | Rarely needed |

## Graph (BGI)
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `InitGraph(Drv,Mode,Path)` | Enter graphics | `Path` '' or `BGI` env; `GraphResult` on error |
| `DetectGraph/Detect` | Auto select driver/mode | Override for IBM8514 or invalid detect |
| `GraphResult/GraphErrorMsg` | Error reporting | Check after init/ops |
| `RegisterBGIdriver/RegisterBGIfont` | Embed drivers/fonts | Use BINOBJ `.BGI`/`.CHR` -> `.OBJ`; register before `InitGraph` |
| `SetGraphBufSize` | Adjust Graph heap | Must call before `InitGraph` |
| `SetColor/SetBkColor` | Palette selection | Color constants in Graph unit |
| `Line/Rectangle/Circle/Bar/OutText` | Drawing primitives | Check `GraphResult` on failures |
| `SetTextStyle/SetLineStyle/SetFillStyle` | Styles | See BGI tables in `topics/GRAPHICS-BGI.md` |

## Strings (TP7 extended syntax)
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `Str`/`Val` | Convert numbers <-> strings | Check `Code` result for `Val` |
| `Concat`, `Pos`, `Copy`, `Delete`, `Insert` | Basic ops | ShortString (255 max) |
| `Move`, `FillChar` | Buffer ops | Beware overlaps with `Move` |
| Null-terminated helpers (`PChar`) | Interop | Only when `{$X+}` or Strings unit is used |
| `StrCopy/StrLen/StrComp` (Strings unit) | PChar helpers | Require `{$X+}` |

## Printer
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `AssignPrn(Text)` | Redirect to LPT | TP7 submits via DOS/LPT; minimal buffering—call `Flush` after batches |
| `WriteLn` on assigned Text | Send lines to printer | Check `IOResult` if `{$I-}` |
| `Flush` | Force output | Use after large writes |

## Turbo3 / Graph3 (compat)
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `Turbo3` unit | Legacy helpers | Provided for backward compat; prefer modern units |
| `Graph3` unit | Legacy BGI | Superseded by `Graph`; limited support |

## System (selected)
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `Halt(ExitCode)` | Terminate program | Sets DOS exit code |
| `Addr/Ofs/Seg/Ptr` | Address helpers | Segment:offset pointers; typed if `$T+` |
| `Mem/Port` arrays | Direct memory/IO access | 16-bit addresses only |
| `New/Dispose/GetMem/FreeMem/Mark/Release` | Heap mgmt | See `topics/MEMORY-MODEL.md` for limits |
| `Randomize/Random` | RNG | Seed once with `Randomize` |
| `ParamCount/ParamStr` | Command-line args | Command tail in PSP at $80 |
