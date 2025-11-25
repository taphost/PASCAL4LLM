# FPC 3.2.2 Unit Cheat Sheet

Compact RTL/FCL/FCL-res lookup for common units. For full detail, refer to `FPC322DOCS` (`rtl.txt`, `fcl.txt`, `fclres.txt`) or generate `fpdoc` HTML.

## System
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `Halt(ExitCode)` | Terminate program | Sets OS exit code |
| `SetExceptionMask/GetExceptionMask` | FPU mask | Affects float exceptions |
| `GetMem/FreeMem/AllocMem` | Heap management | No 64KB segment limits |
| `Randomize/Random` | RNG | Seed once |
| `Val/Str` | Convert text/numbers | Use error code parameter; does not raise exceptions |
| `Ptr`, `Addr`, `Ofs`, `Seg` | Address helpers | `Seg/Ofs` mostly for DOS targets |

## SysUtils
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `Format`, `FormatDateTime` | Formatting | Locale-sensitive |
| `FileExists/DirectoryExists` | Filesystem checks | Unicode paths on supported platforms |
| `IncludeTrailingPathDelimiter` | Path helpers | Use `PathDelim` const |
| `Now/Date/Time/EncodeDate/DecodeDate` | Date/time | Uses system timezone/locale |
| `RaiseLastOSError`, `GetLastOSError` | OS error mapping | Raises `EOSError` |
| `Exception` hierarchy | Error handling | `SysUtils` switches runtime errors to exceptions |
| `AnsiLowerCase/AnsiUpperCase` | Case ops | Locale-aware |
| `DeleteFile/RenameFile/ForceDirectories` | FS operations | Returns bool; check result |
| `FileAge/FileSize` | File metadata | Use `Find*` in `SysUtils` for extended info |

## Classes
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `TStream` + `TFileStream`, `TStringStream` | Stream I/O | Use `Read/Write/Seek` |
| `TMemoryStream` | In-memory stream | Auto-growing buffer |
| `TStringList` | String collection + sorting | `OwnsObjects` for cleanup |
| `TPersistent` | Base for streaming | Supports `Assign` |
| `TThread` | Threading | Override `Execute`; use `Start/Terminate` |
| `TList/TObjectList` | Dynamic lists | `OwnsObjects` dictates freeing |
| `TReader/TWriter` | Component streaming | Used in LCL/DFM-compatible flows |

## StrUtils
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `AnsiStartsText/AnsiEndsText` | Case-insensitive match | Locale-aware |
| `ReplaceText/ReplaceStr` | String replace | Non-regex |
| `LeftStr/RightStr/MidStr` | Substrings | Bounds-checked |
| `DupeString` | Repeat string | Length growth check |

## Math
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `Round/SimpleRoundTo` | Rounding | Bankers or simple |
| `EnsureRange` | Clamp values | Generic helper |
| `Min/Max` | Comparisons | Overloads |
| `IntPower` | Integer exponent | Watch overflow |
| `RandomRange` | RNG range | Uses `Random` |
| `IsNan/IsInfinite` | Float tests | IEEE-754 |
| `SimpleMath` helpers | Common funcs | Check unit docs for target |

## Process / Pipes / CustApp (FCL)
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `RunCommand` | Spawn and capture output | Blocking; collects stdout |
| `TProcess` | Configurable process | Set `Executable`, `Parameters`, `Options` (`poUsePipes`, etc.) |
| `TInputPipeStream/TOutputPipeStream` | Pipe streams | Pair with `TProcess` |
| `TCustApp` | CLI app base | Override `DoRun`; handles params/help |

## BaseUnix / Unix (POSIX)
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `fpOpen/fpClose/fpRead/fpWrite` | POSIX file ops | Use with `c` mode signatures |
| `fpStat/fpLStat/fpFStat` | File metadata | Structs in `BaseUnix` |
| `fpFork/fpWait` | Process control | Unix targets only |
| `fpSelect` | Multiplexing | Use `fpset`/`fdisset` helpers |
| `fpSocket/fpBind/fpListen/fpAccept` | Sockets (BSD) | Combine with `NetDB` lookups |
| `fpChmod/fpChown` | Permissions | Unix only |
| `fpMMap/fpMunmap` | Memory map | Platform-dependent |

## NetDB / Sockets
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `ResolveName/ResolveIP` (NetDB) | DNS helpers | Returns `PHostEnt` |
| `HostToNet/NetToHost` | Endian swaps | 16/32-bit conversions |
| `fpSocket`, `fpConnect`, `fpSend`, `fpRecv`, `fpClose` | BSD-style socket API | Check return codes; use `SocketError` |
| `SocketError`/`h_errno` equivalents | Error codes | Retrieve after failed calls |

## HTTP / JSON / Compression (FCL)
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `TFPHTTPClient` | Simple HTTP client | Set `AllowRedirect`, `AddHeader`; supports HTTPS if compiled |
| `THTTPServer` (fphttpserver) | Basic HTTP server | Hook `OnRequest` |
| `TZipper/TUnZipper` | Zip create/extract | Add entries, `ZipAllFiles`; extract with `Examine/UnZipAllFiles` |

## DB / SQLDB
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `TSQLConnector`/`TSQLTransaction` | DB connection/transaction | Use matching connector (e.g., `TSQLite3Connection`) |
| `TSQLQuery` | Dataset | Call `Open`/`ExecSQL`; set `SQL.Text` |
| `TDatasource` + `TDBDataset` | Data binding | Depends on connector/backend |
| `TParams` | SQL params | Bind params safely |
| `Commit/Rollback` | Transaction control | Call on `TSQLTransaction` |

## fpjson / jsonparser (FCL)
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `GetJSON` -> `TJSONData` | Parse JSON text | Use try/finally to free |
| `TJSONObject/TJSONArray` | DOM objects | Access via `Strings/Integers/Arrays/Objects` |
| `TJSONEnum` | Iteration | Use `Find`/`IndexOfName` for lookups |

## URIParser / FPC-res / Zipper (FCL/FCL-res)
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `ParseURI/URIParser` | URL parsing | Returns `TURI` record |
| `TResources/TVersionResource` (fcl-res) | Read/write resources | PE/COFF focus |
| `TZipper/TUnZipper` | Zip create/extract | Add entries, set `FileName`, call `ZipAllFiles` |

## CRT (FPC)
| Symbol | Purpose | Notes |
| --- | --- | --- |
| `Delay` | Millisecond sleep | Safe from TP RTE200; host-timer accuracy varies—verify if timing-sensitive |
| `SysUtils` RTEs | Exceptions vs ExitCode | Importing `SysUtils` turns many RTEs into exceptions; `ExitCode` still set |
| `KeyPressed/ReadKey`, `GotoXY/WhereX/WhereY`, `TextColor/TextBackground`, `ClrScr/ClrEol` | Console control | Behavior depends on host terminal |
