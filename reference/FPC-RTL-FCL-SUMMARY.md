# FPC 3.2.2 RTL/FCL Summary (token-optimized)

Compact reference of common FPC RTL/FCL/FCL-res units. Use official docs for full signatures.

## System
- Memory: `GetMem/FreeMem/AllocMem`, `ReAllocMem`, `Move/FillChar` (unsafe overlap).
- Control: `Halt`, `ExitCode`, `ExitProc` chain.
- Random: `Randomize/Random`; `RandSeed` is global.
- Typing: `SizeOf`, `Ptr/Ofs/Seg` (Seg/Ofs relevant on DOS targets), `TypeInfo`.
- Exceptions: `SetExceptionMask/GetExceptionMask` (FPU), `InOutRes` for raw RTE codes (SysUtils converts to exceptions).
- Interaction: `ParamCount/ParamStr`.
- Time: `Sleep`.

## SysUtils
- Errors: `RaiseLastOSError/GetLastOSError`, `ExceptObject`/`ExceptAddr`, `Exception` hierarchy (e.g., `EInOutError`, `EConvertError`).
- File/dir: `FileExists/DirectoryExists/DeleteFile/RenameFile/ForceDirectories/IncludeTrailingPathDelimiter`, `FileAge/FileSize`.
- Strings: `Format/FormatDateTime`, `AnsiLowerCase/AnsiUpperCase`, `Trim/TrimLeft/TrimRight`, `QuotedStr/DequotedStr`.
- Dates: `Now/Date/Time/EncodeDate/DecodeDate/IncDay/IncSecond`; `TryEncode*` variants to avoid exceptions.
- Conversions: `TryStrToInt/TryStrToFloat`, `StrToIntDef/StrToFloatDef`; `IntToStr/FloatToStrF`.
- Utilities: `GetAppConfigDir`, `FindFirst/FindNext/FindClose` (extended info), `GetLocaleFormatSettings`.
- Environment/Time: `GetEnvironmentVariable`, `GetTempPath`, `GetTickCount`.
- Notes: Importing SysUtils switches many RTEs to exceptions; locales affect formatting.

## Classes
- Streams: `TStream` base; `TFileStream`, `TMemoryStream`, `TStringStream`, `TResourceStream`. Use `Read/Write/Seek/CopyFrom/SaveToFile`.
- Lists/ownership: `TList/TObjectList` (`OwnsObjects`), `TInterfaceList`, `TStringList` (sorting, duplicates, `Values/Names`, `DelimitedText`).
- Components: `TPersistent` (Assign), `TReader/TWriter` (streaming).
- Threads: `TThread` (`Execute`, `Start`, `Terminate`, `Synchronize/Queue`, `FreeOnTerminate`).
- Sync: `TCriticalSection`, `TEvent` (in `syncobjs`), `TMultiReadExclusiveWriteSynchronizer` (MRW lock).

## StrUtils
- Matching: `AnsiStartsText/AnsiEndsText`, `AnsiContainsText`, `LeftStr/RightStr/MidStr`.
- Replace: `ReplaceText/ReplaceStr`, `DupeString`.
- Search: `PosEx`, `AnsiIndexText/AnsiIndexStr`.
- Extract: `SplitString`, `IfThen` overloads (boolean/strings).

## Math
- Rounding: `Round/Trunc/Int`, `SimpleRoundTo/RoundTo`.
- Clamp: `EnsureRange`.
- Min/Max: overloads for numeric types.
- Power: `IntPower`, `Power`, `Sqr/Sqrt`, angle funcs (`Sin/Cos/ArcTan2`).
- Float tests: `IsNan/IsInfinite`, `CompareValue`.
- Random: `RandomRange/RandomInt64`.

## CRT (FPC)
- Screen/keys: `ClrScr/ClrEol/GotoXY/WhereX/WhereY`, `TextColor/TextBackground`, `KeyPressed/ReadKey`.
- Timing: `Delay` uses host timers (no TP7 RTE 200), `Sound/NoSound` may be stubbed per platform.
- Notes: Behavior depends on terminal/OS; use `ncrt` on *nix if available for better terminal handling.

## Process / Pipes / CustApp (FCL)
- Quick spawn: `RunCommand(Exe, Args, Output)` blocking; returns bool.
- Configurable: `TProcess` (`Executable`, `Parameters`, `Options` like `poUsePipes/poWaitOnExit/poStderrToOut`), `Execute`, `Terminate`, `ExitStatus`.
- Pipes: `TInputPipeStream/TOutputPipeStream` for stream IO with `TProcess`.
- CLI framework: `TCustApp` (`OnRun`/`DoRun`, `CheckOptions`, `HasOption`, `GetOptionValue`), `Application.Terminate`.

## BaseUnix / Unix (POSIX)
- Files: `fpOpen/fpClose/fpRead/fpWrite`, `fpStat/fpLStat/fpFStat`, `fpAccess`, `fpChmod/fpChown`.
- Processes: `fpFork/fpExecVE/fpWait`*, `fpPipe`, `fpDup/fpDup2`, `fpSelect/poll`.
- Sockets: `fpSocket/fpBind/fpListen/fpAccept`, `fpConnect`, `fpSend/fpRecv`, `fpShutdown`.
- Memory: `fpMMap/fpMunmap` (platform-dependent), `fpGetPageSize`.
- Notes: Use `BaseUnix` types (`TFPStat`, `TSocket`, `TInetSockAddr`). Check `fpgeterrno`/`ESysError`.

## NetDB / Sockets (higher-level)
- DNS: `ResolveName/ResolveIP` (NetDB) -> `PHostEnt`.
- Endianness: `HostToNet`/`NetToHost` for 16/32-bit.
- Errors: `SocketError` after BSD-style calls.

## HTTP / Web (FCL)
- Client: `TFPHTTPClient` (`AddHeader`, `AllowRedirect`, `KeepConnection`, `IOTimeout`); methods `Get/Post/FormPost/DownloadFile`. HTTPS depends on build (OpenSSL/etc).
- Server: `TFPHTTPServer` (`OnRequest`, `Active`, `Port`, `Threaded`); basic HTTP handling.
- CGI/FCGI: units `fpcgi`, `fastcgi` for web gateways.

## JSON (fpjson/jsonparser)
- Parse: `GetJSON(Text)` -> `TJSONData` (cast to `TJSONObject/TJSONArray`).
- Access: `TJSONObject.Strings/Integers/Floats/Booleans`, `Find/IndexOfName`; `TJSONArray.Add/Items[]`.
- Enum: `TJSONEnum` for iteration.
- Generate: `FormatJSON` to pretty-print, `AsJSON`.
- Notes: Manage memory; use try/finally to `Free`.

## Compression / Archiving
- Zip: `TZipper` (`FileName`, `Entries.AddFileEntry`) `ZipAllFiles`; `TUnZipper` (`Examine`, `UnZipAllFiles`).
- Streams: `zstream`, `zlib` for raw deflate/inflate.

## Resources (fcl-res)
- Units: `resource`, `coffreader/coffwriter`, `winpeimagereader`, `versionresource`.
- Use: `TResources` container; `TVersionResource` to set Windows version info; `TBitmapResource` etc. Generated/linked via `fpcres`.

## SQLDB (DB access)
- Connectors: `TSQLConnector` subclasses (`TSQLite3Connection`, `TMySQL55Connection`, `TPQConnection`, etc.) + `TSQLTransaction`.
- Queries: `TSQLQuery` (`SQL.Text`, `Params`, `Open` for SELECT, `ExecSQL` for DML).
- Binding: `TParams` for typed parameters; `TDatasource` + `TDBDataset` for data-aware controls.
- Transactions: `Commit/Rollback` on `TSQLTransaction`; set `Database` and `Transaction` on queries.

## Utility units (selected)
- `IniFiles` — INI read/write (`TIniFile`), beware of flush timing.
- `DateUtils` — date math (`IncMonth`, `WeeksBetween`, `WithinPastDays`).
- `SyncObjs` — sync primitives (`TCriticalSection`, `TEvent`, `TSemaphore`).
- `FileUtil` — helpers like `CopyFile`, `DeleteDirectory`.
- `Masks` — wildcard masks (`MatchesMask`).
- `RegExpr` — regular expressions (non-PCRE).

## Variants / RTTI / Generics
- `Variants` — variant type support; conversions and `VarArray` helpers.
- `TypInfo` — RTTI for published properties (`GetPropInfo`, `GetOrdProp`, etc.).
- `Rtti` — extended RTTI (Delphi-like), type inspection, attributes (if enabled).
- `Contnrs` / `Generics.Collections` — container classes (`TObjectStack`, `TObjectQueue`, generic lists/dicts).

## Code pages / Unicode
- `LConvEncoding`, `cwstring` — code page conversions; `LocaleCharsFromUnicode`/`UnicodeFromLocaleChars`.
- `widestringmanager` — Unicode string manager hooks.
- Note: Default string = AnsiString; `UnicodeString` available; code pages matter for SysUtils IO.

## XML / Regex / Text
- `DOM`, `XMLRead`, `XMLWrite` — DOM parsing/emitting; `ReadXMLFile`, `WriteXMLFile`.
- `SAX` — streaming XML parse.
- `RegExpr` — simple regex engine (`Exec`, `SubExprMatchCount`, `Match`, `Replace`).

## Networking extras
- FTP/SMTP: `ftpsend`, `smtpsend`, `popsend`, `imapsend` (if installed); expose high-level send/receive; often rely on Synapse in some distributions.
- JSON-RPC/REST: build on fphttpclient/server; no single unit, but patterns are in HTTP sections.

## DB / Files
- `DBF`/`TDbf` — DBF file access; set `FilePathFull`, `TableName`, `Open`.
- `BufDataset`/`MemDS` — in-memory datasets; `CreateDataset`, `Append/Post`, `SaveToFile`.
- `DB` core — dataset interfaces (`TField`, `TDataSet`, bookmarks).

## Graphics / Media
- `FPImage` — image buffers; `TFPMemoryImage`, color/resource formats.
- Readers/Writers: `FPReadPNG/JPEG/BMP/GIF/TIFF`, `FPWritePNG/JPEG/BMP/GIF/TIFF`; use `TFPReaderPNG.LoadFromStream` etc.
- `FPWritePSD/FPWritePNM` and other codecs depending on install.
- Notes: hook reader/writer with `ImageHandlers.RegisterImageReader/Writer`.

## Zip/Gzip/Compression (more)
- `GZip`/`GunZip` — gzip stream helpers.
- `BZip2` — if built.
- `ZStream` — deflate/inflate streams (raw zlib).

## Resources (extra)
- `fpcres` tool options: `-o` output, `-of raw|coff|elf|win32`, `--use-defaults`; compile `.rc` to `.res`.

## Platform-specific
- `Windows` — WinAPI headers; `Handle/ HWND/ HDC`; messages, GDI, etc. Use `DynLibs` to load DLLs (`LoadLibrary`, `GetProcedureAddress`, `FreeLibrary`).
- `ctypes` — C type aliases for binding; `cdecl` calling convention, pointer sizes.
- `dl` (Unix) / `DynLibs` — dynamic library loading; `LoadLibrary`, `GetProcedureAddress`, `UnloadLibrary`.
- `Printer` — printer APIs differ by platform; often via `printers` unit in FPC.
- `Dos` (FPC) — subset of TP7 semantics for paths/env; not full DOS interrupts outside dos-target builds.

## FPDoc tooling (quick)
- `fpdoc` options: `--project=xml` or `--package` + `--descr`, `--auto-index`, `--auto-toc`, `--format=html|chm`, `--output=dir`, `--css-file=...`.
- Generate docs for units: `fpdoc --package=rtl --input=path/to/unit.pp --dir=out`.

## Platform/Unicode notes
- Strings: default AnsiString unless `-Sh-`/`$H-` for ShortString; UnicodeString available; be mindful of code pages.
- Paths: UTF-8 on Unix; Windows uses UTF-16 APIs by default via SysUtils.
- Exceptions vs. RTE: with SysUtils, most runtime errors raise exceptions but `ExitCode` still set. Use `{$I-}` + `IOResult` if you need TP-style flow.

## Other RTL/FCL units (not detailed)
- `Math` extras, `Variants`, `WideStrings`, `FGL`, `Generics.Collections`, `RegExpr`, `Mask`, `DB` helpers, `fpdebug`, `paszlib`, `fpimage` codecs beyond the listed, GUI/LCL units. Use official docs for rarely used symbols/constants.
