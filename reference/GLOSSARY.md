# Glossary

Extended glossary for Pascal terms used across the documentation set. See also
`reference/UNITS-REFERENCE.md` for System/SysUtils behaviors.

## Terms
- PSP: Program Segment Prefix in DOS; holds command-line data, file handles, and is at segment address `PrefixSeg`. Relevant only to TP7 real-mode DOS targets.
- Segment/Offset: 16-bit segmented addressing in TP7 DOS (`seg:ofs`); absent in flat-model FPC targets.
- Near/Far/Huge pointers: TP7 pointer sizes; far/huge include segment; huge normalize on arithmetic. Avoid in FPC except DOS targets.
- TP mode: FPC compiler mode emulating TP7 syntax/semantics; enabled via `{$MODE TP}`.
- Short-circuit evaluation: `{$B-}` (default TP-style) short-circuits `and`/`or`; `{$B+}` forces full evaluation.
- Overlay: TP7 mechanism to load code from `.OVR` on demand; see `topics/MEMORY-MODEL.md`.
- ShortString: fixed-length string (1-byte length prefix). Default in TP7; in FPC only with `{$H-}`.
- AnsiString: reference-counted string; default in FPC `{$H+}` modes; not TP7-compatible.
- Managed vs unmanaged types: managed include AnsiString, dynamic arrays, interfaces; unmanaged include ShortString, static arrays, records without managed fields.
- Exceptions: `EInOutError`, `ERangeError`, `EHeapMemoryError`, `EInvalidPointer`,
  `EIntOverflow`, `EAccessViolation`, `EAssertionFailed`, `EConvertError`,
  `EDivByZero` (FPC `SysUtils` classes that correspond to TP7 runtime errors;
  `ExitCode` still matches RTE numbers). See `reference/ERRORS-RUNTIME.md` for complete exception mapping.
- Runtime Error (RTE): numeric exit code used by TP7/FPC to report runtime
  faults (e.g., RTE 2 file not found, RTE 200 division by zero). See
  `reference/ERRORS-RUNTIME.md`.
- System unit: base RTL with `RunError`, `ExitCode`, low-level I/O; see
  `reference/UNITS-REFERENCE.md` (System).
- SysUtils unit: exceptions (`EInOutError`, `ERangeError`, etc.), file/path
  helpers; see `reference/UNITS-REFERENCE.md` (SysUtils).
