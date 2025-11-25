# Memory Model

Contrasting TP7 real-mode constraints with FPC's flat memory model, plus strategies to stay portable.
Compatibility: TP3+ (memory limits apply), TP7, FPC TP; overlay sections are TP7-only (FPC has no TP-style overlays).

## TP7: Segment:Offset Basics
- Code/data limited to 64KB per segment; watch static array sizes and unit bloat.
- Near pointers stay within a segment; far/huge pointers traverse segments but are costlier.
- Overlays load code on demand via `{$O filename}` and the `Overlay` unit; requires careful test on DOS.

## FPC: Flat Model
- Linear address space; pointer size depends on target platform (16/32/64-bit).
- No segment juggling; however, when targeting DOS with FPC, 16-bit limits can reappear.

## Shared Strategies
- Prefer `SmallInt`/`LongInt` for explicit sizing instead of `Integer`.
- Split large data structures across units; avoid single arrays approaching 64KB in TP7 builds.
- Allocate on the heap sparingly in TP7 to prevent fragmentation; free buffers when done.
- If code size grows, consider the TP7 overlay system: loads code segments from
  disk into conventional memory on demand.

## Cross-Compiler Patterns
- Guard FPC-only constructs with `{$IFDEF FPC}`.
- Avoid pointers to managed types (AnsiString, dynamic arrays) in shared code.
- For BGI/graphics buffers, size by bytes to stay under TP7 limits.

## Stack and Heap Defaults (TP7)
- `{$M Stack,HeapMin,HeapMax}` controls runtime layout. Defaults (from TP7 manuals) are `{$M 16384,0,655360}`; stack range 1,024–65,520 bytes, heap up to 640KB.
- The Stack grows downward from `SSeg`, heap grows upward from `HeapOrg`; the largest single heap block is 65,519 bytes (segment-limited).
- `{$S+}` inserts stack overflow checks at each routine entry; leave on unless you have measured overhead.
- Overlay buffer (`Overlay` unit) sits between stack and heap; increasing `OvrSetBuf` shrinks available heap accordingly.
- Heap helpers: `MaxAvail`/`MemAvail` to size buffers; `Mark`/`Release` for LIFO batches; `HeapError` hook to intercept allocation failures.

## Heap Manager (TP7)
- Model: heap grows upward from `HeapOrg` to `HeapPtr`; blocks are freed either individually (`Dispose`/`FreeMem`) or via LIFO (`Mark`/`Release`). Single-block cap: 65,519 bytes (segment bound).
- Free list: fragmentation matters; use fixed-size blocks where possible and `Mark`/`Release` around batch allocations to avoid holes.
- Diagnostics: `MemAvail` (total free bytes), `MaxAvail` (largest contiguous block).
- Error hook: assign `HeapError` to a function returning 0 to raise RTE 203 or nonzero to retry/return error code.
- Practical pattern:
  - Call `Mark(SavedHeap)` before a batch; allocate; `Release(SavedHeap)` to free en masse.
  - Query `MaxAvail` before sizing large buffers; reduce or split if near the limit.
  - If using overlays, reserve overlay buffer first (`OvrSetBuf`) so `MaxAvail` reflects real heap headroom.

## Overlay (TP7-only)
Compatibility: TP7-only (FPC lacks TP overlays; TP3/TP5 overlay manager absent)
- Template overlay sequence (illustrative only; no overlay demo units/OVR are shipped in this repo—see `examples/BASIC-PATTERNS.md` section "TP7 Overlay skeleton" around line 157):

```pascal
{$F+,O+}
program OvrDemo;
uses Overlay, Crt, OvrUnit1, OvrUnit2;
{$O OvrUnit1}   { unit overlaid }
{$O OvrUnit2}
begin
  OvrInit('OVRDEMO.OVR');
  if OvrResult <> 0 then Halt(1);
  Write1; { from first overlaid unit }
  Write2;
end.
```
- Checklist: `{$F+}` to force far calls, `{$O+}` to allow overlaying, `{$O unit}`
  directive for each module, call `OvrInit` before using overlaid routines and
  check `OvrResult`. Compile to disk (not in-RAM builds).
- Build flow (TP7 tools): compile units with `{$O+}{$F+}`, in the program list overlays via `{$O UnitName}` after `uses`, run linker to produce `.EXE` + `.OVR` (largest overlay defines default buffer). Optionally enlarge buffer with `OvrSetBuf` at startup. Check `OvrResult` for load errors. See `examples/BASIC-PATTERNS.md` "TP7 Overlay skeleton" section for complete template.
- Return codes (`OvrResult`): `ovrOk=0`, `ovrError=-1` (program has no overlays), `ovrNotFound=-2` (OVR missing), `ovrNoMemory=-3`, `ovrIOError=-4`, `ovrNoEMSDriver=-5`, `ovrNoEMSMemory=-6`.
- Minimal build script (shell/DOS style):
  - `tpc /$o+ /$f+ ovrunit1.pas`
  - `tpc /$o+ /$f+ ovrunit2.pas`
  - `tpc main.pas` (contains `{$O OvrUnit1}`/`{$O OvrUnit2}` after `uses` and calls `OvrInit('MAIN.OVR')`)
  - BINOBJ embedding (drivers/fonts): `BINOBJ CGA.BGI CGA.OBJ` then link OBJ and register before `InitGraph`.
- Batch/Make snippets:
  - DOS batch: 
    ```
    @echo off
    tpc /$o+ /$f+ ovrunit1.pas
    tpc /$o+ /$f+ ovrunit2.pas
    tpc main.pas
    if errorlevel 1 goto :err
    main.exe
    :err
    ```
  - Make (DOS-friendly):
    ```
    TPPC=tpc
    OFLAGS=/$o+ /$f+
    all: main.exe
    ovrunit1.tpu: ovrunit1.pas
    	$(TPPC) $(OFLAGS) ovrunit1.pas
    ovrunit2.tpu: ovrunit2.pas
    	$(TPPC) $(OFLAGS) ovrunit2.pas
    main.exe: main.pas ovrunit1.tpu ovrunit2.tpu
    	$(TPPC) main.pas
    ```
- FPC does not support true overlays; mimic by splitting into smaller units or loading plugins at runtime.

## Stack/Heap sizing tips
- TP7 default stack is ~16KB; raise with `{$M Stack,MinHeap,MaxHeap}` (e.g., `{$M 16384,0,655360}`) for larger recursion. Test under DOS to avoid exceeding conventional memory.
- Free heap buffers promptly in TP7 to avoid fragmentation; prefer small records/arrays and reuse buffers.
- For FPC targets, stack size is OS/ABI-dependent; use linker/OS settings if heavy recursion is needed.

## Memory optimization checklist
- Split large units; keep global data under 64KB in TP7.
- Avoid managed types in shared records; prefer fixed-size buffers.
- For overlays, script the build: compile overlay units separately, link with `BINOBJ`/`OvrMerge`, and ship `.OVR` alongside the executable.
