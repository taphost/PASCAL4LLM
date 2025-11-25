# Assembly and Calling Conventions

Inline assembly guidance for TP7 and FPC with portability warnings.
Compatibility: TP7+ and FPC TP/OBJFPC/Delphi modes; TP3/TP5 lack `asm ... end` (use `Inline` bytes there).

## TP7 Inline ASM
- Intel syntax only; registers must be preserved per Borland rules.
- Avoid touching segment registers unless necessary; document any changes.
- CPU detection helper, useful with `{$G+}`:

Compatibility: TP7+, FPC (Intel syntax)
```pascal
function Is286Able: Boolean; assembler;
asm
  PUSHF
  POP     BX
  AND     BX,0FFFH
  PUSH    BX
  POPF
  PUSHF
  POP     BX
  AND     BX,0F000H
  CMP     BX,0F000H
  MOV     AX,0
  JZ      @@1
  MOV     AX,1
@@1:
end;
```

## FPC Inline ASM
- Intel syntax available across targets; AT&T syntax on select Unix targets.
- Calling conventions depend on target ABI; ensure callee-saved registers are restored.
- Wrap ASM-heavy code with conditional defines when targeting non-DOS platforms.

## Safe Cross-Compatible Patterns
- Keep ASM in small, well-documented routines.
- Pass parameters via registers/stack as expected by TP7; avoid FPC-only calling convention directives in shared code.
- For recursive math routines on the 8087, store intermediate results in local
  variables to avoid 8087 FPU stack overflow.

## Additional Examples

### Passing Parameters (Pascal → ASM)
Compatibility: TP7+, FPC (Intel syntax)
```pascal
{$IFDEF FPC}{$ASMMODE INTEL}{$ENDIF}
function FastMul(A, B: Word): LongInt; assembler;
asm
  MOV   AX, A
  MUL   B        { DX:AX = A * B }
  { Result already in DX:AX for LongInt return }
end;
```

### Modifying var Parameters
Compatibility: TP7+, FPC (Intel syntax)
```pascal
{$IFDEF FPC}{$ASMMODE INTEL}{$ENDIF}
procedure IncVar(var X: Integer); assembler;
asm
  LES   DI, X    { ES:DI = @X }
  INC   WORD PTR ES:[DI]
end;
```

### String Length (ShortString)
Compatibility: TP7+, FPC (Intel syntax)
```pascal
{$IFDEF FPC}{$ASMMODE INTEL}{$ENDIF}
function StrLen(const S: string): Byte; assembler;
asm
  LES   DI, S
  MOV   AL, ES:[DI]  { First byte = length }
end;
```

## Calling Conventions

When interfacing with external code or using FPC directives, understand the calling convention:

| Convention | Parameter Order | Stack Cleanup | Use Case |
|------------|----------------|---------------|----------|
| `pascal`   | Left-to-right  | Callee        | TP7 default; compact code |
| `register` | Registers first| Callee        | FPC default; fastest |
| `cdecl`    | Right-to-left  | Caller        | C libraries; varargs support |
| `stdcall`  | Right-to-left  | Callee        | Win32 API |

- TP7 always uses `pascal` convention.
- FPC allows `{$CALLING convention}` or per-routine `cdecl`/`stdcall`/`register` modifiers.
- When writing ASM routines, match the expected convention or declare it explicitly.

## When to Avoid Assembly

Assembly should be used sparingly in Pascal projects:

**❌ Avoid ASM when:**
- Portability matters (TP7 ↔ FPC ↔ different platforms)
- Code maintainability is critical
- Optimizing without profiling (premature optimization)
- Pascal RTL already provides the functionality

**✅ Use ASM for:**
- Measured hot loops (profile first!)
- Direct hardware access (ports, video memory)
- DOS/BIOS interrupt calls (though see alternatives below)
- Interfacing with external libraries requiring specific calling conventions

## Alternatives to Direct ASM

For DOS/BIOS interrupts, prefer the `Dos` unit's `Intr` and `MsDos` procedures:

Compatibility: TP5+, TP7, FPC TP
```pascal
{$IFDEF FPC}{$MODE TP}{$ENDIF}
uses Dos;
var
  Regs: Registers;
  S: string[80];
begin
  { INT 21h, AH=09h: Print string ($ terminated) }
  S := 'Hello from DOS!$';
  Regs.AH := $09;
  Regs.DS := Seg(S[1]);
  Regs.DX := Ofs(S[1]);
  Intr($21, Regs);
end.
```

This approach:
- Keeps code readable and maintainable
- Works on both TP7 and FPC (DOS targets)
- Avoids inline ASM complexity
- Easier to debug

## FPC Assembly Mode Directive

FPC defaults to AT&T syntax on some targets; force Intel syntax (TP7-compatible) with:

```pascal
{$IFDEF FPC}
  {$ASMMODE INTEL}  { Use Intel syntax like TP7 }
{$ENDIF}
```

Place this at the top of units containing inline assembly to ensure consistent syntax across compilers.

## Notes
- Register preservation (TP7/DOS 16-bit, `pascal` conv.): caller-saves AX,CX,DX; callee must preserve BP,SI,DI,DS,ES. For 32/64-bit FPC targets, consult target ABI (e.g., SysV/Win) and save non-volatile registers.
- Sample wrapper: keep ASM in a small routine and pair with a pure Pascal entry point that validates params and isolates platform-specific code.
- Warning: TP7 assumptions (segment registers, far/near pointers) do not hold on 32/64-bit FPC; avoid relying on DS/ES layout and always restore caller expectations.
- Profile before optimizing: modern Pascal compilers (especially FPC with `-O3`) often produce assembly as fast as hand-written code for most tasks.
