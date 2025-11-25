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

## Notes
- Register preservation (TP7/DOS 16-bit, `pascal` conv.): caller-saves AX,CX,DX; callee must preserve BP,SI,DI,DS,ES. For 32/64-bit FPC targets, consult target ABI (e.g., SysV/Win) and save non-volatile registers.
- Sample wrapper: keep ASM in a small routine and pair with a pure Pascal entry point that validates params and isolates platform-specific code.
- Warning: TP7 assumptions (segment registers, far/near pointers) do not hold on 32/64-bit FPC; avoid relying on DS/ES layout and always restore caller expectations.
