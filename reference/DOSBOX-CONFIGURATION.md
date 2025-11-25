# DOSBox Configuration Guide

Quick reference for running TP7 code under DOSBox emulation.

## Recommended DOSBox Settings

### dosbox.conf Key Settings
```ini
[cpu]
core=auto
cputype=auto
cycles=max 85%   # Prevent Delay RTE200; adjust if needed
```

- **cycles**: Start with `max 85%` or fixed value like `cycles=10000`
- Lower cycles prevent `Crt.Delay` overflow (RTE 200)
- Higher cycles speed up compilation but may trigger timing bugs

### For TP7 Programs with Delay
```ini
[cpu]
cycles=10000    # Conservative value for TP7 CRT timing
```

### For Fast Compilation
```ini
[cpu]
cycles=max      # Maximum speed for compiling
```

## Common Issues

### RTE 200 (Division by Zero in Crt.Delay)
**Symptom**: Program crashes when calling `Delay(ms)` from Crt unit

**Solutions**:
1. **Reduce DOSBox cycles**: Set `cycles=10000` or `cycles=max 50%`
2. **Patch CRT.TPU**: Apply Borland's TPPATCH or use TINST to recalibrate
3. **Use patched CRT**: Place patched `CRT.TPU` in project directory
4. **FPC alternative**: FPC's Delay is safe on modern systems

### Slow Execution
**Symptom**: Program runs slowly

**Solution**: Increase cycles (`cycles=max` or `cycles=50000`)

### Graphics Issues (BGI)
**Symptom**: InitGraph fails or graphics corrupted  

**Solutions**:
1. Ensure `.BGI` drivers in correct path or set `BGI` environment variable
2. Use `output=surface` for better compatibility
3. Check `machine=svga_s3` for VESA modes

## dosbox.conf Graphics Section
```ini
[sdl]
output=surface     # Better TP7/BGI compatibility than OpenGL

[render]
aspect=true        # Maintain 4:3 aspect ratio

[cpu]
core=normal        # More accurate for timing-sensitive code
cycles=10000
```

## File Access
- **Long filenames**: DOSBox supports them but TP7 requires 8.3 format
- **Paths**: Use backslashes `\` or forward slashes `/`
- **Case sensitivity**: DOSBox is case-insensitive like DOS

## Mounting Directories
```bash
# Mount a directory as C: drive
mount c /path/to/turbo/pascal
c:
cd TP
```

## Compilation Tips
```bash
# Fast compilation with high cycles
dosbox -c "mount c ." -c "c:" -c "cycles max" -c "tpc myproj.pas"

# Run with safe timing
dosbox -c "mount c ." -c "c:" -c "cycles 10000" -c "myproj.exe"
```

## Testing Checklist
1. ✅ Test with `cycles=10000` first
2. ✅ Verify `Delay` calls don't crash (RTE 200)
3. ✅ Check keystroke timing (`ReadKey` responsiveness)
4. ✅ Validate BGI graphics init
5. ✅ Test file I/O with both short and long operations

## Performance Tuning

### Optimize for Speed
```ini
[cpu]
core=dynamic
cycles=max
```

### Optimize for Accuracy
```ini
[cpu]
core=normal
cycles=10000
cputype=486_slow
```

## Alternative: Real DOS
For authentic TP7 behavior, consider:
- **FreeDOS**: Modern DOS-compatible OS
- **MS-DOS Player**: Lightweight DOS emulator for Windows/Linux
- **86Box/PCem**: Full PC hardware emulation

## References
- DOSBox Wiki: https://www.dosbox.com/wiki/
- TP7 RTE 200 fix: Apply TPPATCH or throttle cycles
- BGI drivers: Keep in executable directory or set `BGI` env var

## Quick Fix Commands
```bash
# Create dosbox.conf if missing
dosbox -printconf

# Edit dosbox.conf
nano ~/.dosbox/dosbox-*.conf   # Linux
notepad %USERPROFILE%\AppData\Local\DOSBox\dosbox-*.conf  # Windows
```

Compatibility: DOSBox 0.74+, tested with TP7 and most TP5 programs.
