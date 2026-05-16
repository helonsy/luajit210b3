# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Nature

This is a **code-study fork of LuaJIT 2.1.0-beta3** (upstream MIT, Mike Pall 2005–2017). Recent commits (`提交阅读代码记录`) add Chinese-language annotations to internals (currently concentrated in `lj_obj.h`, `lj_gc.c/.h`, `lj_meta.c`, `lj_bc.h`, `lj_def.h`, `lj_dispatch.c`). The primary modification over upstream is a Visual Studio 2022 solution (`LuaJITSolution/`) wired up for Windows x64 debugging. Do not treat this as a general-purpose LuaJIT fork — default behavior is to preserve upstream code and add annotations, not refactor.

## Building

LuaJIT is a two-stage build: a host-side bootstrap generates assembly + headers, then the actual library/binary is compiled.

### Windows (MSVC, primary path per `README`)

From an **x64 Native Tools Command Prompt for VS 2022** (the plain command prompt will fail with `:FAIL` because `%INCLUDE%` is undefined):

```bash
cd src
msvcbuild.bat x64          # produces lj_vm.obj, the 5 generated .h, lua51.dll, luajit.exe
msvcbuild.bat x64 gc64     # same, but enables LUAJIT_ENABLE_GC64 and uses vm_x64.dasc
msvcbuild.bat x64 debug    # adds /Zi + /debug
msvcbuild.bat x64 static   # static lib instead of DLL
msvcbuild.bat x64 amalg    # single-TU build via ljamalg.c
```

Note the script's default `DASC=vm_x86.dasc`: on x64 **without** `gc64`, LuaJIT uses 32-bit GCRefs in a 64-bit address space. Only `gc64` switches to `vm_x64.dasc`.

### Windows (Visual Studio solution)

`LuaJITSolution/LuaJITSolution.sln` contains four projects. Per `README` steps 4–6, the solution has a non-obvious dependency on `msvcbuild.bat`:

1. Run `msvcbuild x64` in `src/` first — this generates `lj_vm.obj` and the five headers (`lj_bcdef.h`, `lj_ffdef.h`, `lj_libdef.h`, `lj_recdef.h`, `lj_folddef.h`) that the VS projects **do not regenerate themselves**.
2. Copy `src/lj_vm.obj` into `LuaJITSolution/bin/Debug/`. The `LuaJITExe` project references it directly in *Linker → Input* (`lj_vm.obj;LuaJITLib.lib;...`).
3. All four projects emit to `$(SolutionDir)\bin\Debug\`.

The five generated `.h` files **are committed to git** (see `src/.gitignore` — the relevant lines are commented out). Do not add them to `.gitignore`.

Project roles:
- **MiniLua** — `host/minilua.c`, a single-file minimal Lua 5.1 used to run DynASM.
- **BuildVM** — `host/buildvm*.c`, consumes `vm_*.dasc` + generated `buildvm_arch.h` to emit the VM object, bytecode/FF/lib/recorder/fold headers, and `jit/vmdef.lua`.
- **LuaJITLib** — static library containing every `lj_*.c` and `lib_*.c` (the LuaJIT core + stdlib).
- **LuaJITExe** — thin `luajit.c` executable, links `LuaJITLib.lib` + `lj_vm.obj`.

### POSIX

Root `Makefile` delegates to `src/Makefile`. `make`, `make amalg`, `make clean`. `src/Makefile` detects target architecture by preprocessing `lj_arch.h` — override via `HOST_CC`, `CROSS`, `TARGET_SYS`, `XCFLAGS` (e.g. `XCFLAGS+=-DLUAJIT_ENABLE_GC64`). The root `Makefile` explicitly warns it is NOT useful for Windows.

## High-Level Architecture

LuaJIT is a tracing JIT with a hand-written assembler interpreter. There are three compile-time pipelines a reader needs to hold in mind simultaneously:

### 1. Build-time code generation (host → target)

`vm_<arch>.dasc` is DynASM source — a mix of C and architecture-specific assembly macros. The flow is:

```
dynasm/dynasm.lua + host/minilua  →  host/buildvm_arch.h
                                         ↓
                             host/buildvm*.c  →  buildvm.exe
                                         ↓
         lj_vm.obj (or .S)   +   lj_bcdef.h, lj_ffdef.h, lj_libdef.h,
                                  lj_recdef.h, lj_folddef.h, jit/vmdef.lua
```

`buildvm` has multiple `-m` modes (`peobj`, `elfasm`, `machasm`, `bcdef`, `ffdef`, `libdef`, `recdef`, `vmdef`, `folddef`) — each mode reuses the same DynASM-emitted VM image but scans it (or scans the `lib_*.c` / `lj_opt_fold.c` sources) for differently-marked sections.

### 2. Runtime VM (interpreter)

- **`lj_obj.h`** — the whole type system: `TValue` (NaN-tagged 64-bit union; layout diverges on `LJ_GC64` and `LJ_FR2`), `GCobj`, all GC types (`GCstr`, `GCtab`, `GCfunc`, `GCproto`, `GCupval`, `GCudata`, `GCcdata`, `GCtrace`). Read this file first — the write-barrier invariant spelled out around line 90 applies everywhere.
- **`lj_gc.c/.h`** — incremental mark-and-sweep. Black-object-never-points-to-white invariant is enforced by `lj_gc_barrier*` and must be preserved by all `setgcref*` macro users.
- **`lj_parse.c`** — bytecode compiler (Lua source → `GCproto`). `lj_lex.c` is the lexer.
- **`lj_bc.h`** — bytecode definitions (referenced from the assembler VM).
- **`lj_dispatch.c/.h`** — dispatch table, hotcounts, hooks; the bridge between C and the assembler interpreter.
- **`lj_state.c`, `lj_api.c`, `lj_meta.c`, `lj_tab.c`, `lj_str.c`, `lj_func.c`, `lj_udata.c`** — the usual Lua machinery.
- **`lj_err.c`** — error handling, stack unwinding (uses MSVC SEH / DWARF depending on platform).

### 3. Trace compiler (JIT)

LuaJIT is a **tracing** JIT — it records linear paths of frequently-executed bytecode as they run, then optimizes and compiles them to machine code. Pipeline:

```
lj_record.c     — record bytecodes into IR as they execute
  ↓
lj_ir.c, lj_ffrecord.c, lj_crecord.c  — build IR, specialize fastfuncs + FFI ops
  ↓
lj_opt_mem.c → fold → narrow → dce → loop → sink → split   (order matters)
  ↓
lj_asm.c + lj_asm_<arch>.h                                 — register allocation, codegen
   └─ uses lj_emit_<arch>.h                                — low-level instruction emission
  ↓
lj_trace.c       — manages traces, side exits, linking
lj_snap.c        — snapshots for deoptimizing on side exits
lj_mcode.c       — machine-code allocation (W^X pages)
lj_gdbjit.c      — optional GDB JIT-interface integration
```

`lj_opt_fold.c` is a rule-driven IR folder; rules are parsed by `buildvm -m folddef` into `lj_folddef.h`.

### 4. FFI

`lj_ctype.c`, `lj_cparse.c` (C declaration parser), `lj_cconv.c` (conversions), `lj_ccall.c` (calling conventions per arch), `lj_ccallback.c` (Lua→C callbacks), `lj_clib.c` (dlopen/LoadLibrary), `lj_carith.c` (cdata arithmetic), `lj_cdata.c`. `lib_ffi.c` exposes this to Lua.

### 5. Standard libraries

`lib_base.c`, `lib_math.c`, `lib_bit.c`, `lib_string.c`, `lib_table.c`, `lib_io.c`, `lib_os.c`, `lib_package.c`, `lib_debug.c`, `lib_jit.c`, `lib_ffi.c`. `buildvm -m bcdef/ffdef/libdef/recdef` scans special markers in these files (e.g. `LJLIB_CF`, `LJLIB_REC`, `LJLIB_ASM`) to generate dispatch tables and IR recorder hooks — modifying a library function signature typically requires a rebuild of all four generated headers.

`src/jit/*.lua` are **runtime** helpers loaded by the `jit.*` module: disassemblers (`dis_<arch>.lua`), trace dumper (`dump.lua`), profiler (`p.lua`, `v.lua`), bytecode saver (`bcsave.lua`), plus the generated `vmdef.lua`.

## Key Compile-Time Switches

- `LJ_GC64` — 64-bit GC pointers. Affects `TValue` layout, `GCRef`/`MRef` macros, nearly every core structure. Without it, x64 uses 32-bit GCRefs.
- `LJ_FR2` — two-slot frame layout (enabled by `LJ_GC64`). Affects `TValue.ftsz` vs. `fr` branch and every place frames are walked.
- `LJ_HASJIT`, `LJ_HASFFI` — disable with `-DLUAJIT_DISABLE_JIT` / `-DLUAJIT_DISABLE_FFI`.
- `LJ_DUALNUM` — use int32 + double instead of double-only numbers (only on some archs).
- `LUA_BUILD_AS_DLL` — Windows DLL builds.

Changing any of these requires a **full rebuild** (`make clean && make`, or nuke obj files in MSVC) because they change the generated VM and headers, not just C compile output.

## Modifying Generated Headers

The five `lj_*def.h` files and `host/buildvm_arch.h` are generated but committed. If a change to `lib_*.c` or `lj_opt_fold.c` rules is needed, regenerate by running `buildvm -m bcdef|ffdef|libdef|recdef|folddef` (or run `msvcbuild.bat` again). Don't edit these files by hand.

## Documentation

`doc/*.html` is the upstream LuaJIT user-facing documentation (install, running, FFI, jit extensions, profiler). For internals, the best starting points are the comments in `lj_obj.h` (GC barrier rules, tag encodings), `lj_gc.c` (incremental GC state machine), `lj_asm.c` top-of-file, and `lj_record.c` top-of-file.
