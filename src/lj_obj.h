/*
** LuaJIT VM tags, values and objects.
** Copyright (C) 2005-2017 Mike Pall. See Copyright Notice in luajit.h
**
** Portions taken verbatim or adapted from the Lua interpreter.
** Copyright (C) 1994-2008 Lua.org, PUC-Rio. See Copyright Notice in lua.h
*/

#ifndef _LJ_OBJ_H
#define _LJ_OBJ_H

#include "lua.h"
#include "lj_def.h"
#include "lj_arch.h"

/* -- Memory references (32 bit address space) ---------------------------- */

/* Memory and GC object sizes. */
typedef uint32_t MSize;
#if LJ_GC64
typedef uint64_t GCSize;
#else
typedef uint32_t GCSize;
#endif

/* Memory reference */
// 用于处理内存引用
typedef struct MRef {
#if LJ_GC64
  uint64_t ptr64;	/* True 64 bit pointer. */
#else
  uint32_t ptr32;	/* Pseudo 32 bit pointer. */
#endif
} MRef;

#if LJ_GC64
#define mref(r, t)	((t *)(void *)(r).ptr64)

#define setmref(r, p)	((r).ptr64 = (uint64_t)(void *)(p))
#define setmrefr(r, v)	((r).ptr64 = (v).ptr64)
#else
#define mref(r, t)	((t *)(void *)(uintptr_t)(r).ptr32)

#define setmref(r, p)	((r).ptr32 = (uint32_t)(uintptr_t)(void *)(p))
#define setmrefr(r, v)	((r).ptr32 = (v).ptr32)
#endif

/* -- GC object references (32 bit address space) ------------------------- */

/* GCobj reference */
typedef struct GCRef {
#if LJ_GC64
  uint64_t gcptr64;	/* True 64 bit pointer. */
#else
  uint32_t gcptr32;	/* Pseudo 32 bit pointer. */
#endif
} GCRef;

/* Common GC header for all collectable objects. */
// 定义了所有可回收对象的基本结构，类似于继承的基类
#define GCHeader	GCRef nextgc; uint8_t marked; uint8_t gct
/* This occupies 6 bytes, so use the next 2 bytes for non-32 bit fields. */

#if LJ_GC64
#define gcref(r)	((GCobj *)(r).gcptr64)
#define gcrefp(r, t)	((t *)(void *)(r).gcptr64)
#define gcrefu(r)	((r).gcptr64)
#define gcrefeq(r1, r2)	((r1).gcptr64 == (r2).gcptr64)

#define setgcref(r, gc)	((r).gcptr64 = (uint64_t)&(gc)->gch)
#define setgcreft(r, gc, it) \
  (r).gcptr64 = (uint64_t)&(gc)->gch | (((uint64_t)(it)) << 47)
#define setgcrefp(r, p)	((r).gcptr64 = (uint64_t)(p))
#define setgcrefnull(r)	((r).gcptr64 = 0)
#define setgcrefr(r, v)	((r).gcptr64 = (v).gcptr64)
#else
#define gcref(r)	((GCobj *)(uintptr_t)(r).gcptr32)
#define gcrefp(r, t)	((t *)(void *)(uintptr_t)(r).gcptr32)
#define gcrefu(r)	((r).gcptr32)
#define gcrefeq(r1, r2)	((r1).gcptr32 == (r2).gcptr32)

#define setgcref(r, gc)	((r).gcptr32 = (uint32_t)(uintptr_t)&(gc)->gch)
#define setgcrefp(r, p)	((r).gcptr32 = (uint32_t)(uintptr_t)(p))
#define setgcrefnull(r)	((r).gcptr32 = 0)
#define setgcrefr(r, v)	((r).gcptr32 = (v).gcptr32)
#endif

#define gcnext(gc)	(gcref((gc)->gch.nextgc))

/* IMPORTANT NOTE:
**
** All uses of the setgcref* macros MUST be accompanied with a write barrier.
**
** This is to ensure the integrity of the incremental GC. The invariant
** to preserve is that a black object never points to a white object.
** I.e. never store a white object into a field of a black object.
**
** It's ok to LEAVE OUT the write barrier ONLY in the following cases:
** - The source is not a GC object (NULL).
** - The target is a GC root. I.e. everything in global_State.
** - The target is a lua_State field (threads are never black).
** - The target is a stack slot, see setgcV et al.
** - The target is an open upvalue, i.e. pointing to a stack slot.
** - The target is a newly created object (i.e. marked white). But make
**   sure nothing invokes the GC inbetween.
** - The target and the source are the same object (self-reference).
** - The target already contains the object (e.g. moving elements around).
**
** The most common case is a store to a stack slot. All other cases where
** a barrier has been omitted are annotated with a NOBARRIER comment.
**
** The same logic applies for stores to table slots (array part or hash
** part). ALL uses of lj_tab_set* require a barrier for the stored value
** *and* the stored key, based on the above rules. In practice this means
** a barrier is needed if *either* of the key or value are a GC object.
**
** It's ok to LEAVE OUT the write barrier in the following special cases:
** - The stored value is nil. The key doesn't matter because it's either
**   not resurrected or lj_tab_newkey() will take care of the key barrier.
** - The key doesn't matter if the *previously* stored value is guaranteed
**   to be non-nil (because the key is kept alive in the table).
** - The key doesn't matter if it's guaranteed not to be part of the table,
**   since lj_tab_newkey() takes care of the key barrier. This applies
**   trivially to new tables, but watch out for resurrected keys. Storing
**   a nil value leaves the key in the table!
**
** In case of doubt use lj_gc_anybarriert() as it's rather cheap. It's used
** by the interpreter for all table stores.
**
** Note: In contrast to Lua's GC, LuaJIT's GC does *not* specially mark
** dead keys in tables. The reference is left in, but it's guaranteed to
** be never dereferenced as long as the value is nil. It's ok if the key is
** freed or if any object subsequently gets the same address.
**
** Not destroying dead keys helps to keep key hash slots stable. This avoids
** specialization back-off for HREFK when a value flips between nil and
** non-nil and the GC gets in the way. It also allows safely hoisting
** HREF/HREFK across GC steps. Dead keys are only removed if a table is
** resized (i.e. by NEWREF) and xREF must not be CSEd across a resize.
**
** The trade-off is that a write barrier for tables must take the key into
** account, too. Implicitly resurrecting the key by storing a non-nil value
** may invalidate the incremental GC invariant.
*/

/* -- Common type definitions --------------------------------------------- */

/* Types for handling bytecodes. Need this here, details in lj_bc.h. */
typedef uint32_t BCIns;  /* Bytecode instruction. */
typedef uint32_t BCPos;  /* Bytecode position. */
typedef uint32_t BCReg;  /* Bytecode register. */
typedef int32_t BCLine;  /* Bytecode line number. */

/* Internal assembler functions. Never call these directly from C. */
typedef void (*ASMFunction)(void);

/* Resizable string buffer. Need this here, details in lj_buf.h. */
typedef struct SBuf {
  MRef p;		/* String buffer pointer. */
  MRef e;		/* String buffer end pointer. */
  MRef b;		/* String buffer base. */
  MRef L;		/* lua_State, used for buffer resizing. */
} SBuf;

/* -- Tags and values ----------------------------------------------------- */

/* Frame link. */
typedef union {
  int32_t ftsz;		/* Frame type and size of previous frame. */
  MRef pcr;		/* Or PC for Lua frames. */
} FrameLink;

/* Tagged value. */
typedef LJ_ALIGN(8) union TValue {
  uint64_t u64;		/* 64 bit pattern overlaps number. */
  lua_Number n;	// Lua数字类型	/* Number object overlaps split tag/value object. */
#if LJ_GC64
  GCRef gcr;		// 带标签的GC对象引用/* GCobj reference with tag. */
  int64_t it64;
  struct {
    LJ_ENDIAN_LOHI(
      int32_t i;	/* Integer value. */
    , uint32_t it;	// 内部对象标签（内部对象的类型）	/* Internal object tag. Must overlap MSW of number. */
    )
  };
#else
  struct {
    LJ_ENDIAN_LOHI( // 用于确保代码在不同字节序的处理器上都能正确工作
      union {
	GCRef gcr;	// GC 对象引用 /* GCobj reference (if any). */
	int32_t i;	/* Integer value. */
      };
    , uint32_t it;	// 内部对象标签（内部对象的类型）	/* Internal object tag. Must overlap MSW of number. */
    )
  };
#endif

// LJ_FR2 是 LuaJIT 中一个重要的编译时配置选项，用于控制函数帧（Function Frame）的布局方式
// 函数帧的概念：
//   - 函数帧是存储函数执行状态的数据结构
//   - 包含局部变量，参数，返回地址等信息
// LJ_FR2 模式：使用更紧凑的64位标识
// 非 LJ_FR2 模式：使用分离的结构体表示
#if LJ_FR2
  int64_t ftsz; // 前一帧的类型和大小，或程序计数器		/* Frame type and size of previous frame, or PC. */
#else
  struct {
    LJ_ENDIAN_LOHI(
      GCRef func;	/* Function for next frame (or dummy L). */
    , FrameLink tp;	/* Link to previous frame. */
    )
  } fr;
#endif
  struct {
    LJ_ENDIAN_LOHI(
      uint32_t lo;	// 到下一帧的函数 /* Lower 32 bits of number. */
    , uint32_t hi;	// 到前一帧的链接 /* Upper 32 bits of number. */
    )
  } u32;
} TValue;

typedef const TValue cTValue;

#define tvref(r)	(mref(r, TValue))

/* More external and GCobj tags for internal objects. */
#define LAST_TT		LUA_TTHREAD
#define LUA_TPROTO	(LAST_TT+1)
#define LUA_TCDATA	(LAST_TT+2)

/* Internal object tags.
**
** Format for 32 bit GC references (!LJ_GC64):
**
** Internal tags overlap the MSW of a number object (must be a double).
** Interpreted as a double these are special NaNs. The FPU only generates
** one type of NaN (0xfff8_0000_0000_0000). So MSWs > 0xfff80000 are available
** for use as internal tags. Small negative numbers are used to shorten the
** encoding of type comparisons (reg/mem against sign-ext. 8 bit immediate).
**
**                  ---MSW---.---LSW---
** primitive types |  itype  |         |
** lightuserdata   |  itype  |  void * |  (32 bit platforms)
** lightuserdata   |ffff|    void *    |  (64 bit platforms, 47 bit pointers)
** GC objects      |  itype  |  GCRef  |
** int (LJ_DUALNUM)|  itype  |   int   |
** number           -------double------
**
** Format for 64 bit GC references (LJ_GC64):
**
** The upper 13 bits must be 1 (0xfff8...) for a special NaN. The next
** 4 bits hold the internal tag. The lowest 47 bits either hold a pointer,
** a zero-extended 32 bit integer or all bits set to 1 for primitive types.
**
**                     ------MSW------.------LSW------
** primitive types    |1..1|itype|1..................1|
** GC objects/lightud |1..1|itype|-------GCRef--------|
** int (LJ_DUALNUM)   |1..1|itype|0..0|-----int-------|
** number              ------------double-------------
**
** ORDER LJ_T
** Primitive types nil/false/true must be first, lightuserdata next.
** GC objects are at the end, table/userdata must be lowest.
** Also check lj_ir.h for similar ordering constraints.
*/
#define LJ_TNIL			(~0u)
#define LJ_TFALSE		(~1u)
#define LJ_TTRUE		(~2u)
#define LJ_TLIGHTUD		(~3u)
#define LJ_TSTR			(~4u)
#define LJ_TUPVAL		(~5u)
#define LJ_TTHREAD		(~6u)
#define LJ_TPROTO		(~7u)
#define LJ_TFUNC		(~8u)
#define LJ_TTRACE		(~9u)
#define LJ_TCDATA		(~10u)
#define LJ_TTAB			(~11u)
#define LJ_TUDATA		(~12u)
/* This is just the canonical number type used in some places. */
#define LJ_TNUMX		(~13u)

/* Integers have itype == LJ_TISNUM doubles have itype < LJ_TISNUM */
#if LJ_64 && !LJ_GC64
#define LJ_TISNUM		0xfffeffffu
#else
#define LJ_TISNUM		LJ_TNUMX
#endif
#define LJ_TISTRUECOND		LJ_TFALSE
#define LJ_TISPRI		LJ_TTRUE
#define LJ_TISGCV		(LJ_TSTR+1)
#define LJ_TISTABUD		LJ_TTAB

#if LJ_GC64
#define LJ_GCVMASK		(((uint64_t)1 << 47) - 1)
#endif

/* -- String object ------------------------------------------------------- */

/* String object header. String payload follows. */
//
typedef struct GCstr {
  GCHeader;
  uint8_t reserved;	/* Used by lexer for fast lookup of reserved words. */
  uint8_t unused;
  MSize hash;		/* Hash of string. */
  MSize len;		/* Size of string. */
} GCstr; // 字符串对象

#define strref(r)	(&gcref((r))->str)
#define strdata(s)	((const char *)((s)+1))
#define strdatawr(s)	((char *)((s)+1))
#define strVdata(o)	strdata(strV(o))
#define sizestring(s)	(sizeof(struct GCstr)+(s)->len+1)

/* -- Userdata object ----------------------------------------------------- */

/* Userdata object. Payload follows. */
typedef struct GCudata {
  GCHeader;
  uint8_t udtype;	/* Userdata type. */
  uint8_t unused2;
  GCRef env;		/* Should be at same offset in GCfunc. */
  MSize len;		/* Size of payload. */
  GCRef metatable;	/* Must be at same offset in GCtab. */
  uint32_t align1;	/* To force 8 byte alignment of the payload. */
} GCudata; // 用户数据对象

/* Userdata types. */
enum {
  UDTYPE_USERDATA,	/* Regular userdata. */
  UDTYPE_IO_FILE,	/* I/O library FILE. */
  UDTYPE_FFI_CLIB,	/* FFI C library namespace. */
  UDTYPE__MAX
};

#define uddata(u)	((void *)((u)+1))
#define sizeudata(u)	(sizeof(struct GCudata)+(u)->len)

/* -- C data object ------------------------------------------------------- */

/* C data object. Payload follows. */
typedef struct GCcdata {
  GCHeader;
  uint16_t ctypeid;	/* C type ID. */
} GCcdata; // C数据对象

/* Prepended to variable-sized or realigned C data objects. */
typedef struct GCcdataVar {
  uint16_t offset;	/* Offset to allocated memory (relative to GCcdata). */
  uint16_t extra;	/* Extra space allocated (incl. GCcdata + GCcdatav). */
  MSize len;		/* Size of payload. */
} GCcdataVar;

#define cdataptr(cd)	((void *)((cd)+1))
#define cdataisv(cd)	((cd)->marked & 0x80)
#define cdatav(cd)	((GCcdataVar *)((char *)(cd) - sizeof(GCcdataVar)))
#define cdatavlen(cd)	check_exp(cdataisv(cd), cdatav(cd)->len)
#define sizecdatav(cd)	(cdatavlen(cd) + cdatav(cd)->extra)
#define memcdatav(cd)	((void *)((char *)(cd) - cdatav(cd)->offset))

/* -- Prototype object ---------------------------------------------------- */

#define SCALE_NUM_GCO	((int32_t)sizeof(lua_Number)/sizeof(GCRef))
#define round_nkgc(n)	(((n) + SCALE_NUM_GCO-1) & ~(SCALE_NUM_GCO-1))

typedef struct GCproto {
  GCHeader;
  uint8_t numparams;	/* Number of parameters. */
  uint8_t framesize;	/* Fixed frame size. */
  MSize sizebc;		/* Number of bytecode instructions. */
#if LJ_GC64
  uint32_t unused_gc64;
#endif
  GCRef gclist;
  MRef k;		/* Split constant array (points to the middle). */
  MRef uv;		/* Upvalue list. local slot|0x8000 or parent uv idx. */
  MSize sizekgc;	/* Number of collectable constants. */
  MSize sizekn;		/* Number of lua_Number constants. */
  MSize sizept;		/* Total size including colocated arrays. */
  uint8_t sizeuv;	/* Number of upvalues. */
  uint8_t flags;	/* Miscellaneous flags (see below). */
  uint16_t trace;	/* Anchor for chain of root traces. */
  /* ------ The following fields are for debugging/tracebacks only ------ */
  GCRef chunkname;	/* Name of the chunk this function was defined in. */
  BCLine firstline;	/* First line of the function definition. */
  BCLine numline;	/* Number of lines for the function definition. */
  MRef lineinfo;	/* Compressed map from bytecode ins. to source line. */
  MRef uvinfo;		/* Upvalue names. */
  MRef varinfo;		/* Names and compressed extents of local variables. */
} GCproto; // 原型对象

/* Flags for prototype. */
#define PROTO_CHILD		0x01	/* Has child prototypes. */
#define PROTO_VARARG		0x02	/* Vararg function. */
#define PROTO_FFI		0x04	/* Uses BC_KCDATA for FFI datatypes. */
#define PROTO_NOJIT		0x08	/* JIT disabled for this function. */
#define PROTO_ILOOP		0x10	/* Patched bytecode with ILOOP etc. */
/* Only used during parsing. */
#define PROTO_HAS_RETURN	0x20	/* Already emitted a return. */
#define PROTO_FIXUP_RETURN	0x40	/* Need to fixup emitted returns. */
/* Top bits used for counting created closures. */
#define PROTO_CLCOUNT		0x20	/* Base of saturating 3 bit counter. */
#define PROTO_CLC_BITS		3
#define PROTO_CLC_POLY		(3*PROTO_CLCOUNT)  /* Polymorphic threshold. */

#define PROTO_UV_LOCAL		0x8000	/* Upvalue for local slot. */
#define PROTO_UV_IMMUTABLE	0x4000	/* Immutable upvalue. */

#define proto_kgc(pt, idx) \
  check_exp((uintptr_t)(intptr_t)(idx) >= (uintptr_t)-(intptr_t)(pt)->sizekgc, \
	    gcref(mref((pt)->k, GCRef)[(idx)]))
#define proto_knumtv(pt, idx) \
  check_exp((uintptr_t)(idx) < (pt)->sizekn, &mref((pt)->k, TValue)[(idx)])
#define proto_bc(pt)		((BCIns *)((char *)(pt) + sizeof(GCproto)))
#define proto_bcpos(pt, pc)	((BCPos)((pc) - proto_bc(pt)))
#define proto_uv(pt)		(mref((pt)->uv, uint16_t))

#define proto_chunkname(pt)	(strref((pt)->chunkname))
#define proto_chunknamestr(pt)	(strdata(proto_chunkname((pt))))
#define proto_lineinfo(pt)	(mref((pt)->lineinfo, const void))
#define proto_uvinfo(pt)	(mref((pt)->uvinfo, const uint8_t))
#define proto_varinfo(pt)	(mref((pt)->varinfo, const uint8_t))

/* -- Upvalue object ------------------------------------------------------ */

typedef struct GCupval {
  GCHeader;
  uint8_t closed;	/* Set if closed (i.e. uv->v == &uv->u.value). */
  uint8_t immutable;	/* Immutable value. */
  union {
    TValue tv;		/* If closed: the value itself. */
    struct {		/* If open: double linked list, anchored at thread. */
      GCRef prev;
      GCRef next;
    };
  };
  MRef v;		/* Points to stack slot (open) or above (closed). */
  uint32_t dhash;	/* Disambiguation hash: dh1 != dh2 => cannot alias. */
} GCupval;

#define uvprev(uv_)	(&gcref((uv_)->prev)->uv)
#define uvnext(uv_)	(&gcref((uv_)->next)->uv)
#define uvval(uv_)	(mref((uv_)->v, TValue))

/* -- Function object (closures) ------------------------------------------ */

/* Common header for functions. env should be at same offset in GCudata. */
#define GCfuncHeader \
  GCHeader; uint8_t ffid; uint8_t nupvalues; \
  GCRef env; GCRef gclist; MRef pc

typedef struct GCfuncC {
  GCfuncHeader;
  lua_CFunction f;	/* C function to be called. */
  TValue upvalue[1];	/* Array of upvalues (TValue). */
} GCfuncC;

typedef struct GCfuncL {
  GCfuncHeader;
  GCRef uvptr[1];	/* Array of _pointers_ to upvalue objects (GCupval). */
} GCfuncL;

// 函数对象
typedef union GCfunc {
  GCfuncC c;
  GCfuncL l;
} GCfunc;

#define FF_LUA		0
#define FF_C		1
#define isluafunc(fn)	((fn)->c.ffid == FF_LUA)
#define iscfunc(fn)	((fn)->c.ffid == FF_C)
#define isffunc(fn)	((fn)->c.ffid > FF_C)
#define funcproto(fn) \
  check_exp(isluafunc(fn), (GCproto *)(mref((fn)->l.pc, char)-sizeof(GCproto)))
#define sizeCfunc(n)	(sizeof(GCfuncC)-sizeof(TValue)+sizeof(TValue)*(n))
#define sizeLfunc(n)	(sizeof(GCfuncL)-sizeof(GCRef)+sizeof(GCRef)*(n))

/* -- Table object -------------------------------------------------------- */

/* Hash node. */
typedef struct Node {
  TValue val;		/* Value object. Must be first field. */
  TValue key;		/* Key object. */
  MRef next;		/* Hash chain. */
#if !LJ_GC64
  MRef freetop;		/* Top of free elements (stored in t->node[0]). */
#endif
} Node;

LJ_STATIC_ASSERT(offsetof(Node, val) == 0);

// 表对象
typedef struct GCtab {
  GCHeader;
  uint8_t nomm;		/* Negative cache for fast metamethods. */
  int8_t colo;		/* Array colocation. */
  MRef array;		/* Array part. */
  GCRef gclist;
  GCRef metatable;	/* Must be at same offset in GCudata. */
  MRef node;		/* Hash part. */
  uint32_t asize;	/* Size of array part (keys [0, asize-1]). */
  uint32_t hmask;	/* Hash part mask (size of hash part - 1). */
#if LJ_GC64
  MRef freetop;		/* Top of free elements. */
#endif
} GCtab; // 表对象

#define sizetabcolo(n)	((n)*sizeof(TValue) + sizeof(GCtab))
#define tabref(r)	(&gcref((r))->tab)
#define noderef(r)	(mref((r), Node))
#define nextnode(n)	(mref((n)->next, Node))
#if LJ_GC64
#define getfreetop(t, n)	(noderef((t)->freetop))
#define setfreetop(t, n, v)	(setmref((t)->freetop, (v)))
#else
#define getfreetop(t, n)	(noderef((n)->freetop))
#define setfreetop(t, n, v)	(setmref((n)->freetop, (v)))
#endif

/* -- State objects ------------------------------------------------------- */

/* VM states. */
enum {
  LJ_VMST_INTERP,	/* Interpreter. */
  LJ_VMST_C,		/* C function. */
  LJ_VMST_GC,		/* Garbage collector. */
  LJ_VMST_EXIT,		/* Trace exit handler. */
  LJ_VMST_RECORD,	/* Trace recorder. */
  LJ_VMST_OPT,		/* Optimizer. */
  LJ_VMST_ASM,		/* Assembler. */
  LJ_VMST__MAX
};

#define setvmstate(g, st)	((g)->vmstate = ~LJ_VMST_##st)

/* Metamethods. ORDER MM */
#ifdef LJ_HASFFI
#define MMDEF_FFI(_) _(new)
#else
#define MMDEF_FFI(_)
#endif

#if LJ_52 || LJ_HASFFI
#define MMDEF_PAIRS(_) _(pairs) _(ipairs)
#else
#define MMDEF_PAIRS(_)
#define MM_pairs	255
#define MM_ipairs	255
#endif

#define MMDEF(_) \
  _(index) _(newindex) _(gc) _(mode) _(eq) _(len) \
  /* Only the above (fast) metamethods are negative cached (max. 8). */ \
  _(lt) _(le) _(concat) _(call) \
  /* The following must be in ORDER ARITH. */ \
  _(add) _(sub) _(mul) _(div) _(mod) _(pow) _(unm) \
  /* The following are used in the standard libraries. */ \
  _(metatable) _(tostring) MMDEF_FFI(_) MMDEF_PAIRS(_)

typedef enum {
#define MMENUM(name)	MM_##name,
MMDEF(MMENUM)
#undef MMENUM
  MM__MAX,
  MM____ = MM__MAX,
  MM_FAST = MM_len
} MMS;

/* GC root IDs. */
typedef enum {
  GCROOT_MMNAME,	/* Metamethod names. */
  GCROOT_MMNAME_LAST = GCROOT_MMNAME + MM__MAX-1,
  GCROOT_BASEMT,	/* Metatables for base types. */
  GCROOT_BASEMT_NUM = GCROOT_BASEMT + ~LJ_TNUMX,
  GCROOT_IO_INPUT,	/* Userdata for default I/O input file. */
  GCROOT_IO_OUTPUT,	/* Userdata for default I/O output file. */
  GCROOT_MAX
} GCRootID;

#define basemt_it(g, it)	((g)->gcroot[GCROOT_BASEMT+~(it)])
#define basemt_obj(g, o)	((g)->gcroot[GCROOT_BASEMT+itypemap(o)])
#define mmname_str(g, mm)	(strref((g)->gcroot[GCROOT_MMNAME+(mm)]))

// GCState 是LuaJIT垃圾回收系统的核心状态管理结构
typedef struct GCState {
  // 内存管理
  GCSize total;	// 当前分配的总内存	/* Memory currently allocated. */
  GCSize threshold; // GC触发阈值	/* Memory threshold. */
  GCSize estimate; // 实际使用内存的估计值	/* Estimate of memory actually in use. */
  GCSize debt; // GC延迟量（GC落后于计划的程度）		/* Debt (how much GC is behind schedule). */

  // GC状态控制
  uint8_t currentwhite;	// 当前白色标记 /* Current white color. */
  uint8_t state; // GC状态	/* GC state. */
  uint8_t nocdatafin; // 是否使用cdata终结器	/* No cdata finalizer called. */
  uint8_t unused2; // 保留字段	/* Unused. */

  // 扫描位置
  MSize sweepstr; // 字符串表扫描位置	/* Sweep position in string table. */
  MRef sweep; // 根列表扫描位置		/* Sweep position in root list. */

  // 对象列表
  GCRef root; // 所有可回收对象的列表	/* List of all collectable objects. */
  GCRef gray;	// 灰色对象列表	/* List of gray objects. */
  GCRef grayagain; // 需要原子遍历的对象列表	/* List of objects for atomic traversal. */
  GCRef weak;	// 弱表列表（需要清理）	/* List of weak tables (to be cleared). */
  GCRef mmudata;	// 需要终结的userdata列表 /* List of userdata (to be finalized). */

  // GC控制参数
  MSize stepmul; // 增量GC步长粒度	/* Incremental GC step granularity. */
  MSize pause;	// GC周期之间的暂停时间	/* Pause between successive GC cycles. */
} GCState;

/* Global state, shared by all threads of a Lua universe. */
// global_State 是LuaJIT中标识全局状态的核心数据结构，它被所有线程共享
typedef struct global_State {
  // 字符串管理：
  // - 使用字符串 intern 池优化内存使用，在LuaJIT中，这个功能通过 global_State 中的strhash，strmask，
  //   strnum，strempty等字段实现。
  //   - 字符串intern的核心思想是：
  //      - 相同的字符串只存储一次
  //      - 所有引用都指向同一个内存位置
  //      - 通过哈希表快速查找已存在的字符串
  // - 通过哈希表快速查找字符串
  GCRef *strhash;	// 字符串哈希表（哈希链锚点）/* String hash table (hash chain anchors). */
  MSize strmask; // 字符串哈希掩码（哈希表大小-1）	/* String hash mask (size of hash table - 1). */
  MSize strnum;	 // 哈希表的字符串数量	/* Number of strings in hash table. */
  GCstr strempty; // 空字符串	/* Empty string. */
  uint8_t stremptyz; // 空字符串的零终止符	/* Zero terminator of empty string. */

  // 内存管理：
  // - 通过 allocf 和 allocd 管理内存分配
  // - 通过gc管理垃圾回收
  lua_Alloc allocf; // 内存分配函数	/* Memory allocator. */
  void *allocd; // 内存分配器数据		/* Memory allocator data. */
  GCState gc;	// 垃圾收集器状态	/* Garbage collector. */

  // 虚拟机管理
  volatile int32_t vmstate; // VM状态或当前JIT代码跟踪号  /* VM state or current JIT code trace number. */
  uint8_t dispatchmode; // 分派模式	/* Dispatch mode. */
  uint8_t vmevmask; // VM事件掩码	/* VM event mask. */

  // 线程管理
  // - 维护主线程引用
  // - 跟踪当前执行线程
  GCRef mainthref; // 主线程的引用	/* Link to main thread. */
  GCRef cur_L; // 当前执行的lua_State		/* Currently executing lua_State. */

  // 钩子（Hook）系统
  uint8_t hookmask; // 钩子掩码	/* Hook mask. */
  int32_t hookcount; // 指令钩子倒计时	/* Instruction hook countdown. */
  int32_t hookcstart; // 指令钩子计数器的起始值	/* Start count for instruction hook counter. */
  lua_Hook hookf; // 钩子函数	/* Hook function. */

  // 错误处理
  lua_CFunction panic; // 作为最后手段调用的错误处理函数 	/* Called as a last resort for errors. */
  lua_CFunction wrapf; // C函数调用的包装器	/* Wrapper for C function calls. */

  // JIT支持
  // - 管理JIT编译状态
  // - 处理字节码生成
  MRef jit_base; // 当前JIT代码L->base或NULL	/* Current JIT code L->base or NULL. */
  BCIns bc_cfunc_int; // 内部C函数调用的字节码	/* Bytecode for internal C function calls. */
  BCIns bc_cfunc_ext; // 外部C函数调用的字节码	/* Bytecode for external C function calls. */

  // 临时缓冲区
  SBuf tmpbuf; // 临时字符串缓冲区		/* Temporary string buffer. */
  TValue tmptv, tmptv2; // 临时TValues	/* Temporary TValues. */

  // 注册表的锚点
  TValue registrytv;	/* Anchor for registry. */

  // 特殊节点
  Node nilnode; // 回退1元素哈希部分（nil键和值）		/* Fallback 1-element hash part (nil key and value). */
  GCupval uvhead; // 所有开放上值的双链表的头	/* Head of double-linked list of all open upvalues. */

  // GC根
  GCRef gcroot[GCROOT_MAX]; // GC根  /* GC roots. */
  
  MRef ctype_state;	/* Pointer to C type state. */
} global_State;

#define mainthread(g)	(&gcref(g->mainthref)->th)
#define niltv(L) \
  check_exp(tvisnil(&G(L)->nilnode.val), &G(L)->nilnode.val)
#define niltvg(g) \
  check_exp(tvisnil(&(g)->nilnode.val), &(g)->nilnode.val)

/* Hook management. Hook event masks are defined in lua.h. */
#define HOOK_EVENTMASK		0x0f
#define HOOK_ACTIVE		0x10
#define HOOK_ACTIVE_SHIFT	4
#define HOOK_VMEVENT		0x20
#define HOOK_GC			0x40
#define HOOK_PROFILE		0x80
#define hook_active(g)		((g)->hookmask & HOOK_ACTIVE)
#define hook_enter(g)		((g)->hookmask |= HOOK_ACTIVE)
#define hook_entergc(g)		((g)->hookmask |= (HOOK_ACTIVE|HOOK_GC))
#define hook_vmevent(g)		((g)->hookmask |= (HOOK_ACTIVE|HOOK_VMEVENT))
#define hook_leave(g)		((g)->hookmask &= ~HOOK_ACTIVE)
#define hook_save(g)		((g)->hookmask & ~HOOK_EVENTMASK)
#define hook_restore(g, h) \
  ((g)->hookmask = ((g)->hookmask & HOOK_EVENTMASK) | (h))

/* Per-thread state object. */
// Lua状态对象
// lua_State 是 LuaJIT 中表示一个Lua线程（协程）的核心数据结构
// 每个 lua_State 代表一个独立的执行线程
// 可以创建多个 lua_State 实现协程
struct lua_State {
  // 内存管理：
  // - GCHeader包括垃圾回收所需信息
  // - gclist 用于垃圾回收链
  // - openupval 管理开放的上值
  GCHeader; // 垃圾回收相关的头部信息
  GCRef gclist; // GC链		/* GC chain. */
  GCRef openupval; // 线程环境（全局变量表）	/* List of open upvalues in the stack. */

  uint8_t dummy_ffid; // 用于虚拟帧的 FF_C 标识	/* Fake FF_C for curr_funcisL() on dummy frames. */
 
  // 栈管理：
  //   - base和top用于管理Lua栈
  //   - stack指向栈的起始位置
  //   - maxstack 定义了栈的最大容量
  //   - stacksize 记录实际分配的栈大小
  TValue *base;	// 当前执行函数的栈基址	/* Base of currently executing function. */
  TValue *top; // 栈顶（第一个空闲槽位）		/* First free slot in the stack. */
  MRef stack;	// 栈的基址	/* Stack base. */
  MRef maxstack; // 栈的最大容量	/* Last free slot in the stack. */
  MSize stacksize; // 实际栈大小（包含额外空间）	/* True stack size (incl. LJ_STACK_EXTRA). */

  // 状态管理：
  // - status记录线程的当前状态
  // - glref 链接到全局状态
  // - env 存储全局变量表
  uint8_t status;	// 线程状态 /* Thread status. */
  MRef glref;	// 指向全局状态的引用	/* Link to global state. */
  GCRef env;	// 线程环境（全局变量表）	/* Thread environment (table of globals). */

  // 函数调用：
  // - base 指向当前执行函数的栈基址
  // - cframe 用于C函数调用
  // - 支持 Lua 和 C 代码之间的交互
  void *cframe;		/* End of C stack frame chain. */
}; // 线程对象

#define G(L)			(mref(L->glref, global_State))
#define registry(L)		(&G(L)->registrytv)

/* Macros to access the currently executing (Lua) function. */
#if LJ_GC64
#define curr_func(L)		(&gcval(L->base-2)->fn)
#elif LJ_FR2
#define curr_func(L)		(&gcref((L->base-2)->gcr)->fn)
#else
#define curr_func(L)		(&gcref((L->base-1)->fr.func)->fn)
#endif
#define curr_funcisL(L)		(isluafunc(curr_func(L)))
#define curr_proto(L)		(funcproto(curr_func(L)))
#define curr_topL(L)		(L->base + curr_proto(L)->framesize)
#define curr_top(L)		(curr_funcisL(L) ? curr_topL(L) : L->top)

/* -- GC object definition and conversions -------------------------------- */

/* GC header for generic access to common fields of GC objects. */
// 这是所有可回收对象的基类
typedef struct GChead {
  GCHeader;
  uint8_t unused1;
  uint8_t unused2;
  GCRef env;
  GCRef gclist;
  GCRef metatable;
} GChead;

/* The env field SHOULD be at the same offset for all GC objects. */
LJ_STATIC_ASSERT(offsetof(GChead, env) == offsetof(GCfuncL, env));
LJ_STATIC_ASSERT(offsetof(GChead, env) == offsetof(GCudata, env));

/* The metatable field MUST be at the same offset for all GC objects. */
LJ_STATIC_ASSERT(offsetof(GChead, metatable) == offsetof(GCtab, metatable));
LJ_STATIC_ASSERT(offsetof(GChead, metatable) == offsetof(GCudata, metatable));

/* The gclist field MUST be at the same offset for all GC objects. */
LJ_STATIC_ASSERT(offsetof(GChead, gclist) == offsetof(lua_State, gclist));
LJ_STATIC_ASSERT(offsetof(GChead, gclist) == offsetof(GCproto, gclist));
LJ_STATIC_ASSERT(offsetof(GChead, gclist) == offsetof(GCfuncL, gclist));
LJ_STATIC_ASSERT(offsetof(GChead, gclist) == offsetof(GCtab, gclist));

typedef union GCobj {
  GChead gch;
  GCstr str;
  GCupval uv;
  lua_State th;
  GCproto pt;
  GCfunc fn;
  GCcdata cd;
  GCtab tab;
  GCudata ud;
} GCobj;

/* Macros to convert a GCobj pointer into a specific value. */
#define gco2str(o)	check_exp((o)->gch.gct == ~LJ_TSTR, &(o)->str)
#define gco2uv(o)	check_exp((o)->gch.gct == ~LJ_TUPVAL, &(o)->uv)
#define gco2th(o)	check_exp((o)->gch.gct == ~LJ_TTHREAD, &(o)->th)
#define gco2pt(o)	check_exp((o)->gch.gct == ~LJ_TPROTO, &(o)->pt)
#define gco2func(o)	check_exp((o)->gch.gct == ~LJ_TFUNC, &(o)->fn)
#define gco2cd(o)	check_exp((o)->gch.gct == ~LJ_TCDATA, &(o)->cd)
#define gco2tab(o)	check_exp((o)->gch.gct == ~LJ_TTAB, &(o)->tab)
#define gco2ud(o)	check_exp((o)->gch.gct == ~LJ_TUDATA, &(o)->ud)

/* Macro to convert any collectable object into a GCobj pointer. */
#define obj2gco(v)	((GCobj *)(v))

/* -- TValue getters/setters ---------------------------------------------- */

#ifdef LUA_USE_ASSERT
#include "lj_gc.h"
#endif

/* Macros to test types. */
#if LJ_GC64
#define itype(o)	((uint32_t)((o)->it64 >> 47))
#define tvisnil(o)	((o)->it64 == -1)
#else
#define itype(o)	((o)->it)
#define tvisnil(o)	(itype(o) == LJ_TNIL)
#endif
#define tvisfalse(o)	(itype(o) == LJ_TFALSE)
#define tvistrue(o)	(itype(o) == LJ_TTRUE)
#define tvisbool(o)	(tvisfalse(o) || tvistrue(o))
#if LJ_64 && !LJ_GC64
#define tvislightud(o)	(((int32_t)itype(o) >> 15) == -2)
#else
#define tvislightud(o)	(itype(o) == LJ_TLIGHTUD)
#endif
#define tvisstr(o)	(itype(o) == LJ_TSTR)
#define tvisfunc(o)	(itype(o) == LJ_TFUNC)
#define tvisthread(o)	(itype(o) == LJ_TTHREAD)
#define tvisproto(o)	(itype(o) == LJ_TPROTO)
#define tviscdata(o)	(itype(o) == LJ_TCDATA)
#define tvistab(o)	(itype(o) == LJ_TTAB)
#define tvisudata(o)	(itype(o) == LJ_TUDATA)
#define tvisnumber(o)	(itype(o) <= LJ_TISNUM)
#define tvisint(o)	(LJ_DUALNUM && itype(o) == LJ_TISNUM)
#define tvisnum(o)	(itype(o) < LJ_TISNUM)

#define tvistruecond(o)	(itype(o) < LJ_TISTRUECOND)
#define tvispri(o)	(itype(o) >= LJ_TISPRI)
#define tvistabud(o)	(itype(o) <= LJ_TISTABUD)  /* && !tvisnum() */
#define tvisgcv(o)	((itype(o) - LJ_TISGCV) > (LJ_TNUMX - LJ_TISGCV))

/* Special macros to test numbers for NaN, +0, -0, +1 and raw equality. */
#define tvisnan(o)	((o)->n != (o)->n)
#if LJ_64
#define tviszero(o)	(((o)->u64 << 1) == 0)
#else
#define tviszero(o)	(((o)->u32.lo | ((o)->u32.hi << 1)) == 0)
#endif
#define tvispzero(o)	((o)->u64 == 0)
#define tvismzero(o)	((o)->u64 == U64x(80000000,00000000))
#define tvispone(o)	((o)->u64 == U64x(3ff00000,00000000))
#define rawnumequal(o1, o2)	((o1)->u64 == (o2)->u64)

/* Macros to convert type ids. */
#if LJ_64 && !LJ_GC64
#define itypemap(o) \
  (tvisnumber(o) ? ~LJ_TNUMX : tvislightud(o) ? ~LJ_TLIGHTUD : ~itype(o))
#else
#define itypemap(o)	(tvisnumber(o) ? ~LJ_TNUMX : ~itype(o))
#endif

/* Macros to get tagged values. */
#if LJ_GC64
#define gcval(o)	((GCobj *)(gcrefu((o)->gcr) & LJ_GCVMASK))
#else
#define gcval(o)	(gcref((o)->gcr))
#endif
#define boolV(o)	check_exp(tvisbool(o), (LJ_TFALSE - itype(o)))
#if LJ_64
#define lightudV(o) \
  check_exp(tvislightud(o), (void *)((o)->u64 & U64x(00007fff,ffffffff)))
#else
#define lightudV(o)	check_exp(tvislightud(o), gcrefp((o)->gcr, void))
#endif
#define gcV(o)		check_exp(tvisgcv(o), gcval(o))
#define strV(o)		check_exp(tvisstr(o), &gcval(o)->str)
#define funcV(o)	check_exp(tvisfunc(o), &gcval(o)->fn)
#define threadV(o)	check_exp(tvisthread(o), &gcval(o)->th)
#define protoV(o)	check_exp(tvisproto(o), &gcval(o)->pt)
#define cdataV(o)	check_exp(tviscdata(o), &gcval(o)->cd)
#define tabV(o)		check_exp(tvistab(o), &gcval(o)->tab)
#define udataV(o)	check_exp(tvisudata(o), &gcval(o)->ud)
#define numV(o)		check_exp(tvisnum(o), (o)->n)
#define intV(o)		check_exp(tvisint(o), (int32_t)(o)->i)

/* Macros to set tagged values. */
#if LJ_GC64
#define setitype(o, i)		((o)->it = ((i) << 15))
#define setnilV(o)		((o)->it64 = -1)
#define setpriV(o, x)		((o)->it64 = (int64_t)~((uint64_t)~(x)<<47))
#define setboolV(o, x)		((o)->it64 = (int64_t)~((uint64_t)((x)+1)<<47))
#else
#define setitype(o, i)		((o)->it = (i))
#define setnilV(o)		((o)->it = LJ_TNIL)
#define setboolV(o, x)		((o)->it = LJ_TFALSE-(uint32_t)(x))
#define setpriV(o, i)		(setitype((o), (i)))
#endif

static LJ_AINLINE void setlightudV(TValue *o, void *p)
{
#if LJ_GC64
  o->u64 = (uint64_t)p | (((uint64_t)LJ_TLIGHTUD) << 47);
#elif LJ_64
  o->u64 = (uint64_t)p | (((uint64_t)0xffff) << 48);
#else
  setgcrefp(o->gcr, p); setitype(o, LJ_TLIGHTUD);
#endif
}

#if LJ_64
#define checklightudptr(L, p) \
  (((uint64_t)(p) >> 47) ? (lj_err_msg(L, LJ_ERR_BADLU), NULL) : (p))
#else
#define checklightudptr(L, p)	(p)
#endif

#if LJ_FR2
#define contptr(f)		((void *)(f))
#define setcont(o, f)		((o)->u64 = (uint64_t)(uintptr_t)contptr(f))
#elif LJ_64
#define contptr(f) \
  ((void *)(uintptr_t)(uint32_t)((intptr_t)(f) - (intptr_t)lj_vm_asm_begin))
#define setcont(o, f) \
  ((o)->u64 = (uint64_t)(void *)(f) - (uint64_t)lj_vm_asm_begin)
#else
#define contptr(f)		((void *)(f))
#define setcont(o, f)		setlightudV((o), contptr(f))
#endif

#define tvchecklive(L, o) \
  UNUSED(L), lua_assert(!tvisgcv(o) || \
  ((~itype(o) == gcval(o)->gch.gct) && !isdead(G(L), gcval(o))))

static LJ_AINLINE void setgcVraw(TValue *o, GCobj *v, uint32_t itype)
{
#if LJ_GC64
  setgcreft(o->gcr, v, itype);
#else
  setgcref(o->gcr, v); setitype(o, itype);
#endif
}

static LJ_AINLINE void setgcV(lua_State *L, TValue *o, GCobj *v, uint32_t it)
{
  setgcVraw(o, v, it); tvchecklive(L, o);
}

#define define_setV(name, type, tag) \
static LJ_AINLINE void name(lua_State *L, TValue *o, type *v) \
{ \
  setgcV(L, o, obj2gco(v), tag); \
}
define_setV(setstrV, GCstr, LJ_TSTR)
define_setV(setthreadV, lua_State, LJ_TTHREAD)
define_setV(setprotoV, GCproto, LJ_TPROTO)
define_setV(setfuncV, GCfunc, LJ_TFUNC)
define_setV(setcdataV, GCcdata, LJ_TCDATA)
define_setV(settabV, GCtab, LJ_TTAB)
define_setV(setudataV, GCudata, LJ_TUDATA)

#define setnumV(o, x)		((o)->n = (x))
#define setnanV(o)		((o)->u64 = U64x(fff80000,00000000))
#define setpinfV(o)		((o)->u64 = U64x(7ff00000,00000000))
#define setminfV(o)		((o)->u64 = U64x(fff00000,00000000))

static LJ_AINLINE void setintV(TValue *o, int32_t i)
{
#if LJ_DUALNUM
  o->i = (uint32_t)i; setitype(o, LJ_TISNUM);
#else
  o->n = (lua_Number)i;
#endif
}

static LJ_AINLINE void setint64V(TValue *o, int64_t i)
{
  if (LJ_DUALNUM && LJ_LIKELY(i == (int64_t)(int32_t)i))
    setintV(o, (int32_t)i);
  else
    setnumV(o, (lua_Number)i);
}

#if LJ_64
#define setintptrV(o, i)	setint64V((o), (i))
#else
#define setintptrV(o, i)	setintV((o), (i))
#endif

/* Copy tagged values. */
static LJ_AINLINE void copyTV(lua_State *L, TValue *o1, const TValue *o2)
{
  *o1 = *o2; tvchecklive(L, o1);
}

/* -- Number to integer conversion ---------------------------------------- */

#if LJ_SOFTFP
LJ_ASMF int32_t lj_vm_tobit(double x);
#endif

static LJ_AINLINE int32_t lj_num2bit(lua_Number n)
{
#if LJ_SOFTFP
  return lj_vm_tobit(n);
#else
  TValue o;
  o.n = n + 6755399441055744.0;  /* 2^52 + 2^51 */
  return (int32_t)o.u32.lo;
#endif
}

#define lj_num2int(n)   ((int32_t)(n))

static LJ_AINLINE uint64_t lj_num2u64(lua_Number n)
{
#ifdef _MSC_VER
  if (n >= 9223372036854775808.0)  /* They think it's a feature. */
    return (uint64_t)(int64_t)(n - 18446744073709551616.0);
  else
#endif
    return (uint64_t)n;
}

static LJ_AINLINE int32_t numberVint(cTValue *o)
{
  if (LJ_LIKELY(tvisint(o)))
    return intV(o);
  else
    return lj_num2int(numV(o));
}

static LJ_AINLINE lua_Number numberVnum(cTValue *o)
{
  if (LJ_UNLIKELY(tvisint(o)))
    return (lua_Number)intV(o);
  else
    return numV(o);
}

/* -- Miscellaneous object handling --------------------------------------- */

/* Names and maps for internal and external object tags. */
LJ_DATA const char *const lj_obj_typename[1+LUA_TCDATA+1];
LJ_DATA const char *const lj_obj_itypename[~LJ_TNUMX+1];

#define lj_typename(o)	(lj_obj_itypename[itypemap(o)])

/* Compare two objects without calling metamethods. */
LJ_FUNC int LJ_FASTCALL lj_obj_equal(cTValue *o1, cTValue *o2);
LJ_FUNC const void * LJ_FASTCALL lj_obj_ptr(cTValue *o);

#endif
