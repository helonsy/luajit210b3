/*
** Garbage collector.
** Copyright (C) 2005-2017 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_GC_H
#define _LJ_GC_H

#include "lj_obj.h"

/* Garbage collector states. Order matters. */
// 垃圾收集器状态
// 1. GCSpause: 暂停状态
// 2. GCSpropagate: 传播阶段
// 3. GCSatomic: 原子阶段
// 4. GCSsweepstring: 字符串清理阶段
// 5. GCSsweep: 清理阶段
// 6. GCSfinalize: 终结阶段
enum {
  GCSpause, GCSpropagate, GCSatomic, GCSsweepstring, GCSsweep, GCSfinalize
};

/* Bitmasks for marked field of GCobj. */
// 采用了三色标记算法（白色，灰色，黑色）
#define LJ_GC_WHITE0	0x01
#define LJ_GC_WHITE1	0x02
#define LJ_GC_BLACK	0x04
// 终结器的概念：
//  - 终结器是对象被回收前调用的清理函数， 用于释放对象占用的外部资源，在 Lua 中通过 __gc 元方法实现
#define LJ_GC_FINALIZED	0x08 // 用于标记对象是否已经执行过终结器（finalizer）的标志位

// 弱引用的基本概念：
//   - 弱引用不会阻止对象被垃圾回收，对象只被弱引用引用时，可以被回收，在 Lua 中通过表的 __mode 元方法实现
//   - 弱引用 key 的示例
        // local cache = setmetatable({}, {__mode = "k"})
        // local key = {id = 1}
        // cache[key] = "value"
        // key = nil  -- key 可以被回收
//   - 弱引用 value 的示例
        // local pool = setmetatable({}, {__mode = "v"})
        // local obj = {data = "test"}
        // pool[1] = obj
        // obj = nil  -- obj 可以被回收
//   - local t = setmetatable({}, {__mode = "kv"}) --弱引用 key 和 value
#define LJ_GC_WEAKKEY	0x08 
#define LJ_GC_WEAKVAL	0x10 

// C 数据类型（cdata）的概念：
//   - 通过 FFI（Foreign Function Interface）创建，用于表示 C 语言的数据类型， 可以包含需要清理的外部资源
#define LJ_GC_CDATA_FIN	0x10 // 用于标记 C 数据类型（cdata）是否已经执行过终结器的标志位
// 固定对象的概念：
//   - 固定对象是永远不会被回收的对象，通常用于全局对象或长期存在的对象，避免被垃圾回收器错误回收
#define LJ_GC_FIXED	0x20 // 用于标记对象是否被固定（fixed）的标志位。被固定的对象不会被垃圾回收器回收。
// 超级固定对象的概念：
//   - 比普通固定对象更高级别的保护，在垃圾回收器关闭时仍然保持，通常用于最基础的系统对象
#define LJ_GC_SFIXED	0x40 // 用于标记对象是否被超级固定（super fixed）的标志位。超级固定的对象在垃圾回收器关闭时也不会被回收。

#define LJ_GC_WHITES	(LJ_GC_WHITE0 | LJ_GC_WHITE1)
#define LJ_GC_COLORS	(LJ_GC_WHITES | LJ_GC_BLACK)
#define LJ_GC_WEAK	(LJ_GC_WEAKKEY | LJ_GC_WEAKVAL)

/* Macros to test and set GCobj colors. */
#define iswhite(x)	((x)->gch.marked & LJ_GC_WHITES)
#define isblack(x)	((x)->gch.marked & LJ_GC_BLACK)
#define isgray(x)	(!((x)->gch.marked & (LJ_GC_BLACK|LJ_GC_WHITES)))
#define tviswhite(x)	(tvisgcv(x) && iswhite(gcV(x))) // 这个宏用于检查一个值是否是白色的垃圾回收对象
#define otherwhite(g)	(g->gc.currentwhite ^ LJ_GC_WHITES) // 这个宏用于获取当前白色集合的补集（另一个白色集合），在 LuaJIT 的垃圾回收中
                                                          // ，使用了两个白色集合来支持增量式垃圾回收，当前白色集合：g->gc.currentwhite
                                                          // ，另一个白色集合：otherwhite(g)，通过异或运算（^）切换

// 用于检查一个对象是否可以被回收（是否"死亡"）
// 假设：
   // 当前白色集合是 WHITE0 (0x01)
   // 另一个白色集合是 WHITE1 (0x02)
   // 假设对象标记(v)->gch.marked是 WHITE0
   // 则：
   // WHITE0 & WHITE1 & (WHITE0 | WHITE1) = 0
   // 对象不是死亡的
#define isdead(g, v)	((v)->gch.marked & otherwhite(g) & LJ_GC_WHITES)

// 假设 currentwhite 是 WHITE0 (0x01)
// 则 curwhite(g) 返回 WHITE0
// 如果 currentwhite 是 WHITE1 (0x02)
// 则 curwhite(g) 返回 WHITE1
#define curwhite(g)	((g)->gc.currentwhite & LJ_GC_WHITES)

// 用于将对象标记为当前白色集合
   // 假设 currentwhite 是 WHITE0 (0x01)
   // 则 newwhite(g, x) 将对象的标记位设置为 WHITE0
   // 如果 currentwhite 是 WHITE1 (0x02)
   // 则 newwhite(g, x) 将对象的标记位设置为 WHITE1
#define newwhite(g, x)	(obj2gco(x)->gch.marked = (uint8_t)curwhite(g))

// 用于将对象标记为当前白色集合，同时保留其他标记位。
   // 假设：
   // LJ_GC_COLORS = 0x07 (0b111)
   // ~LJ_GC_COLORS = 0xF8 (0b11111000)
   // 对象标记是 0x0F (0b1111)
   // 则：
   // 0x0F & 0xF8 = 0x08 (0b1000)
   // 清除了颜色标记位（最后3位是颜色标记位），保留了其他标记位（0x0F=0b1111的最前面的一个1）
#define makewhite(g, x) \
  ((x)->gch.marked = ((x)->gch.marked & (uint8_t)~LJ_GC_COLORS) | curwhite(g))

// 假设：
   // LJ_GC_WHITES = 0x03 (0b11)
   // 对象标记是 0x01 (WHITE0)
   // 则：
   // 0x01 ^ 0x03 = 0x02 (WHITE1)
   // 白色标记从 WHITE0 翻转到 WHITE1
#define flipwhite(x)	((x)->gch.marked ^= LJ_GC_WHITES)

// 假设：
  // LJ_GC_BLACK = 0x04 (0b100)
  // ~LJ_GC_BLACK = 0xFB (0b11111011)
  // 对象标记是 0x07 (黑色 + 其他标记)
  // 则：
  // 0x07 & 0xFB = 0x03 (清除黑色标记位)
  // 对象从黑色变为灰色， 灰色是：#define isgray(x)	(!((x)->gch.marked & (LJ_GC_BLACK|LJ_GC_WHITES))) = !(0b00000111) = 0b11111000
#define black2gray(x)	((x)->gch.marked &= (uint8_t)~LJ_GC_BLACK)
#define fixstring(s)	((s)->marked |= LJ_GC_FIXED)
#define markfinalized(x)	((x)->gch.marked |= LJ_GC_FINALIZED)

/* Collector. */
LJ_FUNC size_t lj_gc_separateudata(global_State *g, int all);
LJ_FUNC void lj_gc_finalize_udata(lua_State *L);
#if LJ_HASFFI // FFI = Foreign Function Interface（外部函数接口），允许 Lua 代码直接调用 C 函数，允许直接访问 C 数据结构
LJ_FUNC void lj_gc_finalize_cdata(lua_State *L);
#else
#define lj_gc_finalize_cdata(L)		UNUSED(L)
#endif
LJ_FUNC void lj_gc_freeall(global_State *g);
LJ_FUNCA int LJ_FASTCALL lj_gc_step(lua_State *L);
LJ_FUNCA void LJ_FASTCALL lj_gc_step_fixtop(lua_State *L);
#if LJ_HASJIT
LJ_FUNC int LJ_FASTCALL lj_gc_step_jit(global_State *g, MSize steps);
#endif
LJ_FUNC void lj_gc_fullgc(lua_State *L);

/* GC check: drive collector forward if the GC threshold has been reached. */
#define lj_gc_check(L) \
  { if (LJ_UNLIKELY(G(L)->gc.total >= G(L)->gc.threshold)) \
      lj_gc_step(L); }
#define lj_gc_check_fixtop(L) \
  { if (LJ_UNLIKELY(G(L)->gc.total >= G(L)->gc.threshold)) \
      lj_gc_step_fixtop(L); }

/* Write barriers. */
LJ_FUNC void lj_gc_barrierf(global_State *g, GCobj *o, GCobj *v);
LJ_FUNCA void LJ_FASTCALL lj_gc_barrieruv(global_State *g, TValue *tv);
LJ_FUNC void lj_gc_closeuv(global_State *g, GCupval *uv);
#if LJ_HASJIT
LJ_FUNC void lj_gc_barriertrace(global_State *g, uint32_t traceno);
#endif

/* Move the GC propagation frontier back for tables (make it gray again). */
static LJ_AINLINE void lj_gc_barrierback(global_State *g, GCtab *t)
{
  GCobj *o = obj2gco(t);
  lua_assert(isblack(o) && !isdead(g, o));
  lua_assert(g->gc.state != GCSfinalize && g->gc.state != GCSpause);
  black2gray(o);
  setgcrefr(t->gclist, g->gc.grayagain);
  setgcref(g->gc.grayagain, o);
}

/* Barrier for stores to table objects. TValue and GCobj variant. */
#define lj_gc_anybarriert(L, t)  \
  { if (LJ_UNLIKELY(isblack(obj2gco(t)))) lj_gc_barrierback(G(L), (t)); }
#define lj_gc_barriert(L, t, tv) \
  { if (tviswhite(tv) && isblack(obj2gco(t))) \
      lj_gc_barrierback(G(L), (t)); }
#define lj_gc_objbarriert(L, t, o)  \
  { if (iswhite(obj2gco(o)) && isblack(obj2gco(t))) \
      lj_gc_barrierback(G(L), (t)); }

/* Barrier for stores to any other object. TValue and GCobj variant. */
#define lj_gc_barrier(L, p, tv) \
  { if (tviswhite(tv) && isblack(obj2gco(p))) \
      lj_gc_barrierf(G(L), obj2gco(p), gcV(tv)); }
#define lj_gc_objbarrier(L, p, o) \
  { if (iswhite(obj2gco(o)) && isblack(obj2gco(p))) \
      lj_gc_barrierf(G(L), obj2gco(p), obj2gco(o)); }

/* Allocator. */
LJ_FUNC void *lj_mem_realloc(lua_State *L, void *p, GCSize osz, GCSize nsz);
LJ_FUNC void * LJ_FASTCALL lj_mem_newgco(lua_State *L, GCSize size);
LJ_FUNC void *lj_mem_grow(lua_State *L, void *p,
			  MSize *szp, MSize lim, MSize esz);

#define lj_mem_new(L, s)	lj_mem_realloc(L, NULL, 0, (s))

static LJ_AINLINE void lj_mem_free(global_State *g, void *p, size_t osize)
{
  g->gc.total -= (GCSize)osize;
  g->allocf(g->allocd, p, osize, 0);
}

#define lj_mem_newvec(L, n, t)	((t *)lj_mem_new(L, (GCSize)((n)*sizeof(t))))
#define lj_mem_reallocvec(L, p, on, n, t) \
  ((p) = (t *)lj_mem_realloc(L, p, (on)*sizeof(t), (GCSize)((n)*sizeof(t))))
#define lj_mem_growvec(L, p, n, m, t) \
  ((p) = (t *)lj_mem_grow(L, (p), &(n), (m), (MSize)sizeof(t)))
#define lj_mem_freevec(g, p, n, t)	lj_mem_free(g, (p), (n)*sizeof(t))

#define lj_mem_newobj(L, t)	((t *)lj_mem_newgco(L, sizeof(t)))
#define lj_mem_newt(L, s, t)	((t *)lj_mem_new(L, (s)))
#define lj_mem_freet(g, p)	lj_mem_free(g, (p), sizeof(*(p)))

#endif
