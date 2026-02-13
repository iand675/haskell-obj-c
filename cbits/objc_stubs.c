/*
 * objc_stubs.c -- Provides access to objc_msgSend as a function pointer
 * and ensures framework symbols are retained by the linker.
 *
 * objc_msgSend is a special trampoline in libobjc. On ARM64 its prototype
 * is declared as `void objc_msgSend(void)` and it must be cast to the
 * correct function pointer type matching the target method's signature.
 *
 * We use libffi on the Haskell side to perform the correctly-typed call.
 * This file simply exposes the address of objc_msgSend (and variants)
 * as function pointers that Haskell can obtain via foreign import.
 */

#include <objc/message.h>
#include <objc/runtime.h>
#include <stddef.h>

/*
 * Force the linker to keep the Foundation framework loaded.
 * Without this, -dead_strip_dylibs (which GHC passes on macOS)
 * would strip Foundation since there are no direct symbol references.
 *
 * We reference the OBJC_CLASS symbol for NSObject, which is in Foundation.
 */
extern void *OBJC_CLASS_$_NSObject;
void *hs_objc_force_foundation_link(void) {
    return &OBJC_CLASS_$_NSObject;
}

/* Return the address of objc_msgSend as a generic function pointer. */
void *hs_objc_msgSend_ptr(void) {
    return (void *)objc_msgSend;
}

/* Return the address of objc_msgSendSuper. */
void *hs_objc_msgSendSuper_ptr(void) {
    return (void *)objc_msgSendSuper;
}

/*
 * objc_msgSend_stret is only available on x86_64, not on ARM64.
 * On ARM64, struct returns go through objc_msgSend directly.
 */
#if defined(__x86_64__)
void *hs_objc_msgSend_stret_ptr(void) {
    return (void *)objc_msgSend_stret;
}
#else
void *hs_objc_msgSend_stret_ptr(void) {
    /* On ARM64, struct returns use the normal objc_msgSend. */
    return (void *)objc_msgSend;
}
#endif

/*
 * objc_msgSend_fpret is only relevant on x86 (returns float/double
 * in x87 FP register). On ARM64 it's just objc_msgSend.
 */
#if defined(__x86_64__) || defined(__i386__)
void *hs_objc_msgSend_fpret_ptr(void) {
    return (void *)objc_msgSend_fpret;
}
#else
void *hs_objc_msgSend_fpret_ptr(void) {
    return (void *)objc_msgSend;
}
#endif

/* -----------------------------------------------------------------------
 * Retain / release helpers for Haskell ForeignPtr finalizers.
 *
 * We send retain/release messages via objc_msgSend rather than calling
 * objc_retain/objc_release (which are private API).  Both functions
 * guard against NULL so that nil ForeignPtrs are safe.
 * ----------------------------------------------------------------------- */

static SEL hs_retain_sel  = NULL;
static SEL hs_release_sel = NULL;

static void hs_ensure_ref_sels(void) {
    if (!hs_release_sel) {
        hs_retain_sel  = sel_registerName("retain");
        hs_release_sel = sel_registerName("release");
    }
}

void hs_objc_release(id obj) {
    if (obj) {
        hs_ensure_ref_sels();
        ((void (*)(id, SEL))objc_msgSend)(obj, hs_release_sel);
    }
}

id hs_objc_retain(id obj) {
    if (obj) {
        hs_ensure_ref_sels();
        return ((id (*)(id, SEL))objc_msgSend)(obj, hs_retain_sel);
    }
    return NULL;
}

static SEL hs_autorelease_sel = NULL;

id hs_objc_autorelease(id obj) {
    if (obj) {
        if (!hs_autorelease_sel) {
            hs_autorelease_sel = sel_registerName("autorelease");
        }
        return ((id (*)(id, SEL))objc_msgSend)(obj, hs_autorelease_sel);
    }
    return NULL;
}

/* -----------------------------------------------------------------------
 * Block support
 *
 * Creates Objective-C block objects from plain C function pointers.
 * The block ABI on Apple platforms uses a well-known struct layout.
 * We construct a stack block, then Block_copy it to the heap.
 * ----------------------------------------------------------------------- */

#include <Block.h>

struct hs_block_descriptor {
    unsigned long reserved;
    unsigned long size;
};

struct hs_block_literal {
    void *isa;
    int flags;
    int reserved;
    void *invoke;
    struct hs_block_descriptor *descriptor;
};

static struct hs_block_descriptor hs_block_desc = {
    0, sizeof(struct hs_block_literal)
};

/*
 * Create a heap-allocated ObjC block wrapping the given function pointer.
 * The invoke pointer can be any arity — the caller is responsible for
 * ensuring the signature matches the block type expected by the API.
 * Returns a Block_copy'd pointer that must be freed with hs_release_block.
 */
void *hs_create_block(void *invoke) {
    struct hs_block_literal stackBlock;
    stackBlock.isa = _NSConcreteStackBlock;
    stackBlock.flags = 0;
    stackBlock.reserved = 0;
    stackBlock.invoke = invoke;
    stackBlock.descriptor = &hs_block_desc;
    return Block_copy(&stackBlock);
}

/* Release a block previously created by hs_create_block. */
void hs_release_block(void *block) {
    if (block) {
        Block_release(block);
    }
}
