{-# LANGUAGE ForeignFunctionInterface #-}

-- | Create Objective-C blocks from Haskell functions.
--
-- Objective-C blocks are callable objects used pervasively as callbacks,
-- completion handlers, and enumeration bodies.  This module lets you wrap
-- a Haskell closure into a heap-allocated block that can be passed to any
-- Objective-C API expecting a block parameter.
--
-- = Supported block signatures
--
-- * @void (^)(void)@ — 'withVoidBlock' / 'newVoidBlock'
-- * @void (^)(id)@ — 'withIdBlock' / 'newIdBlock'
--
-- For other signatures, use 'newBlock' directly with your own
-- @foreign import ccall \"wrapper\"@ declaration.
--
-- = Example
--
-- @
-- -- Fire a timer that prints every second:
-- withVoidBlock (putStrLn \"tick\") $ \\block ->
--   NSTimer.scheduledTimerWithTimeInterval_repeats_block 1.0 True block
-- @
module ObjC.Runtime.Block
  ( -- * Bracket-style (auto-frees the block)
    withVoidBlock
  , withIdBlock

    -- * Manual lifecycle
  , newVoidBlock
  , newIdBlock
  , freeBlock

    -- * Generic (bring your own wrapper)
  , newBlock
  ) where

import Control.Exception (bracket)
import Foreign.Ptr (Ptr, FunPtr, castFunPtrToPtr, castPtr)

import ObjC.Runtime.Types (ObjCObject, RawId(..))

-- ---------------------------------------------------------------------------
-- FFI to C helpers
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "hs_create_block"
  c_hs_create_block :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "hs_release_block"
  c_hs_release_block :: Ptr () -> IO ()

-- ---------------------------------------------------------------------------
-- Wrapper imports for supported block signatures
-- ---------------------------------------------------------------------------

-- void (^)(void) — the invoke function receives only the block pointer
type BlockInvoke0 = Ptr () -> IO ()

foreign import ccall "wrapper"
  wrapBlockInvoke0 :: BlockInvoke0 -> IO (FunPtr BlockInvoke0)

-- void (^)(id) — the invoke function receives block pointer + one id arg
type BlockInvoke1 = Ptr () -> Ptr ObjCObject -> IO ()

foreign import ccall "wrapper"
  wrapBlockInvoke1 :: BlockInvoke1 -> IO (FunPtr BlockInvoke1)

-- ---------------------------------------------------------------------------
-- Generic block creation
-- ---------------------------------------------------------------------------

-- | Create a block from an already-wrapped invoke 'FunPtr'.
--
-- The 'FunPtr' must point to a function whose first argument is
-- @Ptr ()@ (the block pointer itself).  Remaining arguments must
-- match the block's type signature.
--
-- The returned 'RawId' is a heap-allocated block.  Call 'freeBlock'
-- when done (or use one of the bracket-style helpers).
newBlock :: FunPtr a -> IO RawId
newBlock fp = do
  blockPtr <- c_hs_create_block (castFunPtrToPtr fp)
  pure (RawId (castPtr blockPtr))

-- | Release a block created by 'newBlock', 'newVoidBlock', or 'newIdBlock'.
freeBlock :: RawId -> IO ()
freeBlock (RawId ptr) = c_hs_release_block (castPtr ptr)

-- ---------------------------------------------------------------------------
-- void (^)(void)
-- ---------------------------------------------------------------------------

-- | Create a @void (^)(void)@ block from a Haskell 'IO' action.
newVoidBlock :: IO () -> IO RawId
newVoidBlock action = do
  fp <- wrapBlockInvoke0 (\_blockSelf -> action)
  newBlock fp

-- | Bracket-style: create a @void (^)(void)@ block, pass it to the
-- continuation, and free it afterwards.
withVoidBlock :: IO () -> (RawId -> IO a) -> IO a
withVoidBlock action = bracket (newVoidBlock action) freeBlock

-- ---------------------------------------------------------------------------
-- void (^)(id)
-- ---------------------------------------------------------------------------

-- | Create a @void (^)(id)@ block from a Haskell function.
newIdBlock :: (RawId -> IO ()) -> IO RawId
newIdBlock handler = do
  fp <- wrapBlockInvoke1 (\_blockSelf objPtr -> handler (RawId objPtr))
  newBlock fp

-- | Bracket-style: create a @void (^)(id)@ block, pass it to the
-- continuation, and free it afterwards.
withIdBlock :: (RawId -> IO ()) -> (RawId -> IO a) -> IO a
withIdBlock handler = bracket (newIdBlock handler) freeBlock
