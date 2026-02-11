{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLIndirectCommandBuffer@.
--
-- Usage:
--
-- @
-- delegate <- newMTLIndirectCommandBuffer defaultMTLIndirectCommandBufferOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLIndirectCommandBuffer
  ( MTLIndirectCommandBufferOverrides(..)
  , defaultMTLIndirectCommandBufferOverrides
  , newMTLIndirectCommandBuffer
  ) where

import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.C.Types
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (withCString)
import Foreign.LibFFI (retCULong, argPtr)

import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass, class_createInstance)
import ObjC.Runtime.ClassBuilder (objc_allocateClassPair, objc_registerClassPair)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.MsgSend (sendSuperMsg)
import ObjC.Runtime.StableIvar

-- | Overrides record for @\@protocol MTLIndirectCommandBuffer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLIndirectCommandBufferOverrides = MTLIndirectCommandBufferOverrides
  { _indirectRenderCommandAtIndex :: !(Maybe (Int -> IO RawId))
  , _indirectComputeCommandAtIndex :: !(Maybe (Int -> IO RawId))
  , _size :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLIndirectCommandBufferOverrides :: MTLIndirectCommandBufferOverrides
defaultMTLIndirectCommandBufferOverrides = MTLIndirectCommandBufferOverrides
  { _indirectRenderCommandAtIndex = Nothing
  , _indirectComputeCommandAtIndex = Nothing
  , _size = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_Q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlIndirectCommandBufferDelegateClass #-}
mtlIndirectCommandBufferDelegateClass :: Class
mtlIndirectCommandBufferDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLIndirectCommandBuffer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_indirectRenderCommandAtIndex = unSelector (mkSelector "indirectRenderCommandAtIndex:")
      sel_indirectComputeCommandAtIndex = unSelector (mkSelector "indirectComputeCommandAtIndex:")
      sel_size = unSelector (mkSelector "size")
  -- indirectRenderCommandAtIndex:
  stub_0 <- wrap_Q_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectCommandBufferOverrides
    case _indirectRenderCommandAtIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "indirectRenderCommandAtIndex:" "@@:Q" stub_0

  -- indirectComputeCommandAtIndex:
  stub_1 <- wrap_Q_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectCommandBufferOverrides
    case _indirectComputeCommandAtIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "indirectComputeCommandAtIndex:" "@@:Q" stub_1

  -- size
  stub_2 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectCommandBufferOverrides
    case _size rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "size" "Q@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIndirectCommandBufferOverrides
    if queriedSel == sel_indirectRenderCommandAtIndex then pure (maybe 0 (const 1) (_indirectRenderCommandAtIndex rec_))
    else if queriedSel == sel_indirectComputeCommandAtIndex then pure (maybe 0 (const 1) (_indirectComputeCommandAtIndex rec_))
    else if queriedSel == sel_size then pure (maybe 0 (const 1) (_size rec_))
    else do
      let super_ = ObjCSuper (RawId self) superCls
      sendSuperMsg super_ (mkSelector "respondsToSelector:") retCULong
        [argPtr (castPtr queriedSel :: Ptr ())]
  addObjCMethod cls "respondsToSelector:" "B@::" rtsStub

  addStablePtrDeallocHandler cls
  objc_registerClassPair cls
  pure cls

-- | Create a new delegate implementing this protocol.
--
-- The returned 'RawId' can be used as a delegate or data source.
newMTLIndirectCommandBuffer :: MTLIndirectCommandBufferOverrides -> IO RawId
newMTLIndirectCommandBuffer overrides = do
  inst <- class_createInstance mtlIndirectCommandBufferDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
