{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLIOScratchBufferAllocator@.
--
-- Usage:
--
-- @
-- delegate <- newMTLIOScratchBufferAllocator defaultMTLIOScratchBufferAllocatorOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLIOScratchBufferAllocator
  ( MTLIOScratchBufferAllocatorOverrides(..)
  , defaultMTLIOScratchBufferAllocatorOverrides
  , newMTLIOScratchBufferAllocator
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

-- | Overrides record for @\@protocol MTLIOScratchBufferAllocator@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLIOScratchBufferAllocatorOverrides = MTLIOScratchBufferAllocatorOverrides
  { _newScratchBufferWithMinimumSize :: !(Maybe (Int -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLIOScratchBufferAllocatorOverrides :: MTLIOScratchBufferAllocatorOverrides
defaultMTLIOScratchBufferAllocatorOverrides = MTLIOScratchBufferAllocatorOverrides
  { _newScratchBufferWithMinimumSize = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlioScratchBufferAllocatorDelegateClass #-}
mtlioScratchBufferAllocatorDelegateClass :: Class
mtlioScratchBufferAllocatorDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLIOScratchBufferAllocator" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_newScratchBufferWithMinimumSize = unSelector (mkSelector "newScratchBufferWithMinimumSize:")
  -- newScratchBufferWithMinimumSize:
  stub_0 <- wrap_Q_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOScratchBufferAllocatorOverrides
    case _newScratchBufferWithMinimumSize rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (fromIntegral arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "newScratchBufferWithMinimumSize:" "@@:Q" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOScratchBufferAllocatorOverrides
    if queriedSel == sel_newScratchBufferWithMinimumSize then pure (maybe 0 (const 1) (_newScratchBufferWithMinimumSize rec_))
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
newMTLIOScratchBufferAllocator :: MTLIOScratchBufferAllocatorOverrides -> IO RawId
newMTLIOScratchBufferAllocator overrides = do
  inst <- class_createInstance mtlioScratchBufferAllocatorDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
