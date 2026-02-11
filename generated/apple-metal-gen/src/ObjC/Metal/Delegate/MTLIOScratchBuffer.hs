{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLIOScratchBuffer@.
--
-- Usage:
--
-- @
-- delegate <- newMTLIOScratchBuffer defaultMTLIOScratchBufferOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLIOScratchBuffer
  ( MTLIOScratchBufferOverrides(..)
  , defaultMTLIOScratchBufferOverrides
  , newMTLIOScratchBuffer
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

-- | Overrides record for @\@protocol MTLIOScratchBuffer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLIOScratchBufferOverrides = MTLIOScratchBufferOverrides
  { _buffer :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLIOScratchBufferOverrides :: MTLIOScratchBufferOverrides
defaultMTLIOScratchBufferOverrides = MTLIOScratchBufferOverrides
  { _buffer = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlioScratchBufferDelegateClass #-}
mtlioScratchBufferDelegateClass :: Class
mtlioScratchBufferDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLIOScratchBuffer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_buffer = unSelector (mkSelector "buffer")
  -- buffer
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOScratchBufferOverrides
    case _buffer rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "buffer" "@@:" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIOScratchBufferOverrides
    if queriedSel == sel_buffer then pure (maybe 0 (const 1) (_buffer rec_))
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
newMTLIOScratchBuffer :: MTLIOScratchBufferOverrides -> IO RawId
newMTLIOScratchBuffer overrides = do
  inst <- class_createInstance mtlioScratchBufferDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
