{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLBufferBinding@.
--
-- Usage:
--
-- @
-- delegate <- newMTLBufferBinding defaultMTLBufferBindingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLBufferBinding
  ( MTLBufferBindingOverrides(..)
  , defaultMTLBufferBindingOverrides
  , newMTLBufferBinding
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

-- | Overrides record for @\@protocol MTLBufferBinding@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLBufferBindingOverrides = MTLBufferBindingOverrides
  { _bufferAlignment :: !(Maybe (IO Int))
  , _bufferDataSize :: !(Maybe (IO Int))
  , _bufferStructType :: !(Maybe (IO RawId))
  , _bufferPointerType :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLBufferBindingOverrides :: MTLBufferBindingOverrides
defaultMTLBufferBindingOverrides = MTLBufferBindingOverrides
  { _bufferAlignment = Nothing
  , _bufferDataSize = Nothing
  , _bufferStructType = Nothing
  , _bufferPointerType = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlBufferBindingDelegateClass #-}
mtlBufferBindingDelegateClass :: Class
mtlBufferBindingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLBufferBinding" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_bufferAlignment = unSelector (mkSelector "bufferAlignment")
      sel_bufferDataSize = unSelector (mkSelector "bufferDataSize")
      sel_bufferStructType = unSelector (mkSelector "bufferStructType")
      sel_bufferPointerType = unSelector (mkSelector "bufferPointerType")
  -- bufferAlignment
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBufferBindingOverrides
    case _bufferAlignment rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "bufferAlignment" "Q@:" stub_0

  -- bufferDataSize
  stub_1 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBufferBindingOverrides
    case _bufferDataSize rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "bufferDataSize" "Q@:" stub_1

  -- bufferStructType
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBufferBindingOverrides
    case _bufferStructType rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "bufferStructType" "@@:" stub_2

  -- bufferPointerType
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBufferBindingOverrides
    case _bufferPointerType rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "bufferPointerType" "@@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLBufferBindingOverrides
    if queriedSel == sel_bufferAlignment then pure (maybe 0 (const 1) (_bufferAlignment rec_))
    else if queriedSel == sel_bufferDataSize then pure (maybe 0 (const 1) (_bufferDataSize rec_))
    else if queriedSel == sel_bufferStructType then pure (maybe 0 (const 1) (_bufferStructType rec_))
    else if queriedSel == sel_bufferPointerType then pure (maybe 0 (const 1) (_bufferPointerType rec_))
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
newMTLBufferBinding :: MTLBufferBindingOverrides -> IO RawId
newMTLBufferBinding overrides = do
  inst <- class_createInstance mtlBufferBindingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
