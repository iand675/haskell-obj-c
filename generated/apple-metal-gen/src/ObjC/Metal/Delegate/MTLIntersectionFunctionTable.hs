{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLIntersectionFunctionTable@.
--
-- Usage:
--
-- @
-- delegate <- newMTLIntersectionFunctionTable defaultMTLIntersectionFunctionTableOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLIntersectionFunctionTable
  ( MTLIntersectionFunctionTableOverrides(..)
  , defaultMTLIntersectionFunctionTableOverrides
  , newMTLIntersectionFunctionTable
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

-- | Overrides record for @\@protocol MTLIntersectionFunctionTable@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLIntersectionFunctionTableOverrides = MTLIntersectionFunctionTableOverrides
  { _setBuffer_offset_atIndex :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _setFunction_atIndex :: !(Maybe (RawId -> Int -> IO ()))
  , _setVisibleFunctionTable_atBufferIndex :: !(Maybe (RawId -> Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLIntersectionFunctionTableOverrides :: MTLIntersectionFunctionTableOverrides
defaultMTLIntersectionFunctionTableOverrides = MTLIntersectionFunctionTableOverrides
  { _setBuffer_offset_atIndex = Nothing
  , _setFunction_atIndex = Nothing
  , _setVisibleFunctionTable_atBufferIndex = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlIntersectionFunctionTableDelegateClass #-}
mtlIntersectionFunctionTableDelegateClass :: Class
mtlIntersectionFunctionTableDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLIntersectionFunctionTable" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_setBuffer_offset_atIndex = unSelector (mkSelector "setBuffer:offset:atIndex:")
      sel_setFunction_atIndex = unSelector (mkSelector "setFunction:atIndex:")
      sel_setVisibleFunctionTable_atBufferIndex = unSelector (mkSelector "setVisibleFunctionTable:atBufferIndex:")
  -- setBuffer:offset:atIndex:
  stub_0 <- wrap_at_Q_Q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIntersectionFunctionTableOverrides
    case _setBuffer_offset_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "setBuffer:offset:atIndex:" "v@:@QQ" stub_0

  -- setFunction:atIndex:
  stub_1 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIntersectionFunctionTableOverrides
    case _setFunction_atIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setFunction:atIndex:" "v@:@Q" stub_1

  -- setVisibleFunctionTable:atBufferIndex:
  stub_2 <- wrap_at_Q_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIntersectionFunctionTableOverrides
    case _setVisibleFunctionTable_atBufferIndex rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1)
  addObjCMethod cls "setVisibleFunctionTable:atBufferIndex:" "v@:@Q" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLIntersectionFunctionTableOverrides
    if queriedSel == sel_setBuffer_offset_atIndex then pure (maybe 0 (const 1) (_setBuffer_offset_atIndex rec_))
    else if queriedSel == sel_setFunction_atIndex then pure (maybe 0 (const 1) (_setFunction_atIndex rec_))
    else if queriedSel == sel_setVisibleFunctionTable_atBufferIndex then pure (maybe 0 (const 1) (_setVisibleFunctionTable_atBufferIndex rec_))
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
newMTLIntersectionFunctionTable :: MTLIntersectionFunctionTableOverrides -> IO RawId
newMTLIntersectionFunctionTable overrides = do
  inst <- class_createInstance mtlIntersectionFunctionTableDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
