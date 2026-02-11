{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLCommandEncoder@.
--
-- Usage:
--
-- @
-- delegate <- newMTLCommandEncoder defaultMTLCommandEncoderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLCommandEncoder
  ( MTLCommandEncoderOverrides(..)
  , defaultMTLCommandEncoderOverrides
  , newMTLCommandEncoder
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

-- | Overrides record for @\@protocol MTLCommandEncoder@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLCommandEncoderOverrides = MTLCommandEncoderOverrides
  { _endEncoding :: !(Maybe (IO ()))
  , _insertDebugSignpost :: !(Maybe (RawId -> IO ()))
  , _pushDebugGroup :: !(Maybe (RawId -> IO ()))
  , _popDebugGroup :: !(Maybe (IO ()))
  , _device :: !(Maybe (IO RawId))
  , _label :: !(Maybe (IO RawId))
  , _setLabel :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLCommandEncoderOverrides :: MTLCommandEncoderOverrides
defaultMTLCommandEncoderOverrides = MTLCommandEncoderOverrides
  { _endEncoding = Nothing
  , _insertDebugSignpost = Nothing
  , _pushDebugGroup = Nothing
  , _popDebugGroup = Nothing
  , _device = Nothing
  , _label = Nothing
  , _setLabel = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlCommandEncoderDelegateClass #-}
mtlCommandEncoderDelegateClass :: Class
mtlCommandEncoderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLCommandEncoder" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_endEncoding = unSelector (mkSelector "endEncoding")
      sel_insertDebugSignpost = unSelector (mkSelector "insertDebugSignpost:")
      sel_pushDebugGroup = unSelector (mkSelector "pushDebugGroup:")
      sel_popDebugGroup = unSelector (mkSelector "popDebugGroup")
      sel_device = unSelector (mkSelector "device")
      sel_label = unSelector (mkSelector "label")
      sel_setLabel = unSelector (mkSelector "setLabel:")
  -- endEncoding
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCommandEncoderOverrides
    case _endEncoding rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "endEncoding" "v@:" stub_0

  -- insertDebugSignpost:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCommandEncoderOverrides
    case _insertDebugSignpost rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "insertDebugSignpost:" "v@:@" stub_1

  -- pushDebugGroup:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCommandEncoderOverrides
    case _pushDebugGroup rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pushDebugGroup:" "v@:@" stub_2

  -- popDebugGroup
  stub_3 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCommandEncoderOverrides
    case _popDebugGroup rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "popDebugGroup" "v@:" stub_3

  -- device
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCommandEncoderOverrides
    case _device rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "device" "@@:" stub_4

  -- label
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCommandEncoderOverrides
    case _label rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "label" "@@:" stub_5

  -- setLabel:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCommandEncoderOverrides
    case _setLabel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setLabel:" "v@:@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCommandEncoderOverrides
    if queriedSel == sel_endEncoding then pure (maybe 0 (const 1) (_endEncoding rec_))
    else if queriedSel == sel_insertDebugSignpost then pure (maybe 0 (const 1) (_insertDebugSignpost rec_))
    else if queriedSel == sel_pushDebugGroup then pure (maybe 0 (const 1) (_pushDebugGroup rec_))
    else if queriedSel == sel_popDebugGroup then pure (maybe 0 (const 1) (_popDebugGroup rec_))
    else if queriedSel == sel_device then pure (maybe 0 (const 1) (_device rec_))
    else if queriedSel == sel_label then pure (maybe 0 (const 1) (_label rec_))
    else if queriedSel == sel_setLabel then pure (maybe 0 (const 1) (_setLabel rec_))
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
newMTLCommandEncoder :: MTLCommandEncoderOverrides -> IO RawId
newMTLCommandEncoder overrides = do
  inst <- class_createInstance mtlCommandEncoderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
