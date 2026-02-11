{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MTLCaptureScope@.
--
-- Usage:
--
-- @
-- delegate <- newMTLCaptureScope defaultMTLCaptureScopeOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Metal.Delegate.MTLCaptureScope
  ( MTLCaptureScopeOverrides(..)
  , defaultMTLCaptureScopeOverrides
  , newMTLCaptureScope
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

-- | Overrides record for @\@protocol MTLCaptureScope@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MTLCaptureScopeOverrides = MTLCaptureScopeOverrides
  { _beginScope :: !(Maybe (IO ()))
  , _endScope :: !(Maybe (IO ()))
  , _label :: !(Maybe (IO RawId))
  , _setLabel :: !(Maybe (RawId -> IO ()))
  , _device :: !(Maybe (IO RawId))
  , _commandQueue :: !(Maybe (IO RawId))
  , _mtl4CommandQueue :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMTLCaptureScopeOverrides :: MTLCaptureScopeOverrides
defaultMTLCaptureScopeOverrides = MTLCaptureScopeOverrides
  { _beginScope = Nothing
  , _endScope = Nothing
  , _label = Nothing
  , _setLabel = Nothing
  , _device = Nothing
  , _commandQueue = Nothing
  , _mtl4CommandQueue = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mtlCaptureScopeDelegateClass #-}
mtlCaptureScopeDelegateClass :: Class
mtlCaptureScopeDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMTLCaptureScope" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_beginScope = unSelector (mkSelector "beginScope")
      sel_endScope = unSelector (mkSelector "endScope")
      sel_label = unSelector (mkSelector "label")
      sel_setLabel = unSelector (mkSelector "setLabel:")
      sel_device = unSelector (mkSelector "device")
      sel_commandQueue = unSelector (mkSelector "commandQueue")
      sel_mtl4CommandQueue = unSelector (mkSelector "mtl4CommandQueue")
  -- beginScope
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCaptureScopeOverrides
    case _beginScope rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "beginScope" "v@:" stub_0

  -- endScope
  stub_1 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCaptureScopeOverrides
    case _endScope rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "endScope" "v@:" stub_1

  -- label
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCaptureScopeOverrides
    case _label rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "label" "@@:" stub_2

  -- setLabel:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCaptureScopeOverrides
    case _setLabel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setLabel:" "v@:@" stub_3

  -- device
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCaptureScopeOverrides
    case _device rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "device" "@@:" stub_4

  -- commandQueue
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCaptureScopeOverrides
    case _commandQueue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "commandQueue" "@@:" stub_5

  -- mtl4CommandQueue
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCaptureScopeOverrides
    case _mtl4CommandQueue rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "mtl4CommandQueue" "@@:" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MTLCaptureScopeOverrides
    if queriedSel == sel_beginScope then pure (maybe 0 (const 1) (_beginScope rec_))
    else if queriedSel == sel_endScope then pure (maybe 0 (const 1) (_endScope rec_))
    else if queriedSel == sel_label then pure (maybe 0 (const 1) (_label rec_))
    else if queriedSel == sel_setLabel then pure (maybe 0 (const 1) (_setLabel rec_))
    else if queriedSel == sel_device then pure (maybe 0 (const 1) (_device rec_))
    else if queriedSel == sel_commandQueue then pure (maybe 0 (const 1) (_commandQueue rec_))
    else if queriedSel == sel_mtl4CommandQueue then pure (maybe 0 (const 1) (_mtl4CommandQueue rec_))
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
newMTLCaptureScope :: MTLCaptureScopeOverrides -> IO RawId
newMTLCaptureScope overrides = do
  inst <- class_createInstance mtlCaptureScopeDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
