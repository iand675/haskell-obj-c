{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol PDEPlugIn@.
--
-- Usage:
--
-- @
-- delegate <- newPDEPlugIn defaultPDEPlugInOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ApplicationServices.Delegate.PDEPlugIn
  ( PDEPlugInOverrides(..)
  , defaultPDEPlugInOverrides
  , newPDEPlugIn
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

-- | Overrides record for @\@protocol PDEPlugIn@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data PDEPlugInOverrides = PDEPlugInOverrides
  { _initWithBundle :: !(Maybe (RawId -> IO RawId))
  , _pdePanelsForType_withHostInfo :: !(Maybe (RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultPDEPlugInOverrides :: PDEPlugInOverrides
defaultPDEPlugInOverrides = PDEPlugInOverrides
  { _initWithBundle = Nothing
  , _pdePanelsForType_withHostInfo = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE pdePlugInDelegateClass #-}
pdePlugInDelegateClass :: Class
pdePlugInDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsPDEPlugIn" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_initWithBundle = unSelector (mkSelector "initWithBundle:")
      sel_pdePanelsForType_withHostInfo = unSelector (mkSelector "PDEPanelsForType:withHostInfo:")
  -- initWithBundle:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPlugInOverrides
    case _initWithBundle rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "initWithBundle:" "@@:@" stub_0

  -- PDEPanelsForType:withHostInfo:
  stub_1 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPlugInOverrides
    case _pdePanelsForType_withHostInfo rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "PDEPanelsForType:withHostInfo:" "@@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO PDEPlugInOverrides
    if queriedSel == sel_initWithBundle then pure (maybe 0 (const 1) (_initWithBundle rec_))
    else if queriedSel == sel_pdePanelsForType_withHostInfo then pure (maybe 0 (const 1) (_pdePanelsForType_withHostInfo rec_))
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
newPDEPlugIn :: PDEPlugInOverrides -> IO RawId
newPDEPlugIn overrides = do
  inst <- class_createInstance pdePlugInDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
