{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol IKScannerDeviceViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newIKScannerDeviceViewDelegate defaultIKScannerDeviceViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Quartz.Delegate.IKScannerDeviceViewDelegate
  ( IKScannerDeviceViewDelegateOverrides(..)
  , defaultIKScannerDeviceViewDelegateOverrides
  , newIKScannerDeviceViewDelegate
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

-- | Overrides record for @\@protocol IKScannerDeviceViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data IKScannerDeviceViewDelegateOverrides = IKScannerDeviceViewDelegateOverrides
  { _scannerDeviceView_didScanToURL_fileData_error :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _scannerDeviceView_didScanToURL_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _scannerDeviceView_didScanToBandData_scanInfo_error :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _scannerDeviceView_didEncounterError :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultIKScannerDeviceViewDelegateOverrides :: IKScannerDeviceViewDelegateOverrides
defaultIKScannerDeviceViewDelegateOverrides = IKScannerDeviceViewDelegateOverrides
  { _scannerDeviceView_didScanToURL_fileData_error = Nothing
  , _scannerDeviceView_didScanToURL_error = Nothing
  , _scannerDeviceView_didScanToBandData_scanInfo_error = Nothing
  , _scannerDeviceView_didEncounterError = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ikScannerDeviceViewDelegateDelegateClass #-}
ikScannerDeviceViewDelegateDelegateClass :: Class
ikScannerDeviceViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsIKScannerDeviceViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_scannerDeviceView_didScanToURL_fileData_error = unSelector (mkSelector "scannerDeviceView:didScanToURL:fileData:error:")
      sel_scannerDeviceView_didScanToURL_error = unSelector (mkSelector "scannerDeviceView:didScanToURL:error:")
      sel_scannerDeviceView_didScanToBandData_scanInfo_error = unSelector (mkSelector "scannerDeviceView:didScanToBandData:scanInfo:error:")
      sel_scannerDeviceView_didEncounterError = unSelector (mkSelector "scannerDeviceView:didEncounterError:")
  -- scannerDeviceView:didScanToURL:fileData:error:
  stub_0 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKScannerDeviceViewDelegateOverrides
    case _scannerDeviceView_didScanToURL_fileData_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "scannerDeviceView:didScanToURL:fileData:error:" "v@:@@@@" stub_0

  -- scannerDeviceView:didScanToURL:error:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKScannerDeviceViewDelegateOverrides
    case _scannerDeviceView_didScanToURL_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "scannerDeviceView:didScanToURL:error:" "v@:@@@" stub_1

  -- scannerDeviceView:didScanToBandData:scanInfo:error:
  stub_2 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKScannerDeviceViewDelegateOverrides
    case _scannerDeviceView_didScanToBandData_scanInfo_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "scannerDeviceView:didScanToBandData:scanInfo:error:" "v@:@@@@" stub_2

  -- scannerDeviceView:didEncounterError:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKScannerDeviceViewDelegateOverrides
    case _scannerDeviceView_didEncounterError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "scannerDeviceView:didEncounterError:" "v@:@@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO IKScannerDeviceViewDelegateOverrides
    if queriedSel == sel_scannerDeviceView_didScanToURL_fileData_error then pure (maybe 0 (const 1) (_scannerDeviceView_didScanToURL_fileData_error rec_))
    else if queriedSel == sel_scannerDeviceView_didScanToURL_error then pure (maybe 0 (const 1) (_scannerDeviceView_didScanToURL_error rec_))
    else if queriedSel == sel_scannerDeviceView_didScanToBandData_scanInfo_error then pure (maybe 0 (const 1) (_scannerDeviceView_didScanToBandData_scanInfo_error rec_))
    else if queriedSel == sel_scannerDeviceView_didEncounterError then pure (maybe 0 (const 1) (_scannerDeviceView_didEncounterError rec_))
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
newIKScannerDeviceViewDelegate :: IKScannerDeviceViewDelegateOverrides -> IO RawId
newIKScannerDeviceViewDelegate overrides = do
  inst <- class_createInstance ikScannerDeviceViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
