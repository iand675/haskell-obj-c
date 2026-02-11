{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MKLookAroundViewControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newMKLookAroundViewControllerDelegate defaultMKLookAroundViewControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MapKit.Delegate.MKLookAroundViewControllerDelegate
  ( MKLookAroundViewControllerDelegateOverrides(..)
  , defaultMKLookAroundViewControllerDelegateOverrides
  , newMKLookAroundViewControllerDelegate
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

-- | Overrides record for @\@protocol MKLookAroundViewControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MKLookAroundViewControllerDelegateOverrides = MKLookAroundViewControllerDelegateOverrides
  { _lookAroundViewControllerWillUpdateScene :: !(Maybe (RawId -> IO ()))
  , _lookAroundViewControllerDidUpdateScene :: !(Maybe (RawId -> IO ()))
  , _lookAroundViewControllerWillPresentFullScreen :: !(Maybe (RawId -> IO ()))
  , _lookAroundViewControllerDidPresentFullScreen :: !(Maybe (RawId -> IO ()))
  , _lookAroundViewControllerWillDismissFullScreen :: !(Maybe (RawId -> IO ()))
  , _lookAroundViewControllerDidDismissFullScreen :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMKLookAroundViewControllerDelegateOverrides :: MKLookAroundViewControllerDelegateOverrides
defaultMKLookAroundViewControllerDelegateOverrides = MKLookAroundViewControllerDelegateOverrides
  { _lookAroundViewControllerWillUpdateScene = Nothing
  , _lookAroundViewControllerDidUpdateScene = Nothing
  , _lookAroundViewControllerWillPresentFullScreen = Nothing
  , _lookAroundViewControllerDidPresentFullScreen = Nothing
  , _lookAroundViewControllerWillDismissFullScreen = Nothing
  , _lookAroundViewControllerDidDismissFullScreen = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mkLookAroundViewControllerDelegateDelegateClass #-}
mkLookAroundViewControllerDelegateDelegateClass :: Class
mkLookAroundViewControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMKLookAroundViewControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_lookAroundViewControllerWillUpdateScene = unSelector (mkSelector "lookAroundViewControllerWillUpdateScene:")
      sel_lookAroundViewControllerDidUpdateScene = unSelector (mkSelector "lookAroundViewControllerDidUpdateScene:")
      sel_lookAroundViewControllerWillPresentFullScreen = unSelector (mkSelector "lookAroundViewControllerWillPresentFullScreen:")
      sel_lookAroundViewControllerDidPresentFullScreen = unSelector (mkSelector "lookAroundViewControllerDidPresentFullScreen:")
      sel_lookAroundViewControllerWillDismissFullScreen = unSelector (mkSelector "lookAroundViewControllerWillDismissFullScreen:")
      sel_lookAroundViewControllerDidDismissFullScreen = unSelector (mkSelector "lookAroundViewControllerDidDismissFullScreen:")
  -- lookAroundViewControllerWillUpdateScene:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKLookAroundViewControllerDelegateOverrides
    case _lookAroundViewControllerWillUpdateScene rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "lookAroundViewControllerWillUpdateScene:" "v@:@" stub_0

  -- lookAroundViewControllerDidUpdateScene:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKLookAroundViewControllerDelegateOverrides
    case _lookAroundViewControllerDidUpdateScene rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "lookAroundViewControllerDidUpdateScene:" "v@:@" stub_1

  -- lookAroundViewControllerWillPresentFullScreen:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKLookAroundViewControllerDelegateOverrides
    case _lookAroundViewControllerWillPresentFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "lookAroundViewControllerWillPresentFullScreen:" "v@:@" stub_2

  -- lookAroundViewControllerDidPresentFullScreen:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKLookAroundViewControllerDelegateOverrides
    case _lookAroundViewControllerDidPresentFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "lookAroundViewControllerDidPresentFullScreen:" "v@:@" stub_3

  -- lookAroundViewControllerWillDismissFullScreen:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKLookAroundViewControllerDelegateOverrides
    case _lookAroundViewControllerWillDismissFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "lookAroundViewControllerWillDismissFullScreen:" "v@:@" stub_4

  -- lookAroundViewControllerDidDismissFullScreen:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKLookAroundViewControllerDelegateOverrides
    case _lookAroundViewControllerDidDismissFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "lookAroundViewControllerDidDismissFullScreen:" "v@:@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MKLookAroundViewControllerDelegateOverrides
    if queriedSel == sel_lookAroundViewControllerWillUpdateScene then pure (maybe 0 (const 1) (_lookAroundViewControllerWillUpdateScene rec_))
    else if queriedSel == sel_lookAroundViewControllerDidUpdateScene then pure (maybe 0 (const 1) (_lookAroundViewControllerDidUpdateScene rec_))
    else if queriedSel == sel_lookAroundViewControllerWillPresentFullScreen then pure (maybe 0 (const 1) (_lookAroundViewControllerWillPresentFullScreen rec_))
    else if queriedSel == sel_lookAroundViewControllerDidPresentFullScreen then pure (maybe 0 (const 1) (_lookAroundViewControllerDidPresentFullScreen rec_))
    else if queriedSel == sel_lookAroundViewControllerWillDismissFullScreen then pure (maybe 0 (const 1) (_lookAroundViewControllerWillDismissFullScreen rec_))
    else if queriedSel == sel_lookAroundViewControllerDidDismissFullScreen then pure (maybe 0 (const 1) (_lookAroundViewControllerDidDismissFullScreen rec_))
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
newMKLookAroundViewControllerDelegate :: MKLookAroundViewControllerDelegateOverrides -> IO RawId
newMKLookAroundViewControllerDelegate overrides = do
  inst <- class_createInstance mkLookAroundViewControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
