{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MCBrowserViewControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newMCBrowserViewControllerDelegate defaultMCBrowserViewControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MultipeerConnectivity.Delegate.MCBrowserViewControllerDelegate
  ( MCBrowserViewControllerDelegateOverrides(..)
  , defaultMCBrowserViewControllerDelegateOverrides
  , newMCBrowserViewControllerDelegate
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

-- | Overrides record for @\@protocol MCBrowserViewControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MCBrowserViewControllerDelegateOverrides = MCBrowserViewControllerDelegateOverrides
  { _browserViewControllerDidFinish :: !(Maybe (RawId -> IO ()))
  , _browserViewControllerWasCancelled :: !(Maybe (RawId -> IO ()))
  , _browserViewController_shouldPresentNearbyPeer_withDiscoveryInfo :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultMCBrowserViewControllerDelegateOverrides :: MCBrowserViewControllerDelegateOverrides
defaultMCBrowserViewControllerDelegateOverrides = MCBrowserViewControllerDelegateOverrides
  { _browserViewControllerDidFinish = Nothing
  , _browserViewControllerWasCancelled = Nothing
  , _browserViewController_shouldPresentNearbyPeer_withDiscoveryInfo = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mcBrowserViewControllerDelegateDelegateClass #-}
mcBrowserViewControllerDelegateDelegateClass :: Class
mcBrowserViewControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMCBrowserViewControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_browserViewControllerDidFinish = unSelector (mkSelector "browserViewControllerDidFinish:")
      sel_browserViewControllerWasCancelled = unSelector (mkSelector "browserViewControllerWasCancelled:")
      sel_browserViewController_shouldPresentNearbyPeer_withDiscoveryInfo = unSelector (mkSelector "browserViewController:shouldPresentNearbyPeer:withDiscoveryInfo:")
  -- browserViewControllerDidFinish:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCBrowserViewControllerDelegateOverrides
    case _browserViewControllerDidFinish rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "browserViewControllerDidFinish:" "v@:@" stub_0

  -- browserViewControllerWasCancelled:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCBrowserViewControllerDelegateOverrides
    case _browserViewControllerWasCancelled rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "browserViewControllerWasCancelled:" "v@:@" stub_1

  -- browserViewController:shouldPresentNearbyPeer:withDiscoveryInfo:
  stub_2 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCBrowserViewControllerDelegateOverrides
    case _browserViewController_shouldPresentNearbyPeer_withDiscoveryInfo rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "browserViewController:shouldPresentNearbyPeer:withDiscoveryInfo:" "B@:@@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MCBrowserViewControllerDelegateOverrides
    if queriedSel == sel_browserViewControllerDidFinish then pure (maybe 0 (const 1) (_browserViewControllerDidFinish rec_))
    else if queriedSel == sel_browserViewControllerWasCancelled then pure (maybe 0 (const 1) (_browserViewControllerWasCancelled rec_))
    else if queriedSel == sel_browserViewController_shouldPresentNearbyPeer_withDiscoveryInfo then pure (maybe 0 (const 1) (_browserViewController_shouldPresentNearbyPeer_withDiscoveryInfo rec_))
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
newMCBrowserViewControllerDelegate :: MCBrowserViewControllerDelegateOverrides -> IO RawId
newMCBrowserViewControllerDelegate overrides = do
  inst <- class_createInstance mcBrowserViewControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
