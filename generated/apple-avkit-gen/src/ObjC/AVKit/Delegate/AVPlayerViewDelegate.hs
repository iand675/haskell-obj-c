{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVPlayerViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVPlayerViewDelegate defaultAVPlayerViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVKit.Delegate.AVPlayerViewDelegate
  ( AVPlayerViewDelegateOverrides(..)
  , defaultAVPlayerViewDelegateOverrides
  , newAVPlayerViewDelegate
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

-- | Overrides record for @\@protocol AVPlayerViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVPlayerViewDelegateOverrides = AVPlayerViewDelegateOverrides
  { _playerViewWillEnterFullScreen :: !(Maybe (RawId -> IO ()))
  , _playerViewDidEnterFullScreen :: !(Maybe (RawId -> IO ()))
  , _playerViewWillExitFullScreen :: !(Maybe (RawId -> IO ()))
  , _playerViewDidExitFullScreen :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVPlayerViewDelegateOverrides :: AVPlayerViewDelegateOverrides
defaultAVPlayerViewDelegateOverrides = AVPlayerViewDelegateOverrides
  { _playerViewWillEnterFullScreen = Nothing
  , _playerViewDidEnterFullScreen = Nothing
  , _playerViewWillExitFullScreen = Nothing
  , _playerViewDidExitFullScreen = Nothing
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
{-# NOINLINE avPlayerViewDelegateDelegateClass #-}
avPlayerViewDelegateDelegateClass :: Class
avPlayerViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVPlayerViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_playerViewWillEnterFullScreen = unSelector (mkSelector "playerViewWillEnterFullScreen:")
      sel_playerViewDidEnterFullScreen = unSelector (mkSelector "playerViewDidEnterFullScreen:")
      sel_playerViewWillExitFullScreen = unSelector (mkSelector "playerViewWillExitFullScreen:")
      sel_playerViewDidExitFullScreen = unSelector (mkSelector "playerViewDidExitFullScreen:")
  -- playerViewWillEnterFullScreen:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerViewDelegateOverrides
    case _playerViewWillEnterFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "playerViewWillEnterFullScreen:" "v@:@" stub_0

  -- playerViewDidEnterFullScreen:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerViewDelegateOverrides
    case _playerViewDidEnterFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "playerViewDidEnterFullScreen:" "v@:@" stub_1

  -- playerViewWillExitFullScreen:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerViewDelegateOverrides
    case _playerViewWillExitFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "playerViewWillExitFullScreen:" "v@:@" stub_2

  -- playerViewDidExitFullScreen:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerViewDelegateOverrides
    case _playerViewDidExitFullScreen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "playerViewDidExitFullScreen:" "v@:@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerViewDelegateOverrides
    if queriedSel == sel_playerViewWillEnterFullScreen then pure (maybe 0 (const 1) (_playerViewWillEnterFullScreen rec_))
    else if queriedSel == sel_playerViewDidEnterFullScreen then pure (maybe 0 (const 1) (_playerViewDidEnterFullScreen rec_))
    else if queriedSel == sel_playerViewWillExitFullScreen then pure (maybe 0 (const 1) (_playerViewWillExitFullScreen rec_))
    else if queriedSel == sel_playerViewDidExitFullScreen then pure (maybe 0 (const 1) (_playerViewDidExitFullScreen rec_))
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
newAVPlayerViewDelegate :: AVPlayerViewDelegateOverrides -> IO RawId
newAVPlayerViewDelegate overrides = do
  inst <- class_createInstance avPlayerViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
