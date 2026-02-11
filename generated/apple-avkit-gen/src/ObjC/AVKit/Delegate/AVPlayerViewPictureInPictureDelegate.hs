{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVPlayerViewPictureInPictureDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVPlayerViewPictureInPictureDelegate defaultAVPlayerViewPictureInPictureDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVKit.Delegate.AVPlayerViewPictureInPictureDelegate
  ( AVPlayerViewPictureInPictureDelegateOverrides(..)
  , defaultAVPlayerViewPictureInPictureDelegateOverrides
  , newAVPlayerViewPictureInPictureDelegate
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

-- | Overrides record for @\@protocol AVPlayerViewPictureInPictureDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVPlayerViewPictureInPictureDelegateOverrides = AVPlayerViewPictureInPictureDelegateOverrides
  { _playerViewWillStartPictureInPicture :: !(Maybe (RawId -> IO ()))
  , _playerViewDidStartPictureInPicture :: !(Maybe (RawId -> IO ()))
  , _playerView_failedToStartPictureInPictureWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _playerViewWillStopPictureInPicture :: !(Maybe (RawId -> IO ()))
  , _playerViewDidStopPictureInPicture :: !(Maybe (RawId -> IO ()))
  , _playerViewShouldAutomaticallyDismissAtPictureInPictureStart :: !(Maybe (RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultAVPlayerViewPictureInPictureDelegateOverrides :: AVPlayerViewPictureInPictureDelegateOverrides
defaultAVPlayerViewPictureInPictureDelegateOverrides = AVPlayerViewPictureInPictureDelegateOverrides
  { _playerViewWillStartPictureInPicture = Nothing
  , _playerViewDidStartPictureInPicture = Nothing
  , _playerView_failedToStartPictureInPictureWithError = Nothing
  , _playerViewWillStopPictureInPicture = Nothing
  , _playerViewDidStopPictureInPicture = Nothing
  , _playerViewShouldAutomaticallyDismissAtPictureInPictureStart = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avPlayerViewPictureInPictureDelegateDelegateClass #-}
avPlayerViewPictureInPictureDelegateDelegateClass :: Class
avPlayerViewPictureInPictureDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVPlayerViewPictureInPictureDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_playerViewWillStartPictureInPicture = unSelector (mkSelector "playerViewWillStartPictureInPicture:")
      sel_playerViewDidStartPictureInPicture = unSelector (mkSelector "playerViewDidStartPictureInPicture:")
      sel_playerView_failedToStartPictureInPictureWithError = unSelector (mkSelector "playerView:failedToStartPictureInPictureWithError:")
      sel_playerViewWillStopPictureInPicture = unSelector (mkSelector "playerViewWillStopPictureInPicture:")
      sel_playerViewDidStopPictureInPicture = unSelector (mkSelector "playerViewDidStopPictureInPicture:")
      sel_playerViewShouldAutomaticallyDismissAtPictureInPictureStart = unSelector (mkSelector "playerViewShouldAutomaticallyDismissAtPictureInPictureStart:")
  -- playerViewWillStartPictureInPicture:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerViewPictureInPictureDelegateOverrides
    case _playerViewWillStartPictureInPicture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "playerViewWillStartPictureInPicture:" "v@:@" stub_0

  -- playerViewDidStartPictureInPicture:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerViewPictureInPictureDelegateOverrides
    case _playerViewDidStartPictureInPicture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "playerViewDidStartPictureInPicture:" "v@:@" stub_1

  -- playerView:failedToStartPictureInPictureWithError:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerViewPictureInPictureDelegateOverrides
    case _playerView_failedToStartPictureInPictureWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "playerView:failedToStartPictureInPictureWithError:" "v@:@@" stub_2

  -- playerViewWillStopPictureInPicture:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerViewPictureInPictureDelegateOverrides
    case _playerViewWillStopPictureInPicture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "playerViewWillStopPictureInPicture:" "v@:@" stub_3

  -- playerViewDidStopPictureInPicture:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerViewPictureInPictureDelegateOverrides
    case _playerViewDidStopPictureInPicture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "playerViewDidStopPictureInPicture:" "v@:@" stub_4

  -- playerViewShouldAutomaticallyDismissAtPictureInPictureStart:
  stub_5 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerViewPictureInPictureDelegateOverrides
    case _playerViewShouldAutomaticallyDismissAtPictureInPictureStart rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "playerViewShouldAutomaticallyDismissAtPictureInPictureStart:" "B@:@" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPlayerViewPictureInPictureDelegateOverrides
    if queriedSel == sel_playerViewWillStartPictureInPicture then pure (maybe 0 (const 1) (_playerViewWillStartPictureInPicture rec_))
    else if queriedSel == sel_playerViewDidStartPictureInPicture then pure (maybe 0 (const 1) (_playerViewDidStartPictureInPicture rec_))
    else if queriedSel == sel_playerView_failedToStartPictureInPictureWithError then pure (maybe 0 (const 1) (_playerView_failedToStartPictureInPictureWithError rec_))
    else if queriedSel == sel_playerViewWillStopPictureInPicture then pure (maybe 0 (const 1) (_playerViewWillStopPictureInPicture rec_))
    else if queriedSel == sel_playerViewDidStopPictureInPicture then pure (maybe 0 (const 1) (_playerViewDidStopPictureInPicture rec_))
    else if queriedSel == sel_playerViewShouldAutomaticallyDismissAtPictureInPictureStart then pure (maybe 0 (const 1) (_playerViewShouldAutomaticallyDismissAtPictureInPictureStart rec_))
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
newAVPlayerViewPictureInPictureDelegate :: AVPlayerViewPictureInPictureDelegateOverrides -> IO RawId
newAVPlayerViewPictureInPictureDelegate overrides = do
  inst <- class_createInstance avPlayerViewPictureInPictureDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
