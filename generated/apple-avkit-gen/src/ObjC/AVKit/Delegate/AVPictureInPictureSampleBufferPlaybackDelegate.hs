{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVPictureInPictureSampleBufferPlaybackDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVPictureInPictureSampleBufferPlaybackDelegate defaultAVPictureInPictureSampleBufferPlaybackDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVKit.Delegate.AVPictureInPictureSampleBufferPlaybackDelegate
  ( AVPictureInPictureSampleBufferPlaybackDelegateOverrides(..)
  , defaultAVPictureInPictureSampleBufferPlaybackDelegateOverrides
  , newAVPictureInPictureSampleBufferPlaybackDelegate
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

-- | Overrides record for @\@protocol AVPictureInPictureSampleBufferPlaybackDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVPictureInPictureSampleBufferPlaybackDelegateOverrides = AVPictureInPictureSampleBufferPlaybackDelegateOverrides
  { _pictureInPictureController_setPlaying :: !(Maybe (RawId -> Bool -> IO ()))
  , _pictureInPictureControllerIsPlaybackPaused :: !(Maybe (RawId -> IO Bool))
  , _pictureInPictureControllerShouldProhibitBackgroundAudioPlayback :: !(Maybe (RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultAVPictureInPictureSampleBufferPlaybackDelegateOverrides :: AVPictureInPictureSampleBufferPlaybackDelegateOverrides
defaultAVPictureInPictureSampleBufferPlaybackDelegateOverrides = AVPictureInPictureSampleBufferPlaybackDelegateOverrides
  { _pictureInPictureController_setPlaying = Nothing
  , _pictureInPictureControllerIsPlaybackPaused = Nothing
  , _pictureInPictureControllerShouldProhibitBackgroundAudioPlayback = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_B_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avPictureInPictureSampleBufferPlaybackDelegateDelegateClass #-}
avPictureInPictureSampleBufferPlaybackDelegateDelegateClass :: Class
avPictureInPictureSampleBufferPlaybackDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVPictureInPictureSampleBufferPlaybackDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pictureInPictureController_setPlaying = unSelector (mkSelector "pictureInPictureController:setPlaying:")
      sel_pictureInPictureControllerIsPlaybackPaused = unSelector (mkSelector "pictureInPictureControllerIsPlaybackPaused:")
      sel_pictureInPictureControllerShouldProhibitBackgroundAudioPlayback = unSelector (mkSelector "pictureInPictureControllerShouldProhibitBackgroundAudioPlayback:")
  -- pictureInPictureController:setPlaying:
  stub_0 <- wrap_at_B_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPictureInPictureSampleBufferPlaybackDelegateOverrides
    case _pictureInPictureController_setPlaying rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (arg1 /= 0)
  addObjCMethod cls "pictureInPictureController:setPlaying:" "v@:@B" stub_0

  -- pictureInPictureControllerIsPlaybackPaused:
  stub_1 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPictureInPictureSampleBufferPlaybackDelegateOverrides
    case _pictureInPictureControllerIsPlaybackPaused rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "pictureInPictureControllerIsPlaybackPaused:" "B@:@" stub_1

  -- pictureInPictureControllerShouldProhibitBackgroundAudioPlayback:
  stub_2 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPictureInPictureSampleBufferPlaybackDelegateOverrides
    case _pictureInPictureControllerShouldProhibitBackgroundAudioPlayback rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "pictureInPictureControllerShouldProhibitBackgroundAudioPlayback:" "B@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPictureInPictureSampleBufferPlaybackDelegateOverrides
    if queriedSel == sel_pictureInPictureController_setPlaying then pure (maybe 0 (const 1) (_pictureInPictureController_setPlaying rec_))
    else if queriedSel == sel_pictureInPictureControllerIsPlaybackPaused then pure (maybe 0 (const 1) (_pictureInPictureControllerIsPlaybackPaused rec_))
    else if queriedSel == sel_pictureInPictureControllerShouldProhibitBackgroundAudioPlayback then pure (maybe 0 (const 1) (_pictureInPictureControllerShouldProhibitBackgroundAudioPlayback rec_))
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
newAVPictureInPictureSampleBufferPlaybackDelegate :: AVPictureInPictureSampleBufferPlaybackDelegateOverrides -> IO RawId
newAVPictureInPictureSampleBufferPlaybackDelegate overrides = do
  inst <- class_createInstance avPictureInPictureSampleBufferPlaybackDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
