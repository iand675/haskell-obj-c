{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVPictureInPictureControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVPictureInPictureControllerDelegate defaultAVPictureInPictureControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVKit.Delegate.AVPictureInPictureControllerDelegate
  ( AVPictureInPictureControllerDelegateOverrides(..)
  , defaultAVPictureInPictureControllerDelegateOverrides
  , newAVPictureInPictureControllerDelegate
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

-- | Overrides record for @\@protocol AVPictureInPictureControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVPictureInPictureControllerDelegateOverrides = AVPictureInPictureControllerDelegateOverrides
  { _pictureInPictureControllerWillStartPictureInPicture :: !(Maybe (RawId -> IO ()))
  , _pictureInPictureControllerDidStartPictureInPicture :: !(Maybe (RawId -> IO ()))
  , _pictureInPictureController_failedToStartPictureInPictureWithError :: !(Maybe (RawId -> RawId -> IO ()))
  , _pictureInPictureControllerWillStopPictureInPicture :: !(Maybe (RawId -> IO ()))
  , _pictureInPictureControllerDidStopPictureInPicture :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVPictureInPictureControllerDelegateOverrides :: AVPictureInPictureControllerDelegateOverrides
defaultAVPictureInPictureControllerDelegateOverrides = AVPictureInPictureControllerDelegateOverrides
  { _pictureInPictureControllerWillStartPictureInPicture = Nothing
  , _pictureInPictureControllerDidStartPictureInPicture = Nothing
  , _pictureInPictureController_failedToStartPictureInPictureWithError = Nothing
  , _pictureInPictureControllerWillStopPictureInPicture = Nothing
  , _pictureInPictureControllerDidStopPictureInPicture = Nothing
  }

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
{-# NOINLINE avPictureInPictureControllerDelegateDelegateClass #-}
avPictureInPictureControllerDelegateDelegateClass :: Class
avPictureInPictureControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVPictureInPictureControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pictureInPictureControllerWillStartPictureInPicture = unSelector (mkSelector "pictureInPictureControllerWillStartPictureInPicture:")
      sel_pictureInPictureControllerDidStartPictureInPicture = unSelector (mkSelector "pictureInPictureControllerDidStartPictureInPicture:")
      sel_pictureInPictureController_failedToStartPictureInPictureWithError = unSelector (mkSelector "pictureInPictureController:failedToStartPictureInPictureWithError:")
      sel_pictureInPictureControllerWillStopPictureInPicture = unSelector (mkSelector "pictureInPictureControllerWillStopPictureInPicture:")
      sel_pictureInPictureControllerDidStopPictureInPicture = unSelector (mkSelector "pictureInPictureControllerDidStopPictureInPicture:")
  -- pictureInPictureControllerWillStartPictureInPicture:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPictureInPictureControllerDelegateOverrides
    case _pictureInPictureControllerWillStartPictureInPicture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pictureInPictureControllerWillStartPictureInPicture:" "v@:@" stub_0

  -- pictureInPictureControllerDidStartPictureInPicture:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPictureInPictureControllerDelegateOverrides
    case _pictureInPictureControllerDidStartPictureInPicture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pictureInPictureControllerDidStartPictureInPicture:" "v@:@" stub_1

  -- pictureInPictureController:failedToStartPictureInPictureWithError:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPictureInPictureControllerDelegateOverrides
    case _pictureInPictureController_failedToStartPictureInPictureWithError rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "pictureInPictureController:failedToStartPictureInPictureWithError:" "v@:@@" stub_2

  -- pictureInPictureControllerWillStopPictureInPicture:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPictureInPictureControllerDelegateOverrides
    case _pictureInPictureControllerWillStopPictureInPicture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pictureInPictureControllerWillStopPictureInPicture:" "v@:@" stub_3

  -- pictureInPictureControllerDidStopPictureInPicture:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPictureInPictureControllerDelegateOverrides
    case _pictureInPictureControllerDidStopPictureInPicture rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pictureInPictureControllerDidStopPictureInPicture:" "v@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVPictureInPictureControllerDelegateOverrides
    if queriedSel == sel_pictureInPictureControllerWillStartPictureInPicture then pure (maybe 0 (const 1) (_pictureInPictureControllerWillStartPictureInPicture rec_))
    else if queriedSel == sel_pictureInPictureControllerDidStartPictureInPicture then pure (maybe 0 (const 1) (_pictureInPictureControllerDidStartPictureInPicture rec_))
    else if queriedSel == sel_pictureInPictureController_failedToStartPictureInPictureWithError then pure (maybe 0 (const 1) (_pictureInPictureController_failedToStartPictureInPictureWithError rec_))
    else if queriedSel == sel_pictureInPictureControllerWillStopPictureInPicture then pure (maybe 0 (const 1) (_pictureInPictureControllerWillStopPictureInPicture rec_))
    else if queriedSel == sel_pictureInPictureControllerDidStopPictureInPicture then pure (maybe 0 (const 1) (_pictureInPictureControllerDidStopPictureInPicture rec_))
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
newAVPictureInPictureControllerDelegate :: AVPictureInPictureControllerDelegateOverrides -> IO RawId
newAVPictureInPictureControllerDelegate overrides = do
  inst <- class_createInstance avPictureInPictureControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
