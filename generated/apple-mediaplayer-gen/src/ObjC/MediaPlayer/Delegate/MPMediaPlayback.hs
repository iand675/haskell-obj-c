{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MPMediaPlayback@.
--
-- Usage:
--
-- @
-- delegate <- newMPMediaPlayback defaultMPMediaPlaybackOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MediaPlayer.Delegate.MPMediaPlayback
  ( MPMediaPlaybackOverrides(..)
  , defaultMPMediaPlaybackOverrides
  , newMPMediaPlayback
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

-- | Overrides record for @\@protocol MPMediaPlayback@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MPMediaPlaybackOverrides = MPMediaPlaybackOverrides
  { _prepareToPlay :: !(Maybe (IO ()))
  , _play :: !(Maybe (IO ()))
  , _pause :: !(Maybe (IO ()))
  , _stop :: !(Maybe (IO ()))
  , _beginSeekingForward :: !(Maybe (IO ()))
  , _beginSeekingBackward :: !(Maybe (IO ()))
  , _endSeeking :: !(Maybe (IO ()))
  , _isPreparedToPlay :: !(Maybe (IO Bool))
  , _currentPlaybackTime :: !(Maybe (IO Double))
  , _setCurrentPlaybackTime :: !(Maybe (Double -> IO ()))
  , _currentPlaybackRate :: !(Maybe (IO Float))
  , _setCurrentPlaybackRate :: !(Maybe (Float -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMPMediaPlaybackOverrides :: MPMediaPlaybackOverrides
defaultMPMediaPlaybackOverrides = MPMediaPlaybackOverrides
  { _prepareToPlay = Nothing
  , _play = Nothing
  , _pause = Nothing
  , _stop = Nothing
  , _beginSeekingForward = Nothing
  , _beginSeekingBackward = Nothing
  , _endSeeking = Nothing
  , _isPreparedToPlay = Nothing
  , _currentPlaybackTime = Nothing
  , _setCurrentPlaybackTime = Nothing
  , _currentPlaybackRate = Nothing
  , _setCurrentPlaybackRate = Nothing
  }

foreign import ccall "wrapper"
  wrap_f_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CFloat -> IO ()))

foreign import ccall "wrapper"
  wrap_f
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CFloat))

foreign import ccall "wrapper"
  wrap_d_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> IO ()))

foreign import ccall "wrapper"
  wrap_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CDouble))

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mpMediaPlaybackDelegateClass #-}
mpMediaPlaybackDelegateClass :: Class
mpMediaPlaybackDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMPMediaPlayback" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_prepareToPlay = unSelector (mkSelector "prepareToPlay")
      sel_play = unSelector (mkSelector "play")
      sel_pause = unSelector (mkSelector "pause")
      sel_stop = unSelector (mkSelector "stop")
      sel_beginSeekingForward = unSelector (mkSelector "beginSeekingForward")
      sel_beginSeekingBackward = unSelector (mkSelector "beginSeekingBackward")
      sel_endSeeking = unSelector (mkSelector "endSeeking")
      sel_isPreparedToPlay = unSelector (mkSelector "isPreparedToPlay")
      sel_currentPlaybackTime = unSelector (mkSelector "currentPlaybackTime")
      sel_setCurrentPlaybackTime = unSelector (mkSelector "setCurrentPlaybackTime:")
      sel_currentPlaybackRate = unSelector (mkSelector "currentPlaybackRate")
      sel_setCurrentPlaybackRate = unSelector (mkSelector "setCurrentPlaybackRate:")
  -- prepareToPlay
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    case _prepareToPlay rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "prepareToPlay" "v@:" stub_0

  -- play
  stub_1 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    case _play rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "play" "v@:" stub_1

  -- pause
  stub_2 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    case _pause rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "pause" "v@:" stub_2

  -- stop
  stub_3 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    case _stop rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "stop" "v@:" stub_3

  -- beginSeekingForward
  stub_4 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    case _beginSeekingForward rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "beginSeekingForward" "v@:" stub_4

  -- beginSeekingBackward
  stub_5 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    case _beginSeekingBackward rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "beginSeekingBackward" "v@:" stub_5

  -- endSeeking
  stub_6 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    case _endSeeking rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "endSeeking" "v@:" stub_6

  -- isPreparedToPlay
  stub_7 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    case _isPreparedToPlay rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "isPreparedToPlay" "B@:" stub_7

  -- currentPlaybackTime
  stub_8 <- wrap_d $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    case _currentPlaybackTime rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "currentPlaybackTime" "d@:" stub_8

  -- setCurrentPlaybackTime:
  stub_9 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    case _setCurrentPlaybackTime rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setCurrentPlaybackTime:" "v@:d" stub_9

  -- currentPlaybackRate
  stub_10 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    case _currentPlaybackRate rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "currentPlaybackRate" "f@:" stub_10

  -- setCurrentPlaybackRate:
  stub_11 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    case _setCurrentPlaybackRate rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setCurrentPlaybackRate:" "v@:f" stub_11

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MPMediaPlaybackOverrides
    if queriedSel == sel_prepareToPlay then pure (maybe 0 (const 1) (_prepareToPlay rec_))
    else if queriedSel == sel_play then pure (maybe 0 (const 1) (_play rec_))
    else if queriedSel == sel_pause then pure (maybe 0 (const 1) (_pause rec_))
    else if queriedSel == sel_stop then pure (maybe 0 (const 1) (_stop rec_))
    else if queriedSel == sel_beginSeekingForward then pure (maybe 0 (const 1) (_beginSeekingForward rec_))
    else if queriedSel == sel_beginSeekingBackward then pure (maybe 0 (const 1) (_beginSeekingBackward rec_))
    else if queriedSel == sel_endSeeking then pure (maybe 0 (const 1) (_endSeeking rec_))
    else if queriedSel == sel_isPreparedToPlay then pure (maybe 0 (const 1) (_isPreparedToPlay rec_))
    else if queriedSel == sel_currentPlaybackTime then pure (maybe 0 (const 1) (_currentPlaybackTime rec_))
    else if queriedSel == sel_setCurrentPlaybackTime then pure (maybe 0 (const 1) (_setCurrentPlaybackTime rec_))
    else if queriedSel == sel_currentPlaybackRate then pure (maybe 0 (const 1) (_currentPlaybackRate rec_))
    else if queriedSel == sel_setCurrentPlaybackRate then pure (maybe 0 (const 1) (_setCurrentPlaybackRate rec_))
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
newMPMediaPlayback :: MPMediaPlaybackOverrides -> IO RawId
newMPMediaPlayback overrides = do
  inst <- class_createInstance mpMediaPlaybackDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
