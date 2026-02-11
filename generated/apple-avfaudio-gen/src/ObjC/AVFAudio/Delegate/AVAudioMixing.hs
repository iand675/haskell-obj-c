{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVAudioMixing@.
--
-- Usage:
--
-- @
-- delegate <- newAVAudioMixing defaultAVAudioMixingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFAudio.Delegate.AVAudioMixing
  ( AVAudioMixingOverrides(..)
  , defaultAVAudioMixingOverrides
  , newAVAudioMixing
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

-- | Overrides record for @\@protocol AVAudioMixing@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVAudioMixingOverrides = AVAudioMixingOverrides
  { _destinationForMixer_bus :: !(Maybe (RawId -> Int -> IO RawId))
  , _volume :: !(Maybe (IO Float))
  , _setVolume :: !(Maybe (Float -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVAudioMixingOverrides :: AVAudioMixingOverrides
defaultAVAudioMixingOverrides = AVAudioMixingOverrides
  { _destinationForMixer_bus = Nothing
  , _volume = Nothing
  , _setVolume = Nothing
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
  wrap_at_Q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avAudioMixingDelegateClass #-}
avAudioMixingDelegateClass :: Class
avAudioMixingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVAudioMixing" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_destinationForMixer_bus = unSelector (mkSelector "destinationForMixer:bus:")
      sel_volume = unSelector (mkSelector "volume")
      sel_setVolume = unSelector (mkSelector "setVolume:")
  -- destinationForMixer:bus:
  stub_0 <- wrap_at_Q_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioMixingOverrides
    case _destinationForMixer_bus rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "destinationForMixer:bus:" "@@:@Q" stub_0

  -- volume
  stub_1 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioMixingOverrides
    case _volume rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "volume" "f@:" stub_1

  -- setVolume:
  stub_2 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioMixingOverrides
    case _setVolume rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setVolume:" "v@:f" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioMixingOverrides
    if queriedSel == sel_destinationForMixer_bus then pure (maybe 0 (const 1) (_destinationForMixer_bus rec_))
    else if queriedSel == sel_volume then pure (maybe 0 (const 1) (_volume rec_))
    else if queriedSel == sel_setVolume then pure (maybe 0 (const 1) (_setVolume rec_))
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
newAVAudioMixing :: AVAudioMixingOverrides -> IO RawId
newAVAudioMixing overrides = do
  inst <- class_createInstance avAudioMixingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
