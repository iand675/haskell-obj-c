{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVAudioStereoMixing@.
--
-- Usage:
--
-- @
-- delegate <- newAVAudioStereoMixing defaultAVAudioStereoMixingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFAudio.Delegate.AVAudioStereoMixing
  ( AVAudioStereoMixingOverrides(..)
  , defaultAVAudioStereoMixingOverrides
  , newAVAudioStereoMixing
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

-- | Overrides record for @\@protocol AVAudioStereoMixing@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVAudioStereoMixingOverrides = AVAudioStereoMixingOverrides
  { _pan :: !(Maybe (IO Float))
  , _setPan :: !(Maybe (Float -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVAudioStereoMixingOverrides :: AVAudioStereoMixingOverrides
defaultAVAudioStereoMixingOverrides = AVAudioStereoMixingOverrides
  { _pan = Nothing
  , _setPan = Nothing
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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE avAudioStereoMixingDelegateClass #-}
avAudioStereoMixingDelegateClass :: Class
avAudioStereoMixingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVAudioStereoMixing" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_pan = unSelector (mkSelector "pan")
      sel_setPan = unSelector (mkSelector "setPan:")
  -- pan
  stub_0 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioStereoMixingOverrides
    case _pan rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "pan" "f@:" stub_0

  -- setPan:
  stub_1 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioStereoMixingOverrides
    case _setPan rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setPan:" "v@:f" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudioStereoMixingOverrides
    if queriedSel == sel_pan then pure (maybe 0 (const 1) (_pan rec_))
    else if queriedSel == sel_setPan then pure (maybe 0 (const 1) (_setPan rec_))
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
newAVAudioStereoMixing :: AVAudioStereoMixingOverrides -> IO RawId
newAVAudioStereoMixing overrides = do
  inst <- class_createInstance avAudioStereoMixingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
