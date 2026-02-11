{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVAudio3DMixing@.
--
-- Usage:
--
-- @
-- delegate <- newAVAudio3DMixing defaultAVAudio3DMixingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVFAudio.Delegate.AVAudio3DMixing
  ( AVAudio3DMixingOverrides(..)
  , defaultAVAudio3DMixingOverrides
  , newAVAudio3DMixing
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

-- | Overrides record for @\@protocol AVAudio3DMixing@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVAudio3DMixingOverrides = AVAudio3DMixingOverrides
  { _rate :: !(Maybe (IO Float))
  , _setRate :: !(Maybe (Float -> IO ()))
  , _reverbBlend :: !(Maybe (IO Float))
  , _setReverbBlend :: !(Maybe (Float -> IO ()))
  , _obstruction :: !(Maybe (IO Float))
  , _setObstruction :: !(Maybe (Float -> IO ()))
  , _occlusion :: !(Maybe (IO Float))
  , _setOcclusion :: !(Maybe (Float -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVAudio3DMixingOverrides :: AVAudio3DMixingOverrides
defaultAVAudio3DMixingOverrides = AVAudio3DMixingOverrides
  { _rate = Nothing
  , _setRate = Nothing
  , _reverbBlend = Nothing
  , _setReverbBlend = Nothing
  , _obstruction = Nothing
  , _setObstruction = Nothing
  , _occlusion = Nothing
  , _setOcclusion = Nothing
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
{-# NOINLINE avAudio3DMixingDelegateClass #-}
avAudio3DMixingDelegateClass :: Class
avAudio3DMixingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVAudio3DMixing" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_rate = unSelector (mkSelector "rate")
      sel_setRate = unSelector (mkSelector "setRate:")
      sel_reverbBlend = unSelector (mkSelector "reverbBlend")
      sel_setReverbBlend = unSelector (mkSelector "setReverbBlend:")
      sel_obstruction = unSelector (mkSelector "obstruction")
      sel_setObstruction = unSelector (mkSelector "setObstruction:")
      sel_occlusion = unSelector (mkSelector "occlusion")
      sel_setOcclusion = unSelector (mkSelector "setOcclusion:")
  -- rate
  stub_0 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudio3DMixingOverrides
    case _rate rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "rate" "f@:" stub_0

  -- setRate:
  stub_1 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudio3DMixingOverrides
    case _setRate rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setRate:" "v@:f" stub_1

  -- reverbBlend
  stub_2 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudio3DMixingOverrides
    case _reverbBlend rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "reverbBlend" "f@:" stub_2

  -- setReverbBlend:
  stub_3 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudio3DMixingOverrides
    case _setReverbBlend rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setReverbBlend:" "v@:f" stub_3

  -- obstruction
  stub_4 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudio3DMixingOverrides
    case _obstruction rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "obstruction" "f@:" stub_4

  -- setObstruction:
  stub_5 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudio3DMixingOverrides
    case _setObstruction rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setObstruction:" "v@:f" stub_5

  -- occlusion
  stub_6 <- wrap_f $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudio3DMixingOverrides
    case _occlusion rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f 
        pure (realToFrac r)
  addObjCMethod cls "occlusion" "f@:" stub_6

  -- setOcclusion:
  stub_7 <- wrap_f_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudio3DMixingOverrides
    case _setOcclusion rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "setOcclusion:" "v@:f" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVAudio3DMixingOverrides
    if queriedSel == sel_rate then pure (maybe 0 (const 1) (_rate rec_))
    else if queriedSel == sel_setRate then pure (maybe 0 (const 1) (_setRate rec_))
    else if queriedSel == sel_reverbBlend then pure (maybe 0 (const 1) (_reverbBlend rec_))
    else if queriedSel == sel_setReverbBlend then pure (maybe 0 (const 1) (_setReverbBlend rec_))
    else if queriedSel == sel_obstruction then pure (maybe 0 (const 1) (_obstruction rec_))
    else if queriedSel == sel_setObstruction then pure (maybe 0 (const 1) (_setObstruction rec_))
    else if queriedSel == sel_occlusion then pure (maybe 0 (const 1) (_occlusion rec_))
    else if queriedSel == sel_setOcclusion then pure (maybe 0 (const 1) (_setOcclusion rec_))
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
newAVAudio3DMixing :: AVAudio3DMixingOverrides -> IO RawId
newAVAudio3DMixing overrides = do
  inst <- class_createInstance avAudio3DMixingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
