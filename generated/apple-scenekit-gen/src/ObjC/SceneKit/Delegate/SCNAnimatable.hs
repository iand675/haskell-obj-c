{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SCNAnimatable@.
--
-- Usage:
--
-- @
-- delegate <- newSCNAnimatable defaultSCNAnimatableOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SceneKit.Delegate.SCNAnimatable
  ( SCNAnimatableOverrides(..)
  , defaultSCNAnimatableOverrides
  , newSCNAnimatable
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

-- | Overrides record for @\@protocol SCNAnimatable@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SCNAnimatableOverrides = SCNAnimatableOverrides
  { _addAnimation_forKey :: !(Maybe (RawId -> RawId -> IO ()))
  , _addAnimationPlayer_forKey :: !(Maybe (RawId -> RawId -> IO ()))
  , _removeAllAnimations :: !(Maybe (IO ()))
  , _removeAllAnimationsWithBlendOutDuration :: !(Maybe (Double -> IO ()))
  , _removeAnimationForKey :: !(Maybe (RawId -> IO ()))
  , _removeAnimationForKey_blendOutDuration :: !(Maybe (RawId -> Double -> IO ()))
  , _animationPlayerForKey :: !(Maybe (RawId -> IO RawId))
  , _removeAnimationForKey_fadeOutDuration :: !(Maybe (RawId -> Double -> IO ()))
  , _animationForKey :: !(Maybe (RawId -> IO RawId))
  , _pauseAnimationForKey :: !(Maybe (RawId -> IO ()))
  , _resumeAnimationForKey :: !(Maybe (RawId -> IO ()))
  , _setSpeed_forAnimationKey :: !(Maybe (Double -> RawId -> IO ()))
  , _isAnimationForKeyPaused :: !(Maybe (RawId -> IO Bool))
  , _animationKeys :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultSCNAnimatableOverrides :: SCNAnimatableOverrides
defaultSCNAnimatableOverrides = SCNAnimatableOverrides
  { _addAnimation_forKey = Nothing
  , _addAnimationPlayer_forKey = Nothing
  , _removeAllAnimations = Nothing
  , _removeAllAnimationsWithBlendOutDuration = Nothing
  , _removeAnimationForKey = Nothing
  , _removeAnimationForKey_blendOutDuration = Nothing
  , _animationPlayerForKey = Nothing
  , _removeAnimationForKey_fadeOutDuration = Nothing
  , _animationForKey = Nothing
  , _pauseAnimationForKey = Nothing
  , _resumeAnimationForKey = Nothing
  , _setSpeed_forAnimationKey = Nothing
  , _isAnimationForKeyPaused = Nothing
  , _animationKeys = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_d_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_d_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> IO ()))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_d_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> IO ()))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE scnAnimatableDelegateClass #-}
scnAnimatableDelegateClass :: Class
scnAnimatableDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSCNAnimatable" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_addAnimation_forKey = unSelector (mkSelector "addAnimation:forKey:")
      sel_addAnimationPlayer_forKey = unSelector (mkSelector "addAnimationPlayer:forKey:")
      sel_removeAllAnimations = unSelector (mkSelector "removeAllAnimations")
      sel_removeAllAnimationsWithBlendOutDuration = unSelector (mkSelector "removeAllAnimationsWithBlendOutDuration:")
      sel_removeAnimationForKey = unSelector (mkSelector "removeAnimationForKey:")
      sel_removeAnimationForKey_blendOutDuration = unSelector (mkSelector "removeAnimationForKey:blendOutDuration:")
      sel_animationPlayerForKey = unSelector (mkSelector "animationPlayerForKey:")
      sel_removeAnimationForKey_fadeOutDuration = unSelector (mkSelector "removeAnimationForKey:fadeOutDuration:")
      sel_animationForKey = unSelector (mkSelector "animationForKey:")
      sel_pauseAnimationForKey = unSelector (mkSelector "pauseAnimationForKey:")
      sel_resumeAnimationForKey = unSelector (mkSelector "resumeAnimationForKey:")
      sel_setSpeed_forAnimationKey = unSelector (mkSelector "setSpeed:forAnimationKey:")
      sel_isAnimationForKeyPaused = unSelector (mkSelector "isAnimationForKeyPaused:")
      sel_animationKeys = unSelector (mkSelector "animationKeys")
  -- addAnimation:forKey:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _addAnimation_forKey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "addAnimation:forKey:" "v@:@@" stub_0

  -- addAnimationPlayer:forKey:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _addAnimationPlayer_forKey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "addAnimationPlayer:forKey:" "v@:@@" stub_1

  -- removeAllAnimations
  stub_2 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _removeAllAnimations rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "removeAllAnimations" "v@:" stub_2

  -- removeAllAnimationsWithBlendOutDuration:
  stub_3 <- wrap_d_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _removeAllAnimationsWithBlendOutDuration rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0)
  addObjCMethod cls "removeAllAnimationsWithBlendOutDuration:" "v@:d" stub_3

  -- removeAnimationForKey:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _removeAnimationForKey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "removeAnimationForKey:" "v@:@" stub_4

  -- removeAnimationForKey:blendOutDuration:
  stub_5 <- wrap_at_d_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _removeAnimationForKey_blendOutDuration rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (realToFrac arg1)
  addObjCMethod cls "removeAnimationForKey:blendOutDuration:" "v@:@d" stub_5

  -- animationPlayerForKey:
  stub_6 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _animationPlayerForKey rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "animationPlayerForKey:" "@@:@" stub_6

  -- removeAnimationForKey:fadeOutDuration:
  stub_7 <- wrap_at_d_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _removeAnimationForKey_fadeOutDuration rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (realToFrac arg1)
  addObjCMethod cls "removeAnimationForKey:fadeOutDuration:" "v@:@d" stub_7

  -- animationForKey:
  stub_8 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _animationForKey rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "animationForKey:" "@@:@" stub_8

  -- pauseAnimationForKey:
  stub_9 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _pauseAnimationForKey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "pauseAnimationForKey:" "v@:@" stub_9

  -- resumeAnimationForKey:
  stub_10 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _resumeAnimationForKey rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "resumeAnimationForKey:" "v@:@" stub_10

  -- setSpeed:forAnimationKey:
  stub_11 <- wrap_d_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _setSpeed_forAnimationKey rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0) (RawId arg1)
  addObjCMethod cls "setSpeed:forAnimationKey:" "v@:d@" stub_11

  -- isAnimationForKeyPaused:
  stub_12 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _isAnimationForKeyPaused rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "isAnimationForKeyPaused:" "B@:@" stub_12

  -- animationKeys
  stub_13 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    case _animationKeys rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "animationKeys" "@@:" stub_13

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAnimatableOverrides
    if queriedSel == sel_addAnimation_forKey then pure (maybe 0 (const 1) (_addAnimation_forKey rec_))
    else if queriedSel == sel_addAnimationPlayer_forKey then pure (maybe 0 (const 1) (_addAnimationPlayer_forKey rec_))
    else if queriedSel == sel_removeAllAnimations then pure (maybe 0 (const 1) (_removeAllAnimations rec_))
    else if queriedSel == sel_removeAllAnimationsWithBlendOutDuration then pure (maybe 0 (const 1) (_removeAllAnimationsWithBlendOutDuration rec_))
    else if queriedSel == sel_removeAnimationForKey then pure (maybe 0 (const 1) (_removeAnimationForKey rec_))
    else if queriedSel == sel_removeAnimationForKey_blendOutDuration then pure (maybe 0 (const 1) (_removeAnimationForKey_blendOutDuration rec_))
    else if queriedSel == sel_animationPlayerForKey then pure (maybe 0 (const 1) (_animationPlayerForKey rec_))
    else if queriedSel == sel_removeAnimationForKey_fadeOutDuration then pure (maybe 0 (const 1) (_removeAnimationForKey_fadeOutDuration rec_))
    else if queriedSel == sel_animationForKey then pure (maybe 0 (const 1) (_animationForKey rec_))
    else if queriedSel == sel_pauseAnimationForKey then pure (maybe 0 (const 1) (_pauseAnimationForKey rec_))
    else if queriedSel == sel_resumeAnimationForKey then pure (maybe 0 (const 1) (_resumeAnimationForKey rec_))
    else if queriedSel == sel_setSpeed_forAnimationKey then pure (maybe 0 (const 1) (_setSpeed_forAnimationKey rec_))
    else if queriedSel == sel_isAnimationForKeyPaused then pure (maybe 0 (const 1) (_isAnimationForKeyPaused rec_))
    else if queriedSel == sel_animationKeys then pure (maybe 0 (const 1) (_animationKeys rec_))
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
newSCNAnimatable :: SCNAnimatableOverrides -> IO RawId
newSCNAnimatable overrides = do
  inst <- class_createInstance scnAnimatableDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
