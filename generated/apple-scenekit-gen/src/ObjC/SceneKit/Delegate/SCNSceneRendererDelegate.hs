{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SCNSceneRendererDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSCNSceneRendererDelegate defaultSCNSceneRendererDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SceneKit.Delegate.SCNSceneRendererDelegate
  ( SCNSceneRendererDelegateOverrides(..)
  , defaultSCNSceneRendererDelegateOverrides
  , newSCNSceneRendererDelegate
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

-- | Overrides record for @\@protocol SCNSceneRendererDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SCNSceneRendererDelegateOverrides = SCNSceneRendererDelegateOverrides
  { _renderer_updateAtTime :: !(Maybe (RawId -> Double -> IO ()))
  , _renderer_didApplyAnimationsAtTime :: !(Maybe (RawId -> Double -> IO ()))
  , _renderer_didSimulatePhysicsAtTime :: !(Maybe (RawId -> Double -> IO ()))
  , _renderer_didApplyConstraintsAtTime :: !(Maybe (RawId -> Double -> IO ()))
  , _renderer_willRenderScene_atTime :: !(Maybe (RawId -> RawId -> Double -> IO ()))
  , _renderer_didRenderScene_atTime :: !(Maybe (RawId -> RawId -> Double -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSCNSceneRendererDelegateOverrides :: SCNSceneRendererDelegateOverrides
defaultSCNSceneRendererDelegateOverrides = SCNSceneRendererDelegateOverrides
  { _renderer_updateAtTime = Nothing
  , _renderer_didApplyAnimationsAtTime = Nothing
  , _renderer_didSimulatePhysicsAtTime = Nothing
  , _renderer_didApplyConstraintsAtTime = Nothing
  , _renderer_willRenderScene_atTime = Nothing
  , _renderer_didRenderScene_atTime = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_d_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CDouble -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CDouble -> IO ()))

foreign import ccall "wrapper"
  wrap_at_d_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE scnSceneRendererDelegateDelegateClass #-}
scnSceneRendererDelegateDelegateClass :: Class
scnSceneRendererDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSCNSceneRendererDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_renderer_updateAtTime = unSelector (mkSelector "renderer:updateAtTime:")
      sel_renderer_didApplyAnimationsAtTime = unSelector (mkSelector "renderer:didApplyAnimationsAtTime:")
      sel_renderer_didSimulatePhysicsAtTime = unSelector (mkSelector "renderer:didSimulatePhysicsAtTime:")
      sel_renderer_didApplyConstraintsAtTime = unSelector (mkSelector "renderer:didApplyConstraintsAtTime:")
      sel_renderer_willRenderScene_atTime = unSelector (mkSelector "renderer:willRenderScene:atTime:")
      sel_renderer_didRenderScene_atTime = unSelector (mkSelector "renderer:didRenderScene:atTime:")
  -- renderer:updateAtTime:
  stub_0 <- wrap_at_d_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererDelegateOverrides
    case _renderer_updateAtTime rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (realToFrac arg1)
  addObjCMethod cls "renderer:updateAtTime:" "v@:@d" stub_0

  -- renderer:didApplyAnimationsAtTime:
  stub_1 <- wrap_at_d_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererDelegateOverrides
    case _renderer_didApplyAnimationsAtTime rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (realToFrac arg1)
  addObjCMethod cls "renderer:didApplyAnimationsAtTime:" "v@:@d" stub_1

  -- renderer:didSimulatePhysicsAtTime:
  stub_2 <- wrap_at_d_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererDelegateOverrides
    case _renderer_didSimulatePhysicsAtTime rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (realToFrac arg1)
  addObjCMethod cls "renderer:didSimulatePhysicsAtTime:" "v@:@d" stub_2

  -- renderer:didApplyConstraintsAtTime:
  stub_3 <- wrap_at_d_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererDelegateOverrides
    case _renderer_didApplyConstraintsAtTime rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (realToFrac arg1)
  addObjCMethod cls "renderer:didApplyConstraintsAtTime:" "v@:@d" stub_3

  -- renderer:willRenderScene:atTime:
  stub_4 <- wrap_at_at_d_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererDelegateOverrides
    case _renderer_willRenderScene_atTime rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (realToFrac arg2)
  addObjCMethod cls "renderer:willRenderScene:atTime:" "v@:@@d" stub_4

  -- renderer:didRenderScene:atTime:
  stub_5 <- wrap_at_at_d_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererDelegateOverrides
    case _renderer_didRenderScene_atTime rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (realToFrac arg2)
  addObjCMethod cls "renderer:didRenderScene:atTime:" "v@:@@d" stub_5

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNSceneRendererDelegateOverrides
    if queriedSel == sel_renderer_updateAtTime then pure (maybe 0 (const 1) (_renderer_updateAtTime rec_))
    else if queriedSel == sel_renderer_didApplyAnimationsAtTime then pure (maybe 0 (const 1) (_renderer_didApplyAnimationsAtTime rec_))
    else if queriedSel == sel_renderer_didSimulatePhysicsAtTime then pure (maybe 0 (const 1) (_renderer_didSimulatePhysicsAtTime rec_))
    else if queriedSel == sel_renderer_didApplyConstraintsAtTime then pure (maybe 0 (const 1) (_renderer_didApplyConstraintsAtTime rec_))
    else if queriedSel == sel_renderer_willRenderScene_atTime then pure (maybe 0 (const 1) (_renderer_willRenderScene_atTime rec_))
    else if queriedSel == sel_renderer_didRenderScene_atTime then pure (maybe 0 (const 1) (_renderer_didRenderScene_atTime rec_))
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
newSCNSceneRendererDelegate :: SCNSceneRendererDelegateOverrides -> IO RawId
newSCNSceneRendererDelegate overrides = do
  inst <- class_createInstance scnSceneRendererDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
