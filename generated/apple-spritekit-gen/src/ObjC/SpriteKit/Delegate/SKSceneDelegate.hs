{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SKSceneDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSKSceneDelegate defaultSKSceneDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SpriteKit.Delegate.SKSceneDelegate
  ( SKSceneDelegateOverrides(..)
  , defaultSKSceneDelegateOverrides
  , newSKSceneDelegate
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

-- | Overrides record for @\@protocol SKSceneDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SKSceneDelegateOverrides = SKSceneDelegateOverrides
  { _update_forScene :: !(Maybe (Double -> RawId -> IO ()))
  , _didEvaluateActionsForScene :: !(Maybe (RawId -> IO ()))
  , _didSimulatePhysicsForScene :: !(Maybe (RawId -> IO ()))
  , _didApplyConstraintsForScene :: !(Maybe (RawId -> IO ()))
  , _didFinishUpdateForScene :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSKSceneDelegateOverrides :: SKSceneDelegateOverrides
defaultSKSceneDelegateOverrides = SKSceneDelegateOverrides
  { _update_forScene = Nothing
  , _didEvaluateActionsForScene = Nothing
  , _didSimulatePhysicsForScene = Nothing
  , _didApplyConstraintsForScene = Nothing
  , _didFinishUpdateForScene = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_d_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CDouble -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE skSceneDelegateDelegateClass #-}
skSceneDelegateDelegateClass :: Class
skSceneDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSKSceneDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_update_forScene = unSelector (mkSelector "update:forScene:")
      sel_didEvaluateActionsForScene = unSelector (mkSelector "didEvaluateActionsForScene:")
      sel_didSimulatePhysicsForScene = unSelector (mkSelector "didSimulatePhysicsForScene:")
      sel_didApplyConstraintsForScene = unSelector (mkSelector "didApplyConstraintsForScene:")
      sel_didFinishUpdateForScene = unSelector (mkSelector "didFinishUpdateForScene:")
  -- update:forScene:
  stub_0 <- wrap_d_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKSceneDelegateOverrides
    case _update_forScene rec_ of
      Nothing -> pure ()
      Just f -> f (realToFrac arg0) (RawId arg1)
  addObjCMethod cls "update:forScene:" "v@:d@" stub_0

  -- didEvaluateActionsForScene:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKSceneDelegateOverrides
    case _didEvaluateActionsForScene rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "didEvaluateActionsForScene:" "v@:@" stub_1

  -- didSimulatePhysicsForScene:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKSceneDelegateOverrides
    case _didSimulatePhysicsForScene rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "didSimulatePhysicsForScene:" "v@:@" stub_2

  -- didApplyConstraintsForScene:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKSceneDelegateOverrides
    case _didApplyConstraintsForScene rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "didApplyConstraintsForScene:" "v@:@" stub_3

  -- didFinishUpdateForScene:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKSceneDelegateOverrides
    case _didFinishUpdateForScene rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "didFinishUpdateForScene:" "v@:@" stub_4

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SKSceneDelegateOverrides
    if queriedSel == sel_update_forScene then pure (maybe 0 (const 1) (_update_forScene rec_))
    else if queriedSel == sel_didEvaluateActionsForScene then pure (maybe 0 (const 1) (_didEvaluateActionsForScene rec_))
    else if queriedSel == sel_didSimulatePhysicsForScene then pure (maybe 0 (const 1) (_didSimulatePhysicsForScene rec_))
    else if queriedSel == sel_didApplyConstraintsForScene then pure (maybe 0 (const 1) (_didApplyConstraintsForScene rec_))
    else if queriedSel == sel_didFinishUpdateForScene then pure (maybe 0 (const 1) (_didFinishUpdateForScene rec_))
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
newSKSceneDelegate :: SKSceneDelegateOverrides -> IO RawId
newSKSceneDelegate overrides = do
  inst <- class_createInstance skSceneDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
