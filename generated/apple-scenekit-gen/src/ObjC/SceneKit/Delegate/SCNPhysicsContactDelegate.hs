{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SCNPhysicsContactDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSCNPhysicsContactDelegate defaultSCNPhysicsContactDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SceneKit.Delegate.SCNPhysicsContactDelegate
  ( SCNPhysicsContactDelegateOverrides(..)
  , defaultSCNPhysicsContactDelegateOverrides
  , newSCNPhysicsContactDelegate
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

-- | Overrides record for @\@protocol SCNPhysicsContactDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SCNPhysicsContactDelegateOverrides = SCNPhysicsContactDelegateOverrides
  { _physicsWorld_didBeginContact :: !(Maybe (RawId -> RawId -> IO ()))
  , _physicsWorld_didUpdateContact :: !(Maybe (RawId -> RawId -> IO ()))
  , _physicsWorld_didEndContact :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSCNPhysicsContactDelegateOverrides :: SCNPhysicsContactDelegateOverrides
defaultSCNPhysicsContactDelegateOverrides = SCNPhysicsContactDelegateOverrides
  { _physicsWorld_didBeginContact = Nothing
  , _physicsWorld_didUpdateContact = Nothing
  , _physicsWorld_didEndContact = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE scnPhysicsContactDelegateDelegateClass #-}
scnPhysicsContactDelegateDelegateClass :: Class
scnPhysicsContactDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSCNPhysicsContactDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_physicsWorld_didBeginContact = unSelector (mkSelector "physicsWorld:didBeginContact:")
      sel_physicsWorld_didUpdateContact = unSelector (mkSelector "physicsWorld:didUpdateContact:")
      sel_physicsWorld_didEndContact = unSelector (mkSelector "physicsWorld:didEndContact:")
  -- physicsWorld:didBeginContact:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNPhysicsContactDelegateOverrides
    case _physicsWorld_didBeginContact rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "physicsWorld:didBeginContact:" "v@:@@" stub_0

  -- physicsWorld:didUpdateContact:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNPhysicsContactDelegateOverrides
    case _physicsWorld_didUpdateContact rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "physicsWorld:didUpdateContact:" "v@:@@" stub_1

  -- physicsWorld:didEndContact:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNPhysicsContactDelegateOverrides
    case _physicsWorld_didEndContact rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "physicsWorld:didEndContact:" "v@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNPhysicsContactDelegateOverrides
    if queriedSel == sel_physicsWorld_didBeginContact then pure (maybe 0 (const 1) (_physicsWorld_didBeginContact rec_))
    else if queriedSel == sel_physicsWorld_didUpdateContact then pure (maybe 0 (const 1) (_physicsWorld_didUpdateContact rec_))
    else if queriedSel == sel_physicsWorld_didEndContact then pure (maybe 0 (const 1) (_physicsWorld_didEndContact rec_))
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
newSCNPhysicsContactDelegate :: SCNPhysicsContactDelegateOverrides -> IO RawId
newSCNPhysicsContactDelegate overrides = do
  inst <- class_createInstance scnPhysicsContactDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
