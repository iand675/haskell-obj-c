{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SCNAvoidOccluderConstraintDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSCNAvoidOccluderConstraintDelegate defaultSCNAvoidOccluderConstraintDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SceneKit.Delegate.SCNAvoidOccluderConstraintDelegate
  ( SCNAvoidOccluderConstraintDelegateOverrides(..)
  , defaultSCNAvoidOccluderConstraintDelegateOverrides
  , newSCNAvoidOccluderConstraintDelegate
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

-- | Overrides record for @\@protocol SCNAvoidOccluderConstraintDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SCNAvoidOccluderConstraintDelegateOverrides = SCNAvoidOccluderConstraintDelegateOverrides
  { _avoidOccluderConstraint_shouldAvoidOccluder_forNode :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _avoidOccluderConstraint_didAvoidOccluder_forNode :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSCNAvoidOccluderConstraintDelegateOverrides :: SCNAvoidOccluderConstraintDelegateOverrides
defaultSCNAvoidOccluderConstraintDelegateOverrides = SCNAvoidOccluderConstraintDelegateOverrides
  { _avoidOccluderConstraint_shouldAvoidOccluder_forNode = Nothing
  , _avoidOccluderConstraint_didAvoidOccluder_forNode = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE scnAvoidOccluderConstraintDelegateDelegateClass #-}
scnAvoidOccluderConstraintDelegateDelegateClass :: Class
scnAvoidOccluderConstraintDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSCNAvoidOccluderConstraintDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_avoidOccluderConstraint_shouldAvoidOccluder_forNode = unSelector (mkSelector "avoidOccluderConstraint:shouldAvoidOccluder:forNode:")
      sel_avoidOccluderConstraint_didAvoidOccluder_forNode = unSelector (mkSelector "avoidOccluderConstraint:didAvoidOccluder:forNode:")
  -- avoidOccluderConstraint:shouldAvoidOccluder:forNode:
  stub_0 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAvoidOccluderConstraintDelegateOverrides
    case _avoidOccluderConstraint_shouldAvoidOccluder_forNode rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "avoidOccluderConstraint:shouldAvoidOccluder:forNode:" "B@:@@@" stub_0

  -- avoidOccluderConstraint:didAvoidOccluder:forNode:
  stub_1 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAvoidOccluderConstraintDelegateOverrides
    case _avoidOccluderConstraint_didAvoidOccluder_forNode rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "avoidOccluderConstraint:didAvoidOccluder:forNode:" "v@:@@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNAvoidOccluderConstraintDelegateOverrides
    if queriedSel == sel_avoidOccluderConstraint_shouldAvoidOccluder_forNode then pure (maybe 0 (const 1) (_avoidOccluderConstraint_shouldAvoidOccluder_forNode rec_))
    else if queriedSel == sel_avoidOccluderConstraint_didAvoidOccluder_forNode then pure (maybe 0 (const 1) (_avoidOccluderConstraint_didAvoidOccluder_forNode rec_))
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
newSCNAvoidOccluderConstraintDelegate :: SCNAvoidOccluderConstraintDelegateOverrides -> IO RawId
newSCNAvoidOccluderConstraintDelegate overrides = do
  inst <- class_createInstance scnAvoidOccluderConstraintDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
