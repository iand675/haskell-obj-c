{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol HKLiveWorkoutBuilderDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newHKLiveWorkoutBuilderDelegate defaultHKLiveWorkoutBuilderDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.HealthKit.Delegate.HKLiveWorkoutBuilderDelegate
  ( HKLiveWorkoutBuilderDelegateOverrides(..)
  , defaultHKLiveWorkoutBuilderDelegateOverrides
  , newHKLiveWorkoutBuilderDelegate
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

-- | Overrides record for @\@protocol HKLiveWorkoutBuilderDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data HKLiveWorkoutBuilderDelegateOverrides = HKLiveWorkoutBuilderDelegateOverrides
  { _workoutBuilder_didCollectDataOfTypes :: !(Maybe (RawId -> RawId -> IO ()))
  , _workoutBuilderDidCollectEvent :: !(Maybe (RawId -> IO ()))
  , _workoutBuilder_didBeginActivity :: !(Maybe (RawId -> RawId -> IO ()))
  , _workoutBuilder_didEndActivity :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultHKLiveWorkoutBuilderDelegateOverrides :: HKLiveWorkoutBuilderDelegateOverrides
defaultHKLiveWorkoutBuilderDelegateOverrides = HKLiveWorkoutBuilderDelegateOverrides
  { _workoutBuilder_didCollectDataOfTypes = Nothing
  , _workoutBuilderDidCollectEvent = Nothing
  , _workoutBuilder_didBeginActivity = Nothing
  , _workoutBuilder_didEndActivity = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE hkLiveWorkoutBuilderDelegateDelegateClass #-}
hkLiveWorkoutBuilderDelegateDelegateClass :: Class
hkLiveWorkoutBuilderDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsHKLiveWorkoutBuilderDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_workoutBuilder_didCollectDataOfTypes = unSelector (mkSelector "workoutBuilder:didCollectDataOfTypes:")
      sel_workoutBuilderDidCollectEvent = unSelector (mkSelector "workoutBuilderDidCollectEvent:")
      sel_workoutBuilder_didBeginActivity = unSelector (mkSelector "workoutBuilder:didBeginActivity:")
      sel_workoutBuilder_didEndActivity = unSelector (mkSelector "workoutBuilder:didEndActivity:")
  -- workoutBuilder:didCollectDataOfTypes:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO HKLiveWorkoutBuilderDelegateOverrides
    case _workoutBuilder_didCollectDataOfTypes rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "workoutBuilder:didCollectDataOfTypes:" "v@:@@" stub_0

  -- workoutBuilderDidCollectEvent:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO HKLiveWorkoutBuilderDelegateOverrides
    case _workoutBuilderDidCollectEvent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "workoutBuilderDidCollectEvent:" "v@:@" stub_1

  -- workoutBuilder:didBeginActivity:
  stub_2 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO HKLiveWorkoutBuilderDelegateOverrides
    case _workoutBuilder_didBeginActivity rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "workoutBuilder:didBeginActivity:" "v@:@@" stub_2

  -- workoutBuilder:didEndActivity:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO HKLiveWorkoutBuilderDelegateOverrides
    case _workoutBuilder_didEndActivity rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "workoutBuilder:didEndActivity:" "v@:@@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO HKLiveWorkoutBuilderDelegateOverrides
    if queriedSel == sel_workoutBuilder_didCollectDataOfTypes then pure (maybe 0 (const 1) (_workoutBuilder_didCollectDataOfTypes rec_))
    else if queriedSel == sel_workoutBuilderDidCollectEvent then pure (maybe 0 (const 1) (_workoutBuilderDidCollectEvent rec_))
    else if queriedSel == sel_workoutBuilder_didBeginActivity then pure (maybe 0 (const 1) (_workoutBuilder_didBeginActivity rec_))
    else if queriedSel == sel_workoutBuilder_didEndActivity then pure (maybe 0 (const 1) (_workoutBuilder_didEndActivity rec_))
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
newHKLiveWorkoutBuilderDelegate :: HKLiveWorkoutBuilderDelegateOverrides -> IO RawId
newHKLiveWorkoutBuilderDelegate overrides = do
  inst <- class_createInstance hkLiveWorkoutBuilderDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
