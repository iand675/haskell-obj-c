{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SCNBoundingVolume@.
--
-- Usage:
--
-- @
-- delegate <- newSCNBoundingVolume defaultSCNBoundingVolumeOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SceneKit.Delegate.SCNBoundingVolume
  ( SCNBoundingVolumeOverrides(..)
  , defaultSCNBoundingVolumeOverrides
  , newSCNBoundingVolume
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

-- | Overrides record for @\@protocol SCNBoundingVolume@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SCNBoundingVolumeOverrides = SCNBoundingVolumeOverrides
  { _getBoundingBoxMin_max :: !(Maybe (RawId -> RawId -> IO Bool))
  , _setBoundingBoxMin_max :: !(Maybe (RawId -> RawId -> IO ()))
  , _getBoundingSphereCenter_radius :: !(Maybe (RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultSCNBoundingVolumeOverrides :: SCNBoundingVolumeOverrides
defaultSCNBoundingVolumeOverrides = SCNBoundingVolumeOverrides
  { _getBoundingBoxMin_max = Nothing
  , _setBoundingBoxMin_max = Nothing
  , _getBoundingSphereCenter_radius = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE scnBoundingVolumeDelegateClass #-}
scnBoundingVolumeDelegateClass :: Class
scnBoundingVolumeDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSCNBoundingVolume" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_getBoundingBoxMin_max = unSelector (mkSelector "getBoundingBoxMin:max:")
      sel_setBoundingBoxMin_max = unSelector (mkSelector "setBoundingBoxMin:max:")
      sel_getBoundingSphereCenter_radius = unSelector (mkSelector "getBoundingSphereCenter:radius:")
  -- getBoundingBoxMin:max:
  stub_0 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNBoundingVolumeOverrides
    case _getBoundingBoxMin_max rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "getBoundingBoxMin:max:" "B@:@@" stub_0

  -- setBoundingBoxMin:max:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNBoundingVolumeOverrides
    case _setBoundingBoxMin_max rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "setBoundingBoxMin:max:" "v@:@@" stub_1

  -- getBoundingSphereCenter:radius:
  stub_2 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNBoundingVolumeOverrides
    case _getBoundingSphereCenter_radius rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "getBoundingSphereCenter:radius:" "B@:@@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNBoundingVolumeOverrides
    if queriedSel == sel_getBoundingBoxMin_max then pure (maybe 0 (const 1) (_getBoundingBoxMin_max rec_))
    else if queriedSel == sel_setBoundingBoxMin_max then pure (maybe 0 (const 1) (_setBoundingBoxMin_max rec_))
    else if queriedSel == sel_getBoundingSphereCenter_radius then pure (maybe 0 (const 1) (_getBoundingSphereCenter_radius rec_))
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
newSCNBoundingVolume :: SCNBoundingVolumeOverrides -> IO RawId
newSCNBoundingVolume overrides = do
  inst <- class_createInstance scnBoundingVolumeDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
