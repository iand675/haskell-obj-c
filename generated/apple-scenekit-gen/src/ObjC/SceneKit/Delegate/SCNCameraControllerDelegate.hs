{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol SCNCameraControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newSCNCameraControllerDelegate defaultSCNCameraControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.SceneKit.Delegate.SCNCameraControllerDelegate
  ( SCNCameraControllerDelegateOverrides(..)
  , defaultSCNCameraControllerDelegateOverrides
  , newSCNCameraControllerDelegate
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

-- | Overrides record for @\@protocol SCNCameraControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data SCNCameraControllerDelegateOverrides = SCNCameraControllerDelegateOverrides
  { _cameraInertiaWillStartForController :: !(Maybe (RawId -> IO ()))
  , _cameraInertiaDidEndForController :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultSCNCameraControllerDelegateOverrides :: SCNCameraControllerDelegateOverrides
defaultSCNCameraControllerDelegateOverrides = SCNCameraControllerDelegateOverrides
  { _cameraInertiaWillStartForController = Nothing
  , _cameraInertiaDidEndForController = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE scnCameraControllerDelegateDelegateClass #-}
scnCameraControllerDelegateDelegateClass :: Class
scnCameraControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsSCNCameraControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_cameraInertiaWillStartForController = unSelector (mkSelector "cameraInertiaWillStartForController:")
      sel_cameraInertiaDidEndForController = unSelector (mkSelector "cameraInertiaDidEndForController:")
  -- cameraInertiaWillStartForController:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControllerDelegateOverrides
    case _cameraInertiaWillStartForController rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "cameraInertiaWillStartForController:" "v@:@" stub_0

  -- cameraInertiaDidEndForController:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControllerDelegateOverrides
    case _cameraInertiaDidEndForController rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "cameraInertiaDidEndForController:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO SCNCameraControllerDelegateOverrides
    if queriedSel == sel_cameraInertiaWillStartForController then pure (maybe 0 (const 1) (_cameraInertiaWillStartForController rec_))
    else if queriedSel == sel_cameraInertiaDidEndForController then pure (maybe 0 (const 1) (_cameraInertiaDidEndForController rec_))
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
newSCNCameraControllerDelegate :: SCNCameraControllerDelegateOverrides -> IO RawId
newSCNCameraControllerDelegate overrides = do
  inst <- class_createInstance scnCameraControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
