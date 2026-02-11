{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MDLLightProbeIrradianceDataSource@.
--
-- Usage:
--
-- @
-- delegate <- newMDLLightProbeIrradianceDataSource defaultMDLLightProbeIrradianceDataSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ModelIO.Delegate.MDLLightProbeIrradianceDataSource
  ( MDLLightProbeIrradianceDataSourceOverrides(..)
  , defaultMDLLightProbeIrradianceDataSourceOverrides
  , newMDLLightProbeIrradianceDataSource
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

-- | Overrides record for @\@protocol MDLLightProbeIrradianceDataSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MDLLightProbeIrradianceDataSourceOverrides = MDLLightProbeIrradianceDataSourceOverrides
  { _sphericalHarmonicsLevel :: !(Maybe (IO Int))
  , _setSphericalHarmonicsLevel :: !(Maybe (Int -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultMDLLightProbeIrradianceDataSourceOverrides :: MDLLightProbeIrradianceDataSourceOverrides
defaultMDLLightProbeIrradianceDataSourceOverrides = MDLLightProbeIrradianceDataSourceOverrides
  { _sphericalHarmonicsLevel = Nothing
  , _setSphericalHarmonicsLevel = Nothing
  }

foreign import ccall "wrapper"
  wrap_Q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> CULong -> IO ()))

foreign import ccall "wrapper"
  wrap_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE mdlLightProbeIrradianceDataSourceDelegateClass #-}
mdlLightProbeIrradianceDataSourceDelegateClass :: Class
mdlLightProbeIrradianceDataSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMDLLightProbeIrradianceDataSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_sphericalHarmonicsLevel = unSelector (mkSelector "sphericalHarmonicsLevel")
      sel_setSphericalHarmonicsLevel = unSelector (mkSelector "setSphericalHarmonicsLevel:")
  -- sphericalHarmonicsLevel
  stub_0 <- wrap_Q $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLLightProbeIrradianceDataSourceOverrides
    case _sphericalHarmonicsLevel rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "sphericalHarmonicsLevel" "Q@:" stub_0

  -- setSphericalHarmonicsLevel:
  stub_1 <- wrap_Q_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLLightProbeIrradianceDataSourceOverrides
    case _setSphericalHarmonicsLevel rec_ of
      Nothing -> pure ()
      Just f -> f (fromIntegral arg0)
  addObjCMethod cls "setSphericalHarmonicsLevel:" "v@:Q" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MDLLightProbeIrradianceDataSourceOverrides
    if queriedSel == sel_sphericalHarmonicsLevel then pure (maybe 0 (const 1) (_sphericalHarmonicsLevel rec_))
    else if queriedSel == sel_setSphericalHarmonicsLevel then pure (maybe 0 (const 1) (_setSphericalHarmonicsLevel rec_))
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
newMDLLightProbeIrradianceDataSource :: MDLLightProbeIrradianceDataSourceOverrides -> IO RawId
newMDLLightProbeIrradianceDataSource overrides = do
  inst <- class_createInstance mdlLightProbeIrradianceDataSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
