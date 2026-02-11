{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol JRSRemoteLayer@.
--
-- Usage:
--
-- @
-- delegate <- newJRSRemoteLayer defaultJRSRemoteLayerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.JavaRuntimeSupport.Delegate.JRSRemoteLayer
  ( JRSRemoteLayerOverrides(..)
  , defaultJRSRemoteLayerOverrides
  , newJRSRemoteLayer
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

-- | Overrides record for @\@protocol JRSRemoteLayer@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data JRSRemoteLayerOverrides = JRSRemoteLayerOverrides
  { _layerID :: !(Maybe (IO Int))
  }

-- | Default overrides with all methods unimplemented.
defaultJRSRemoteLayerOverrides :: JRSRemoteLayerOverrides
defaultJRSRemoteLayerOverrides = JRSRemoteLayerOverrides
  { _layerID = Nothing
  }

foreign import ccall "wrapper"
  wrap_I
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CUInt)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CUInt))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE jrsRemoteLayerDelegateClass #-}
jrsRemoteLayerDelegateClass :: Class
jrsRemoteLayerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsJRSRemoteLayer" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_layerID = unSelector (mkSelector "layerID")
  -- layerID
  stub_0 <- wrap_I $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO JRSRemoteLayerOverrides
    case _layerID rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (fromIntegral r)
  addObjCMethod cls "layerID" "I@:" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO JRSRemoteLayerOverrides
    if queriedSel == sel_layerID then pure (maybe 0 (const 1) (_layerID rec_))
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
newJRSRemoteLayer :: JRSRemoteLayerOverrides -> IO RawId
newJRSRemoteLayer overrides = do
  inst <- class_createInstance jrsRemoteLayerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
