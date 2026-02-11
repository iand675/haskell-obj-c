{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSViewLayerContentScaleDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSViewLayerContentScaleDelegate defaultNSViewLayerContentScaleDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSViewLayerContentScaleDelegate
  ( NSViewLayerContentScaleDelegateOverrides(..)
  , defaultNSViewLayerContentScaleDelegateOverrides
  , newNSViewLayerContentScaleDelegate
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

-- | Overrides record for @\@protocol NSViewLayerContentScaleDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSViewLayerContentScaleDelegateOverrides = NSViewLayerContentScaleDelegateOverrides
  { _layer_shouldInheritContentsScale_fromWindow :: !(Maybe (RawId -> Double -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSViewLayerContentScaleDelegateOverrides :: NSViewLayerContentScaleDelegateOverrides
defaultNSViewLayerContentScaleDelegateOverrides = NSViewLayerContentScaleDelegateOverrides
  { _layer_shouldInheritContentsScale_fromWindow = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_d_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CDouble -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsViewLayerContentScaleDelegateDelegateClass #-}
nsViewLayerContentScaleDelegateDelegateClass :: Class
nsViewLayerContentScaleDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSViewLayerContentScaleDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_layer_shouldInheritContentsScale_fromWindow = unSelector (mkSelector "layer:shouldInheritContentsScale:fromWindow:")
  -- layer:shouldInheritContentsScale:fromWindow:
  stub_0 <- wrap_at_d_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSViewLayerContentScaleDelegateOverrides
    case _layer_shouldInheritContentsScale_fromWindow rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (realToFrac arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "layer:shouldInheritContentsScale:fromWindow:" "B@:@d@" stub_0

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSViewLayerContentScaleDelegateOverrides
    if queriedSel == sel_layer_shouldInheritContentsScale_fromWindow then pure (maybe 0 (const 1) (_layer_shouldInheritContentsScale_fromWindow rec_))
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
newNSViewLayerContentScaleDelegate :: NSViewLayerContentScaleDelegateOverrides -> IO RawId
newNSViewLayerContentScaleDelegate overrides = do
  inst <- class_createInstance nsViewLayerContentScaleDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
