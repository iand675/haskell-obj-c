{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NCWidgetProviding@.
--
-- Usage:
--
-- @
-- delegate <- newNCWidgetProviding defaultNCWidgetProvidingOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.NotificationCenter.Delegate.NCWidgetProviding
  ( NCWidgetProvidingOverrides(..)
  , defaultNCWidgetProvidingOverrides
  , newNCWidgetProviding
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

-- | Overrides record for @\@protocol NCWidgetProviding@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NCWidgetProvidingOverrides = NCWidgetProvidingOverrides
  { _widgetDidBeginEditing :: !(Maybe (IO ()))
  , _widgetDidEndEditing :: !(Maybe (IO ()))
  , _widgetAllowsEditing :: !(Maybe (IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNCWidgetProvidingOverrides :: NCWidgetProvidingOverrides
defaultNCWidgetProvidingOverrides = NCWidgetProvidingOverrides
  { _widgetDidBeginEditing = Nothing
  , _widgetDidEndEditing = Nothing
  , _widgetAllowsEditing = Nothing
  }

foreign import ccall "wrapper"
  wrap_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO CULong))

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE ncWidgetProvidingDelegateClass #-}
ncWidgetProvidingDelegateClass :: Class
ncWidgetProvidingDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNCWidgetProviding" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_widgetDidBeginEditing = unSelector (mkSelector "widgetDidBeginEditing")
      sel_widgetDidEndEditing = unSelector (mkSelector "widgetDidEndEditing")
      sel_widgetAllowsEditing = unSelector (mkSelector "widgetAllowsEditing")
  -- widgetDidBeginEditing
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetProvidingOverrides
    case _widgetDidBeginEditing rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "widgetDidBeginEditing" "v@:" stub_0

  -- widgetDidEndEditing
  stub_1 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetProvidingOverrides
    case _widgetDidEndEditing rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "widgetDidEndEditing" "v@:" stub_1

  -- widgetAllowsEditing
  stub_2 <- wrap_B $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetProvidingOverrides
    case _widgetAllowsEditing rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f 
        pure (if r then 1 else 0)
  addObjCMethod cls "widgetAllowsEditing" "B@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NCWidgetProvidingOverrides
    if queriedSel == sel_widgetDidBeginEditing then pure (maybe 0 (const 1) (_widgetDidBeginEditing rec_))
    else if queriedSel == sel_widgetDidEndEditing then pure (maybe 0 (const 1) (_widgetDidEndEditing rec_))
    else if queriedSel == sel_widgetAllowsEditing then pure (maybe 0 (const 1) (_widgetAllowsEditing rec_))
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
newNCWidgetProviding :: NCWidgetProvidingOverrides -> IO RawId
newNCWidgetProviding overrides = do
  inst <- class_createInstance ncWidgetProvidingDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
