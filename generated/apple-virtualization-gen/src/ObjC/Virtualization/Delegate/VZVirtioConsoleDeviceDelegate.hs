{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol VZVirtioConsoleDeviceDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newVZVirtioConsoleDeviceDelegate defaultVZVirtioConsoleDeviceDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Virtualization.Delegate.VZVirtioConsoleDeviceDelegate
  ( VZVirtioConsoleDeviceDelegateOverrides(..)
  , defaultVZVirtioConsoleDeviceDelegateOverrides
  , newVZVirtioConsoleDeviceDelegate
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

-- | Overrides record for @\@protocol VZVirtioConsoleDeviceDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data VZVirtioConsoleDeviceDelegateOverrides = VZVirtioConsoleDeviceDelegateOverrides
  { _consoleDevice_didOpenPort :: !(Maybe (RawId -> RawId -> IO ()))
  , _consoleDevice_didClosePort :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultVZVirtioConsoleDeviceDelegateOverrides :: VZVirtioConsoleDeviceDelegateOverrides
defaultVZVirtioConsoleDeviceDelegateOverrides = VZVirtioConsoleDeviceDelegateOverrides
  { _consoleDevice_didOpenPort = Nothing
  , _consoleDevice_didClosePort = Nothing
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
{-# NOINLINE vzVirtioConsoleDeviceDelegateDelegateClass #-}
vzVirtioConsoleDeviceDelegateDelegateClass :: Class
vzVirtioConsoleDeviceDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsVZVirtioConsoleDeviceDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_consoleDevice_didOpenPort = unSelector (mkSelector "consoleDevice:didOpenPort:")
      sel_consoleDevice_didClosePort = unSelector (mkSelector "consoleDevice:didClosePort:")
  -- consoleDevice:didOpenPort:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VZVirtioConsoleDeviceDelegateOverrides
    case _consoleDevice_didOpenPort rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "consoleDevice:didOpenPort:" "v@:@@" stub_0

  -- consoleDevice:didClosePort:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VZVirtioConsoleDeviceDelegateOverrides
    case _consoleDevice_didClosePort rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "consoleDevice:didClosePort:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO VZVirtioConsoleDeviceDelegateOverrides
    if queriedSel == sel_consoleDevice_didOpenPort then pure (maybe 0 (const 1) (_consoleDevice_didOpenPort rec_))
    else if queriedSel == sel_consoleDevice_didClosePort then pure (maybe 0 (const 1) (_consoleDevice_didClosePort rec_))
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
newVZVirtioConsoleDeviceDelegate :: VZVirtioConsoleDeviceDelegateOverrides -> IO RawId
newVZVirtioConsoleDeviceDelegate overrides = do
  inst <- class_createInstance vzVirtioConsoleDeviceDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
