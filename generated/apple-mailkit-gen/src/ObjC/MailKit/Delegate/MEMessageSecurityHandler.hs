{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MEMessageSecurityHandler@.
--
-- Usage:
--
-- @
-- delegate <- newMEMessageSecurityHandler defaultMEMessageSecurityHandlerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MailKit.Delegate.MEMessageSecurityHandler
  ( MEMessageSecurityHandlerOverrides(..)
  , defaultMEMessageSecurityHandlerOverrides
  , newMEMessageSecurityHandler
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

-- | Overrides record for @\@protocol MEMessageSecurityHandler@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MEMessageSecurityHandlerOverrides = MEMessageSecurityHandlerOverrides
  { _extensionViewControllerForMessageSigners :: !(Maybe (RawId -> IO RawId))
  , _extensionViewControllerForMessageContext :: !(Maybe (RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMEMessageSecurityHandlerOverrides :: MEMessageSecurityHandlerOverrides
defaultMEMessageSecurityHandlerOverrides = MEMessageSecurityHandlerOverrides
  { _extensionViewControllerForMessageSigners = Nothing
  , _extensionViewControllerForMessageContext = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE meMessageSecurityHandlerDelegateClass #-}
meMessageSecurityHandlerDelegateClass :: Class
meMessageSecurityHandlerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMEMessageSecurityHandler" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_extensionViewControllerForMessageSigners = unSelector (mkSelector "extensionViewControllerForMessageSigners:")
      sel_extensionViewControllerForMessageContext = unSelector (mkSelector "extensionViewControllerForMessageContext:")
  -- extensionViewControllerForMessageSigners:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEMessageSecurityHandlerOverrides
    case _extensionViewControllerForMessageSigners rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "extensionViewControllerForMessageSigners:" "@@:@" stub_0

  -- extensionViewControllerForMessageContext:
  stub_1 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEMessageSecurityHandlerOverrides
    case _extensionViewControllerForMessageContext rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "extensionViewControllerForMessageContext:" "@@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEMessageSecurityHandlerOverrides
    if queriedSel == sel_extensionViewControllerForMessageSigners then pure (maybe 0 (const 1) (_extensionViewControllerForMessageSigners rec_))
    else if queriedSel == sel_extensionViewControllerForMessageContext then pure (maybe 0 (const 1) (_extensionViewControllerForMessageContext rec_))
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
newMEMessageSecurityHandler :: MEMessageSecurityHandlerOverrides -> IO RawId
newMEMessageSecurityHandler overrides = do
  inst <- class_createInstance meMessageSecurityHandlerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
