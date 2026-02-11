{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol MEExtension@.
--
-- Usage:
--
-- @
-- delegate <- newMEExtension defaultMEExtensionOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.MailKit.Delegate.MEExtension
  ( MEExtensionOverrides(..)
  , defaultMEExtensionOverrides
  , newMEExtension
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

-- | Overrides record for @\@protocol MEExtension@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data MEExtensionOverrides = MEExtensionOverrides
  { _handlerForComposeSession :: !(Maybe (RawId -> IO RawId))
  , _handlerForMessageActions :: !(Maybe (IO RawId))
  , _handlerForContentBlocker :: !(Maybe (IO RawId))
  , _handlerForMessageSecurity :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultMEExtensionOverrides :: MEExtensionOverrides
defaultMEExtensionOverrides = MEExtensionOverrides
  { _handlerForComposeSession = Nothing
  , _handlerForMessageActions = Nothing
  , _handlerForContentBlocker = Nothing
  , _handlerForMessageSecurity = Nothing
  }

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE meExtensionDelegateClass #-}
meExtensionDelegateClass :: Class
meExtensionDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsMEExtension" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_handlerForComposeSession = unSelector (mkSelector "handlerForComposeSession:")
      sel_handlerForMessageActions = unSelector (mkSelector "handlerForMessageActions")
      sel_handlerForContentBlocker = unSelector (mkSelector "handlerForContentBlocker")
      sel_handlerForMessageSecurity = unSelector (mkSelector "handlerForMessageSecurity")
  -- handlerForComposeSession:
  stub_0 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEExtensionOverrides
    case _handlerForComposeSession rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "handlerForComposeSession:" "@@:@" stub_0

  -- handlerForMessageActions
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEExtensionOverrides
    case _handlerForMessageActions rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "handlerForMessageActions" "@@:" stub_1

  -- handlerForContentBlocker
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEExtensionOverrides
    case _handlerForContentBlocker rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "handlerForContentBlocker" "@@:" stub_2

  -- handlerForMessageSecurity
  stub_3 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEExtensionOverrides
    case _handlerForMessageSecurity rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "handlerForMessageSecurity" "@@:" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO MEExtensionOverrides
    if queriedSel == sel_handlerForComposeSession then pure (maybe 0 (const 1) (_handlerForComposeSession rec_))
    else if queriedSel == sel_handlerForMessageActions then pure (maybe 0 (const 1) (_handlerForMessageActions rec_))
    else if queriedSel == sel_handlerForContentBlocker then pure (maybe 0 (const 1) (_handlerForContentBlocker rec_))
    else if queriedSel == sel_handlerForMessageSecurity then pure (maybe 0 (const 1) (_handlerForMessageSecurity rec_))
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
newMEExtension :: MEExtensionOverrides -> IO RawId
newMEExtension overrides = do
  inst <- class_createInstance meExtensionDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
