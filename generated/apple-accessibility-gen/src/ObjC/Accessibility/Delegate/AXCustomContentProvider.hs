{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AXCustomContentProvider@.
--
-- Usage:
--
-- @
-- delegate <- newAXCustomContentProvider defaultAXCustomContentProviderOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.Accessibility.Delegate.AXCustomContentProvider
  ( AXCustomContentProviderOverrides(..)
  , defaultAXCustomContentProviderOverrides
  , newAXCustomContentProvider
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

-- | Overrides record for @\@protocol AXCustomContentProvider@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AXCustomContentProviderOverrides = AXCustomContentProviderOverrides
  { _accessibilityCustomContent :: !(Maybe (IO RawId))
  , _setAccessibilityCustomContent :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAXCustomContentProviderOverrides :: AXCustomContentProviderOverrides
defaultAXCustomContentProviderOverrides = AXCustomContentProviderOverrides
  { _accessibilityCustomContent = Nothing
  , _setAccessibilityCustomContent = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE axCustomContentProviderDelegateClass #-}
axCustomContentProviderDelegateClass :: Class
axCustomContentProviderDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAXCustomContentProvider" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessibilityCustomContent = unSelector (mkSelector "accessibilityCustomContent")
      sel_setAccessibilityCustomContent = unSelector (mkSelector "setAccessibilityCustomContent:")
  -- accessibilityCustomContent
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AXCustomContentProviderOverrides
    case _accessibilityCustomContent rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityCustomContent" "@@:" stub_0

  -- setAccessibilityCustomContent:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AXCustomContentProviderOverrides
    case _setAccessibilityCustomContent rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilityCustomContent:" "v@:@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AXCustomContentProviderOverrides
    if queriedSel == sel_accessibilityCustomContent then pure (maybe 0 (const 1) (_accessibilityCustomContent rec_))
    else if queriedSel == sel_setAccessibilityCustomContent then pure (maybe 0 (const 1) (_setAccessibilityCustomContent rec_))
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
newAXCustomContentProvider :: AXCustomContentProviderOverrides -> IO RawId
newAXCustomContentProvider overrides = do
  inst <- class_createInstance axCustomContentProviderDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
