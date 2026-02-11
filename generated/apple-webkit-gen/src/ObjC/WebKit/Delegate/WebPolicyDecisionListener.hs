{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol WebPolicyDecisionListener@.
--
-- Usage:
--
-- @
-- delegate <- newWebPolicyDecisionListener defaultWebPolicyDecisionListenerOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.WebKit.Delegate.WebPolicyDecisionListener
  ( WebPolicyDecisionListenerOverrides(..)
  , defaultWebPolicyDecisionListenerOverrides
  , newWebPolicyDecisionListener
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

-- | Overrides record for @\@protocol WebPolicyDecisionListener@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data WebPolicyDecisionListenerOverrides = WebPolicyDecisionListenerOverrides
  { _use :: !(Maybe (IO ()))
  , _download :: !(Maybe (IO ()))
  , _ignore :: !(Maybe (IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultWebPolicyDecisionListenerOverrides :: WebPolicyDecisionListenerOverrides
defaultWebPolicyDecisionListenerOverrides = WebPolicyDecisionListenerOverrides
  { _use = Nothing
  , _download = Nothing
  , _ignore = Nothing
  }

foreign import ccall "wrapper"
  wrap_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE webPolicyDecisionListenerDelegateClass #-}
webPolicyDecisionListenerDelegateClass :: Class
webPolicyDecisionListenerDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsWebPolicyDecisionListener" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_use = unSelector (mkSelector "use")
      sel_download = unSelector (mkSelector "download")
      sel_ignore = unSelector (mkSelector "ignore")
  -- use
  stub_0 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebPolicyDecisionListenerOverrides
    case _use rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "use" "v@:" stub_0

  -- download
  stub_1 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebPolicyDecisionListenerOverrides
    case _download rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "download" "v@:" stub_1

  -- ignore
  stub_2 <- wrap_v $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebPolicyDecisionListenerOverrides
    case _ignore rec_ of
      Nothing -> pure ()
      Just f -> f 
  addObjCMethod cls "ignore" "v@:" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO WebPolicyDecisionListenerOverrides
    if queriedSel == sel_use then pure (maybe 0 (const 1) (_use rec_))
    else if queriedSel == sel_download then pure (maybe 0 (const 1) (_download rec_))
    else if queriedSel == sel_ignore then pure (maybe 0 (const 1) (_ignore rec_))
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
newWebPolicyDecisionListener :: WebPolicyDecisionListenerOverrides -> IO RawId
newWebPolicyDecisionListener overrides = do
  inst <- class_createInstance webPolicyDecisionListenerDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
