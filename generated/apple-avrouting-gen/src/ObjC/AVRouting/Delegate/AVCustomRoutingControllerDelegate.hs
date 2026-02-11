{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol AVCustomRoutingControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newAVCustomRoutingControllerDelegate defaultAVCustomRoutingControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AVRouting.Delegate.AVCustomRoutingControllerDelegate
  ( AVCustomRoutingControllerDelegateOverrides(..)
  , defaultAVCustomRoutingControllerDelegateOverrides
  , newAVCustomRoutingControllerDelegate
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

-- | Overrides record for @\@protocol AVCustomRoutingControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data AVCustomRoutingControllerDelegateOverrides = AVCustomRoutingControllerDelegateOverrides
  { _customRoutingController_eventDidTimeOut :: !(Maybe (RawId -> RawId -> IO ()))
  , _customRoutingController_didSelectItem :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultAVCustomRoutingControllerDelegateOverrides :: AVCustomRoutingControllerDelegateOverrides
defaultAVCustomRoutingControllerDelegateOverrides = AVCustomRoutingControllerDelegateOverrides
  { _customRoutingController_eventDidTimeOut = Nothing
  , _customRoutingController_didSelectItem = Nothing
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
{-# NOINLINE avCustomRoutingControllerDelegateDelegateClass #-}
avCustomRoutingControllerDelegateDelegateClass :: Class
avCustomRoutingControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsAVCustomRoutingControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_customRoutingController_eventDidTimeOut = unSelector (mkSelector "customRoutingController:eventDidTimeOut:")
      sel_customRoutingController_didSelectItem = unSelector (mkSelector "customRoutingController:didSelectItem:")
  -- customRoutingController:eventDidTimeOut:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCustomRoutingControllerDelegateOverrides
    case _customRoutingController_eventDidTimeOut rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "customRoutingController:eventDidTimeOut:" "v@:@@" stub_0

  -- customRoutingController:didSelectItem:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCustomRoutingControllerDelegateOverrides
    case _customRoutingController_didSelectItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "customRoutingController:didSelectItem:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO AVCustomRoutingControllerDelegateOverrides
    if queriedSel == sel_customRoutingController_eventDidTimeOut then pure (maybe 0 (const 1) (_customRoutingController_eventDidTimeOut rec_))
    else if queriedSel == sel_customRoutingController_didSelectItem then pure (maybe 0 (const 1) (_customRoutingController_didSelectItem rec_))
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
newAVCustomRoutingControllerDelegate :: AVCustomRoutingControllerDelegateOverrides -> IO RawId
newAVCustomRoutingControllerDelegate overrides = do
  inst <- class_createInstance avCustomRoutingControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
