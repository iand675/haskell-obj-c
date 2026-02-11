{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol INUIEditVoiceShortcutViewControllerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newINUIEditVoiceShortcutViewControllerDelegate defaultINUIEditVoiceShortcutViewControllerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.IntentsUI.Delegate.INUIEditVoiceShortcutViewControllerDelegate
  ( INUIEditVoiceShortcutViewControllerDelegateOverrides(..)
  , defaultINUIEditVoiceShortcutViewControllerDelegateOverrides
  , newINUIEditVoiceShortcutViewControllerDelegate
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

-- | Overrides record for @\@protocol INUIEditVoiceShortcutViewControllerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data INUIEditVoiceShortcutViewControllerDelegateOverrides = INUIEditVoiceShortcutViewControllerDelegateOverrides
  { _editVoiceShortcutViewController_didUpdateVoiceShortcut_error :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _editVoiceShortcutViewController_didDeleteVoiceShortcutWithIdentifier :: !(Maybe (RawId -> RawId -> IO ()))
  , _editVoiceShortcutViewControllerDidCancel :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultINUIEditVoiceShortcutViewControllerDelegateOverrides :: INUIEditVoiceShortcutViewControllerDelegateOverrides
defaultINUIEditVoiceShortcutViewControllerDelegateOverrides = INUIEditVoiceShortcutViewControllerDelegateOverrides
  { _editVoiceShortcutViewController_didUpdateVoiceShortcut_error = Nothing
  , _editVoiceShortcutViewController_didDeleteVoiceShortcutWithIdentifier = Nothing
  , _editVoiceShortcutViewControllerDidCancel = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE inuiEditVoiceShortcutViewControllerDelegateDelegateClass #-}
inuiEditVoiceShortcutViewControllerDelegateDelegateClass :: Class
inuiEditVoiceShortcutViewControllerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsINUIEditVoiceShortcutViewControllerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_editVoiceShortcutViewController_didUpdateVoiceShortcut_error = unSelector (mkSelector "editVoiceShortcutViewController:didUpdateVoiceShortcut:error:")
      sel_editVoiceShortcutViewController_didDeleteVoiceShortcutWithIdentifier = unSelector (mkSelector "editVoiceShortcutViewController:didDeleteVoiceShortcutWithIdentifier:")
      sel_editVoiceShortcutViewControllerDidCancel = unSelector (mkSelector "editVoiceShortcutViewControllerDidCancel:")
  -- editVoiceShortcutViewController:didUpdateVoiceShortcut:error:
  stub_0 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INUIEditVoiceShortcutViewControllerDelegateOverrides
    case _editVoiceShortcutViewController_didUpdateVoiceShortcut_error rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "editVoiceShortcutViewController:didUpdateVoiceShortcut:error:" "v@:@@@" stub_0

  -- editVoiceShortcutViewController:didDeleteVoiceShortcutWithIdentifier:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INUIEditVoiceShortcutViewControllerDelegateOverrides
    case _editVoiceShortcutViewController_didDeleteVoiceShortcutWithIdentifier rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "editVoiceShortcutViewController:didDeleteVoiceShortcutWithIdentifier:" "v@:@@" stub_1

  -- editVoiceShortcutViewControllerDidCancel:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INUIEditVoiceShortcutViewControllerDelegateOverrides
    case _editVoiceShortcutViewControllerDidCancel rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "editVoiceShortcutViewControllerDidCancel:" "v@:@" stub_2

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INUIEditVoiceShortcutViewControllerDelegateOverrides
    if queriedSel == sel_editVoiceShortcutViewController_didUpdateVoiceShortcut_error then pure (maybe 0 (const 1) (_editVoiceShortcutViewController_didUpdateVoiceShortcut_error rec_))
    else if queriedSel == sel_editVoiceShortcutViewController_didDeleteVoiceShortcutWithIdentifier then pure (maybe 0 (const 1) (_editVoiceShortcutViewController_didDeleteVoiceShortcutWithIdentifier rec_))
    else if queriedSel == sel_editVoiceShortcutViewControllerDidCancel then pure (maybe 0 (const 1) (_editVoiceShortcutViewControllerDidCancel rec_))
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
newINUIEditVoiceShortcutViewControllerDelegate :: INUIEditVoiceShortcutViewControllerDelegateOverrides -> IO RawId
newINUIEditVoiceShortcutViewControllerDelegate overrides = do
  inst <- class_createInstance inuiEditVoiceShortcutViewControllerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
