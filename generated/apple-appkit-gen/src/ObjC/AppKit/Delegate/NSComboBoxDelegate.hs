{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSComboBoxDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSComboBoxDelegate defaultNSComboBoxDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSComboBoxDelegate
  ( NSComboBoxDelegateOverrides(..)
  , defaultNSComboBoxDelegateOverrides
  , newNSComboBoxDelegate
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

-- | Overrides record for @\@protocol NSComboBoxDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSComboBoxDelegateOverrides = NSComboBoxDelegateOverrides
  { _comboBoxWillPopUp :: !(Maybe (RawId -> IO ()))
  , _comboBoxWillDismiss :: !(Maybe (RawId -> IO ()))
  , _comboBoxSelectionDidChange :: !(Maybe (RawId -> IO ()))
  , _comboBoxSelectionIsChanging :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSComboBoxDelegateOverrides :: NSComboBoxDelegateOverrides
defaultNSComboBoxDelegateOverrides = NSComboBoxDelegateOverrides
  { _comboBoxWillPopUp = Nothing
  , _comboBoxWillDismiss = Nothing
  , _comboBoxSelectionDidChange = Nothing
  , _comboBoxSelectionIsChanging = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsComboBoxDelegateDelegateClass #-}
nsComboBoxDelegateDelegateClass :: Class
nsComboBoxDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSComboBoxDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_comboBoxWillPopUp = unSelector (mkSelector "comboBoxWillPopUp:")
      sel_comboBoxWillDismiss = unSelector (mkSelector "comboBoxWillDismiss:")
      sel_comboBoxSelectionDidChange = unSelector (mkSelector "comboBoxSelectionDidChange:")
      sel_comboBoxSelectionIsChanging = unSelector (mkSelector "comboBoxSelectionIsChanging:")
  -- comboBoxWillPopUp:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxDelegateOverrides
    case _comboBoxWillPopUp rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "comboBoxWillPopUp:" "v@:@" stub_0

  -- comboBoxWillDismiss:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxDelegateOverrides
    case _comboBoxWillDismiss rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "comboBoxWillDismiss:" "v@:@" stub_1

  -- comboBoxSelectionDidChange:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxDelegateOverrides
    case _comboBoxSelectionDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "comboBoxSelectionDidChange:" "v@:@" stub_2

  -- comboBoxSelectionIsChanging:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxDelegateOverrides
    case _comboBoxSelectionIsChanging rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "comboBoxSelectionIsChanging:" "v@:@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxDelegateOverrides
    if queriedSel == sel_comboBoxWillPopUp then pure (maybe 0 (const 1) (_comboBoxWillPopUp rec_))
    else if queriedSel == sel_comboBoxWillDismiss then pure (maybe 0 (const 1) (_comboBoxWillDismiss rec_))
    else if queriedSel == sel_comboBoxSelectionDidChange then pure (maybe 0 (const 1) (_comboBoxSelectionDidChange rec_))
    else if queriedSel == sel_comboBoxSelectionIsChanging then pure (maybe 0 (const 1) (_comboBoxSelectionIsChanging rec_))
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
newNSComboBoxDelegate :: NSComboBoxDelegateOverrides -> IO RawId
newNSComboBoxDelegate overrides = do
  inst <- class_createInstance nsComboBoxDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
