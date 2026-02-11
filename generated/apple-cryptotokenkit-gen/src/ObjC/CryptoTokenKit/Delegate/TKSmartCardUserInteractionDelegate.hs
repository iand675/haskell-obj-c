{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol TKSmartCardUserInteractionDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newTKSmartCardUserInteractionDelegate defaultTKSmartCardUserInteractionDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.CryptoTokenKit.Delegate.TKSmartCardUserInteractionDelegate
  ( TKSmartCardUserInteractionDelegateOverrides(..)
  , defaultTKSmartCardUserInteractionDelegateOverrides
  , newTKSmartCardUserInteractionDelegate
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

-- | Overrides record for @\@protocol TKSmartCardUserInteractionDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data TKSmartCardUserInteractionDelegateOverrides = TKSmartCardUserInteractionDelegateOverrides
  { _characterEnteredInUserInteraction :: !(Maybe (RawId -> IO ()))
  , _correctionKeyPressedInUserInteraction :: !(Maybe (RawId -> IO ()))
  , _validationKeyPressedInUserInteraction :: !(Maybe (RawId -> IO ()))
  , _invalidCharacterEnteredInUserInteraction :: !(Maybe (RawId -> IO ()))
  , _oldPINRequestedInUserInteraction :: !(Maybe (RawId -> IO ()))
  , _newPINRequestedInUserInteraction :: !(Maybe (RawId -> IO ()))
  , _newPINConfirmationRequestedInUserInteraction :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultTKSmartCardUserInteractionDelegateOverrides :: TKSmartCardUserInteractionDelegateOverrides
defaultTKSmartCardUserInteractionDelegateOverrides = TKSmartCardUserInteractionDelegateOverrides
  { _characterEnteredInUserInteraction = Nothing
  , _correctionKeyPressedInUserInteraction = Nothing
  , _validationKeyPressedInUserInteraction = Nothing
  , _invalidCharacterEnteredInUserInteraction = Nothing
  , _oldPINRequestedInUserInteraction = Nothing
  , _newPINRequestedInUserInteraction = Nothing
  , _newPINConfirmationRequestedInUserInteraction = Nothing
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
{-# NOINLINE tkSmartCardUserInteractionDelegateDelegateClass #-}
tkSmartCardUserInteractionDelegateDelegateClass :: Class
tkSmartCardUserInteractionDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsTKSmartCardUserInteractionDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_characterEnteredInUserInteraction = unSelector (mkSelector "characterEnteredInUserInteraction:")
      sel_correctionKeyPressedInUserInteraction = unSelector (mkSelector "correctionKeyPressedInUserInteraction:")
      sel_validationKeyPressedInUserInteraction = unSelector (mkSelector "validationKeyPressedInUserInteraction:")
      sel_invalidCharacterEnteredInUserInteraction = unSelector (mkSelector "invalidCharacterEnteredInUserInteraction:")
      sel_oldPINRequestedInUserInteraction = unSelector (mkSelector "oldPINRequestedInUserInteraction:")
      sel_newPINRequestedInUserInteraction = unSelector (mkSelector "newPINRequestedInUserInteraction:")
      sel_newPINConfirmationRequestedInUserInteraction = unSelector (mkSelector "newPINConfirmationRequestedInUserInteraction:")
  -- characterEnteredInUserInteraction:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKSmartCardUserInteractionDelegateOverrides
    case _characterEnteredInUserInteraction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "characterEnteredInUserInteraction:" "v@:@" stub_0

  -- correctionKeyPressedInUserInteraction:
  stub_1 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKSmartCardUserInteractionDelegateOverrides
    case _correctionKeyPressedInUserInteraction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "correctionKeyPressedInUserInteraction:" "v@:@" stub_1

  -- validationKeyPressedInUserInteraction:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKSmartCardUserInteractionDelegateOverrides
    case _validationKeyPressedInUserInteraction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "validationKeyPressedInUserInteraction:" "v@:@" stub_2

  -- invalidCharacterEnteredInUserInteraction:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKSmartCardUserInteractionDelegateOverrides
    case _invalidCharacterEnteredInUserInteraction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "invalidCharacterEnteredInUserInteraction:" "v@:@" stub_3

  -- oldPINRequestedInUserInteraction:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKSmartCardUserInteractionDelegateOverrides
    case _oldPINRequestedInUserInteraction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "oldPINRequestedInUserInteraction:" "v@:@" stub_4

  -- newPINRequestedInUserInteraction:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKSmartCardUserInteractionDelegateOverrides
    case _newPINRequestedInUserInteraction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "newPINRequestedInUserInteraction:" "v@:@" stub_5

  -- newPINConfirmationRequestedInUserInteraction:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKSmartCardUserInteractionDelegateOverrides
    case _newPINConfirmationRequestedInUserInteraction rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "newPINConfirmationRequestedInUserInteraction:" "v@:@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO TKSmartCardUserInteractionDelegateOverrides
    if queriedSel == sel_characterEnteredInUserInteraction then pure (maybe 0 (const 1) (_characterEnteredInUserInteraction rec_))
    else if queriedSel == sel_correctionKeyPressedInUserInteraction then pure (maybe 0 (const 1) (_correctionKeyPressedInUserInteraction rec_))
    else if queriedSel == sel_validationKeyPressedInUserInteraction then pure (maybe 0 (const 1) (_validationKeyPressedInUserInteraction rec_))
    else if queriedSel == sel_invalidCharacterEnteredInUserInteraction then pure (maybe 0 (const 1) (_invalidCharacterEnteredInUserInteraction rec_))
    else if queriedSel == sel_oldPINRequestedInUserInteraction then pure (maybe 0 (const 1) (_oldPINRequestedInUserInteraction rec_))
    else if queriedSel == sel_newPINRequestedInUserInteraction then pure (maybe 0 (const 1) (_newPINRequestedInUserInteraction rec_))
    else if queriedSel == sel_newPINConfirmationRequestedInUserInteraction then pure (maybe 0 (const 1) (_newPINConfirmationRequestedInUserInteraction rec_))
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
newTKSmartCardUserInteractionDelegate :: TKSmartCardUserInteractionDelegateOverrides -> IO RawId
newTKSmartCardUserInteractionDelegate overrides = do
  inst <- class_createInstance tkSmartCardUserInteractionDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
