{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol INUIAddVoiceShortcutButtonDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newINUIAddVoiceShortcutButtonDelegate defaultINUIAddVoiceShortcutButtonDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.IntentsUI.Delegate.INUIAddVoiceShortcutButtonDelegate
  ( INUIAddVoiceShortcutButtonDelegateOverrides(..)
  , defaultINUIAddVoiceShortcutButtonDelegateOverrides
  , newINUIAddVoiceShortcutButtonDelegate
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

-- | Overrides record for @\@protocol INUIAddVoiceShortcutButtonDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data INUIAddVoiceShortcutButtonDelegateOverrides = INUIAddVoiceShortcutButtonDelegateOverrides
  { _presentAddVoiceShortcutViewController_forAddVoiceShortcutButton :: !(Maybe (RawId -> RawId -> IO ()))
  , _presentEditVoiceShortcutViewController_forAddVoiceShortcutButton :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultINUIAddVoiceShortcutButtonDelegateOverrides :: INUIAddVoiceShortcutButtonDelegateOverrides
defaultINUIAddVoiceShortcutButtonDelegateOverrides = INUIAddVoiceShortcutButtonDelegateOverrides
  { _presentAddVoiceShortcutViewController_forAddVoiceShortcutButton = Nothing
  , _presentEditVoiceShortcutViewController_forAddVoiceShortcutButton = Nothing
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
{-# NOINLINE inuiAddVoiceShortcutButtonDelegateDelegateClass #-}
inuiAddVoiceShortcutButtonDelegateDelegateClass :: Class
inuiAddVoiceShortcutButtonDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsINUIAddVoiceShortcutButtonDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_presentAddVoiceShortcutViewController_forAddVoiceShortcutButton = unSelector (mkSelector "presentAddVoiceShortcutViewController:forAddVoiceShortcutButton:")
      sel_presentEditVoiceShortcutViewController_forAddVoiceShortcutButton = unSelector (mkSelector "presentEditVoiceShortcutViewController:forAddVoiceShortcutButton:")
  -- presentAddVoiceShortcutViewController:forAddVoiceShortcutButton:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INUIAddVoiceShortcutButtonDelegateOverrides
    case _presentAddVoiceShortcutViewController_forAddVoiceShortcutButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "presentAddVoiceShortcutViewController:forAddVoiceShortcutButton:" "v@:@@" stub_0

  -- presentEditVoiceShortcutViewController:forAddVoiceShortcutButton:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INUIAddVoiceShortcutButtonDelegateOverrides
    case _presentEditVoiceShortcutViewController_forAddVoiceShortcutButton rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "presentEditVoiceShortcutViewController:forAddVoiceShortcutButton:" "v@:@@" stub_1

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO INUIAddVoiceShortcutButtonDelegateOverrides
    if queriedSel == sel_presentAddVoiceShortcutViewController_forAddVoiceShortcutButton then pure (maybe 0 (const 1) (_presentAddVoiceShortcutViewController_forAddVoiceShortcutButton rec_))
    else if queriedSel == sel_presentEditVoiceShortcutViewController_forAddVoiceShortcutButton then pure (maybe 0 (const 1) (_presentEditVoiceShortcutViewController_forAddVoiceShortcutButton rec_))
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
newINUIAddVoiceShortcutButtonDelegate :: INUIAddVoiceShortcutButtonDelegateOverrides -> IO RawId
newINUIAddVoiceShortcutButtonDelegate overrides = do
  inst <- class_createInstance inuiAddVoiceShortcutButtonDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
