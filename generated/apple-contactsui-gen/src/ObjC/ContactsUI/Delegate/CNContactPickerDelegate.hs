{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol CNContactPickerDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newCNContactPickerDelegate defaultCNContactPickerDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.ContactsUI.Delegate.CNContactPickerDelegate
  ( CNContactPickerDelegateOverrides(..)
  , defaultCNContactPickerDelegateOverrides
  , newCNContactPickerDelegate
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

-- | Overrides record for @\@protocol CNContactPickerDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data CNContactPickerDelegateOverrides = CNContactPickerDelegateOverrides
  { _contactPicker_didSelectContact :: !(Maybe (RawId -> RawId -> IO ()))
  , _contactPicker_didSelectContactProperty :: !(Maybe (RawId -> RawId -> IO ()))
  , _contactPickerWillClose :: !(Maybe (RawId -> IO ()))
  , _contactPickerDidClose :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultCNContactPickerDelegateOverrides :: CNContactPickerDelegateOverrides
defaultCNContactPickerDelegateOverrides = CNContactPickerDelegateOverrides
  { _contactPicker_didSelectContact = Nothing
  , _contactPicker_didSelectContactProperty = Nothing
  , _contactPickerWillClose = Nothing
  , _contactPickerDidClose = Nothing
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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE cnContactPickerDelegateDelegateClass #-}
cnContactPickerDelegateDelegateClass :: Class
cnContactPickerDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsCNContactPickerDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_contactPicker_didSelectContact = unSelector (mkSelector "contactPicker:didSelectContact:")
      sel_contactPicker_didSelectContactProperty = unSelector (mkSelector "contactPicker:didSelectContactProperty:")
      sel_contactPickerWillClose = unSelector (mkSelector "contactPickerWillClose:")
      sel_contactPickerDidClose = unSelector (mkSelector "contactPickerDidClose:")
  -- contactPicker:didSelectContact:
  stub_0 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNContactPickerDelegateOverrides
    case _contactPicker_didSelectContact rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "contactPicker:didSelectContact:" "v@:@@" stub_0

  -- contactPicker:didSelectContactProperty:
  stub_1 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNContactPickerDelegateOverrides
    case _contactPicker_didSelectContactProperty rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "contactPicker:didSelectContactProperty:" "v@:@@" stub_1

  -- contactPickerWillClose:
  stub_2 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNContactPickerDelegateOverrides
    case _contactPickerWillClose rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "contactPickerWillClose:" "v@:@" stub_2

  -- contactPickerDidClose:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNContactPickerDelegateOverrides
    case _contactPickerDidClose rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "contactPickerDidClose:" "v@:@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO CNContactPickerDelegateOverrides
    if queriedSel == sel_contactPicker_didSelectContact then pure (maybe 0 (const 1) (_contactPicker_didSelectContact rec_))
    else if queriedSel == sel_contactPicker_didSelectContactProperty then pure (maybe 0 (const 1) (_contactPicker_didSelectContactProperty rec_))
    else if queriedSel == sel_contactPickerWillClose then pure (maybe 0 (const 1) (_contactPickerWillClose rec_))
    else if queriedSel == sel_contactPickerDidClose then pure (maybe 0 (const 1) (_contactPickerDidClose rec_))
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
newCNContactPickerDelegate :: CNContactPickerDelegateOverrides -> IO RawId
newCNContactPickerDelegate overrides = do
  inst <- class_createInstance cnContactPickerDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
