{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTokenFieldDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSTokenFieldDelegate defaultNSTokenFieldDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTokenFieldDelegate
  ( NSTokenFieldDelegateOverrides(..)
  , defaultNSTokenFieldDelegateOverrides
  , newNSTokenFieldDelegate
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

-- | Overrides record for @\@protocol NSTokenFieldDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTokenFieldDelegateOverrides = NSTokenFieldDelegateOverrides
  { _tokenField_completionsForSubstring_indexOfToken_indexOfSelectedItem :: !(Maybe (RawId -> RawId -> Int -> RawId -> IO RawId))
  , _tokenField_shouldAddObjects_atIndex :: !(Maybe (RawId -> RawId -> Int -> IO RawId))
  , _tokenField_displayStringForRepresentedObject :: !(Maybe (RawId -> RawId -> IO RawId))
  , _tokenField_editingStringForRepresentedObject :: !(Maybe (RawId -> RawId -> IO RawId))
  , _tokenField_representedObjectForEditingString :: !(Maybe (RawId -> RawId -> IO RawId))
  , _tokenField_writeRepresentedObjects_toPasteboard :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _tokenField_readFromPasteboard :: !(Maybe (RawId -> RawId -> IO RawId))
  , _tokenField_menuForRepresentedObject :: !(Maybe (RawId -> RawId -> IO RawId))
  , _tokenField_hasMenuForRepresentedObject :: !(Maybe (RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTokenFieldDelegateOverrides :: NSTokenFieldDelegateOverrides
defaultNSTokenFieldDelegateOverrides = NSTokenFieldDelegateOverrides
  { _tokenField_completionsForSubstring_indexOfToken_indexOfSelectedItem = Nothing
  , _tokenField_shouldAddObjects_atIndex = Nothing
  , _tokenField_displayStringForRepresentedObject = Nothing
  , _tokenField_editingStringForRepresentedObject = Nothing
  , _tokenField_representedObjectForEditingString = Nothing
  , _tokenField_writeRepresentedObjects_toPasteboard = Nothing
  , _tokenField_readFromPasteboard = Nothing
  , _tokenField_menuForRepresentedObject = Nothing
  , _tokenField_hasMenuForRepresentedObject = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_Q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_q_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTokenFieldDelegateDelegateClass #-}
nsTokenFieldDelegateDelegateClass :: Class
nsTokenFieldDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTokenFieldDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_tokenField_completionsForSubstring_indexOfToken_indexOfSelectedItem = unSelector (mkSelector "tokenField:completionsForSubstring:indexOfToken:indexOfSelectedItem:")
      sel_tokenField_shouldAddObjects_atIndex = unSelector (mkSelector "tokenField:shouldAddObjects:atIndex:")
      sel_tokenField_displayStringForRepresentedObject = unSelector (mkSelector "tokenField:displayStringForRepresentedObject:")
      sel_tokenField_editingStringForRepresentedObject = unSelector (mkSelector "tokenField:editingStringForRepresentedObject:")
      sel_tokenField_representedObjectForEditingString = unSelector (mkSelector "tokenField:representedObjectForEditingString:")
      sel_tokenField_writeRepresentedObjects_toPasteboard = unSelector (mkSelector "tokenField:writeRepresentedObjects:toPasteboard:")
      sel_tokenField_readFromPasteboard = unSelector (mkSelector "tokenField:readFromPasteboard:")
      sel_tokenField_menuForRepresentedObject = unSelector (mkSelector "tokenField:menuForRepresentedObject:")
      sel_tokenField_hasMenuForRepresentedObject = unSelector (mkSelector "tokenField:hasMenuForRepresentedObject:")
  -- tokenField:completionsForSubstring:indexOfToken:indexOfSelectedItem:
  stub_0 <- wrap_at_at_q_at_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldDelegateOverrides
    case _tokenField_completionsForSubstring_indexOfToken_indexOfSelectedItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2) (RawId arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenField:completionsForSubstring:indexOfToken:indexOfSelectedItem:" "@@:@@q@" stub_0

  -- tokenField:shouldAddObjects:atIndex:
  stub_1 <- wrap_at_at_Q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldDelegateOverrides
    case _tokenField_shouldAddObjects_atIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenField:shouldAddObjects:atIndex:" "@@:@@Q" stub_1

  -- tokenField:displayStringForRepresentedObject:
  stub_2 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldDelegateOverrides
    case _tokenField_displayStringForRepresentedObject rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenField:displayStringForRepresentedObject:" "@@:@@" stub_2

  -- tokenField:editingStringForRepresentedObject:
  stub_3 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldDelegateOverrides
    case _tokenField_editingStringForRepresentedObject rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenField:editingStringForRepresentedObject:" "@@:@@" stub_3

  -- tokenField:representedObjectForEditingString:
  stub_4 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldDelegateOverrides
    case _tokenField_representedObjectForEditingString rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenField:representedObjectForEditingString:" "@@:@@" stub_4

  -- tokenField:writeRepresentedObjects:toPasteboard:
  stub_5 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldDelegateOverrides
    case _tokenField_writeRepresentedObjects_toPasteboard rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "tokenField:writeRepresentedObjects:toPasteboard:" "B@:@@@" stub_5

  -- tokenField:readFromPasteboard:
  stub_6 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldDelegateOverrides
    case _tokenField_readFromPasteboard rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenField:readFromPasteboard:" "@@:@@" stub_6

  -- tokenField:menuForRepresentedObject:
  stub_7 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldDelegateOverrides
    case _tokenField_menuForRepresentedObject rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenField:menuForRepresentedObject:" "@@:@@" stub_7

  -- tokenField:hasMenuForRepresentedObject:
  stub_8 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldDelegateOverrides
    case _tokenField_hasMenuForRepresentedObject rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "tokenField:hasMenuForRepresentedObject:" "B@:@@" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldDelegateOverrides
    if queriedSel == sel_tokenField_completionsForSubstring_indexOfToken_indexOfSelectedItem then pure (maybe 0 (const 1) (_tokenField_completionsForSubstring_indexOfToken_indexOfSelectedItem rec_))
    else if queriedSel == sel_tokenField_shouldAddObjects_atIndex then pure (maybe 0 (const 1) (_tokenField_shouldAddObjects_atIndex rec_))
    else if queriedSel == sel_tokenField_displayStringForRepresentedObject then pure (maybe 0 (const 1) (_tokenField_displayStringForRepresentedObject rec_))
    else if queriedSel == sel_tokenField_editingStringForRepresentedObject then pure (maybe 0 (const 1) (_tokenField_editingStringForRepresentedObject rec_))
    else if queriedSel == sel_tokenField_representedObjectForEditingString then pure (maybe 0 (const 1) (_tokenField_representedObjectForEditingString rec_))
    else if queriedSel == sel_tokenField_writeRepresentedObjects_toPasteboard then pure (maybe 0 (const 1) (_tokenField_writeRepresentedObjects_toPasteboard rec_))
    else if queriedSel == sel_tokenField_readFromPasteboard then pure (maybe 0 (const 1) (_tokenField_readFromPasteboard rec_))
    else if queriedSel == sel_tokenField_menuForRepresentedObject then pure (maybe 0 (const 1) (_tokenField_menuForRepresentedObject rec_))
    else if queriedSel == sel_tokenField_hasMenuForRepresentedObject then pure (maybe 0 (const 1) (_tokenField_hasMenuForRepresentedObject rec_))
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
newNSTokenFieldDelegate :: NSTokenFieldDelegateOverrides -> IO RawId
newNSTokenFieldDelegate overrides = do
  inst <- class_createInstance nsTokenFieldDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
