{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTokenFieldCellDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSTokenFieldCellDelegate defaultNSTokenFieldCellDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTokenFieldCellDelegate
  ( NSTokenFieldCellDelegateOverrides(..)
  , defaultNSTokenFieldCellDelegateOverrides
  , newNSTokenFieldCellDelegate
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

-- | Overrides record for @\@protocol NSTokenFieldCellDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTokenFieldCellDelegateOverrides = NSTokenFieldCellDelegateOverrides
  { _tokenFieldCell_completionsForSubstring_indexOfToken_indexOfSelectedItem :: !(Maybe (RawId -> RawId -> Int -> RawId -> IO RawId))
  , _tokenFieldCell_shouldAddObjects_atIndex :: !(Maybe (RawId -> RawId -> Int -> IO RawId))
  , _tokenFieldCell_displayStringForRepresentedObject :: !(Maybe (RawId -> RawId -> IO RawId))
  , _tokenFieldCell_editingStringForRepresentedObject :: !(Maybe (RawId -> RawId -> IO RawId))
  , _tokenFieldCell_representedObjectForEditingString :: !(Maybe (RawId -> RawId -> IO RawId))
  , _tokenFieldCell_writeRepresentedObjects_toPasteboard :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _tokenFieldCell_readFromPasteboard :: !(Maybe (RawId -> RawId -> IO RawId))
  , _tokenFieldCell_menuForRepresentedObject :: !(Maybe (RawId -> RawId -> IO RawId))
  , _tokenFieldCell_hasMenuForRepresentedObject :: !(Maybe (RawId -> RawId -> IO Bool))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTokenFieldCellDelegateOverrides :: NSTokenFieldCellDelegateOverrides
defaultNSTokenFieldCellDelegateOverrides = NSTokenFieldCellDelegateOverrides
  { _tokenFieldCell_completionsForSubstring_indexOfToken_indexOfSelectedItem = Nothing
  , _tokenFieldCell_shouldAddObjects_atIndex = Nothing
  , _tokenFieldCell_displayStringForRepresentedObject = Nothing
  , _tokenFieldCell_editingStringForRepresentedObject = Nothing
  , _tokenFieldCell_representedObjectForEditingString = Nothing
  , _tokenFieldCell_writeRepresentedObjects_toPasteboard = Nothing
  , _tokenFieldCell_readFromPasteboard = Nothing
  , _tokenFieldCell_menuForRepresentedObject = Nothing
  , _tokenFieldCell_hasMenuForRepresentedObject = Nothing
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
{-# NOINLINE nsTokenFieldCellDelegateDelegateClass #-}
nsTokenFieldCellDelegateDelegateClass :: Class
nsTokenFieldCellDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTokenFieldCellDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_tokenFieldCell_completionsForSubstring_indexOfToken_indexOfSelectedItem = unSelector (mkSelector "tokenFieldCell:completionsForSubstring:indexOfToken:indexOfSelectedItem:")
      sel_tokenFieldCell_shouldAddObjects_atIndex = unSelector (mkSelector "tokenFieldCell:shouldAddObjects:atIndex:")
      sel_tokenFieldCell_displayStringForRepresentedObject = unSelector (mkSelector "tokenFieldCell:displayStringForRepresentedObject:")
      sel_tokenFieldCell_editingStringForRepresentedObject = unSelector (mkSelector "tokenFieldCell:editingStringForRepresentedObject:")
      sel_tokenFieldCell_representedObjectForEditingString = unSelector (mkSelector "tokenFieldCell:representedObjectForEditingString:")
      sel_tokenFieldCell_writeRepresentedObjects_toPasteboard = unSelector (mkSelector "tokenFieldCell:writeRepresentedObjects:toPasteboard:")
      sel_tokenFieldCell_readFromPasteboard = unSelector (mkSelector "tokenFieldCell:readFromPasteboard:")
      sel_tokenFieldCell_menuForRepresentedObject = unSelector (mkSelector "tokenFieldCell:menuForRepresentedObject:")
      sel_tokenFieldCell_hasMenuForRepresentedObject = unSelector (mkSelector "tokenFieldCell:hasMenuForRepresentedObject:")
  -- tokenFieldCell:completionsForSubstring:indexOfToken:indexOfSelectedItem:
  stub_0 <- wrap_at_at_q_at_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldCellDelegateOverrides
    case _tokenFieldCell_completionsForSubstring_indexOfToken_indexOfSelectedItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2) (RawId arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenFieldCell:completionsForSubstring:indexOfToken:indexOfSelectedItem:" "@@:@@q@" stub_0

  -- tokenFieldCell:shouldAddObjects:atIndex:
  stub_1 <- wrap_at_at_Q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldCellDelegateOverrides
    case _tokenFieldCell_shouldAddObjects_atIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenFieldCell:shouldAddObjects:atIndex:" "@@:@@Q" stub_1

  -- tokenFieldCell:displayStringForRepresentedObject:
  stub_2 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldCellDelegateOverrides
    case _tokenFieldCell_displayStringForRepresentedObject rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenFieldCell:displayStringForRepresentedObject:" "@@:@@" stub_2

  -- tokenFieldCell:editingStringForRepresentedObject:
  stub_3 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldCellDelegateOverrides
    case _tokenFieldCell_editingStringForRepresentedObject rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenFieldCell:editingStringForRepresentedObject:" "@@:@@" stub_3

  -- tokenFieldCell:representedObjectForEditingString:
  stub_4 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldCellDelegateOverrides
    case _tokenFieldCell_representedObjectForEditingString rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenFieldCell:representedObjectForEditingString:" "@@:@@" stub_4

  -- tokenFieldCell:writeRepresentedObjects:toPasteboard:
  stub_5 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldCellDelegateOverrides
    case _tokenFieldCell_writeRepresentedObjects_toPasteboard rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "tokenFieldCell:writeRepresentedObjects:toPasteboard:" "B@:@@@" stub_5

  -- tokenFieldCell:readFromPasteboard:
  stub_6 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldCellDelegateOverrides
    case _tokenFieldCell_readFromPasteboard rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenFieldCell:readFromPasteboard:" "@@:@@" stub_6

  -- tokenFieldCell:menuForRepresentedObject:
  stub_7 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldCellDelegateOverrides
    case _tokenFieldCell_menuForRepresentedObject rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tokenFieldCell:menuForRepresentedObject:" "@@:@@" stub_7

  -- tokenFieldCell:hasMenuForRepresentedObject:
  stub_8 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldCellDelegateOverrides
    case _tokenFieldCell_hasMenuForRepresentedObject rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "tokenFieldCell:hasMenuForRepresentedObject:" "B@:@@" stub_8

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTokenFieldCellDelegateOverrides
    if queriedSel == sel_tokenFieldCell_completionsForSubstring_indexOfToken_indexOfSelectedItem then pure (maybe 0 (const 1) (_tokenFieldCell_completionsForSubstring_indexOfToken_indexOfSelectedItem rec_))
    else if queriedSel == sel_tokenFieldCell_shouldAddObjects_atIndex then pure (maybe 0 (const 1) (_tokenFieldCell_shouldAddObjects_atIndex rec_))
    else if queriedSel == sel_tokenFieldCell_displayStringForRepresentedObject then pure (maybe 0 (const 1) (_tokenFieldCell_displayStringForRepresentedObject rec_))
    else if queriedSel == sel_tokenFieldCell_editingStringForRepresentedObject then pure (maybe 0 (const 1) (_tokenFieldCell_editingStringForRepresentedObject rec_))
    else if queriedSel == sel_tokenFieldCell_representedObjectForEditingString then pure (maybe 0 (const 1) (_tokenFieldCell_representedObjectForEditingString rec_))
    else if queriedSel == sel_tokenFieldCell_writeRepresentedObjects_toPasteboard then pure (maybe 0 (const 1) (_tokenFieldCell_writeRepresentedObjects_toPasteboard rec_))
    else if queriedSel == sel_tokenFieldCell_readFromPasteboard then pure (maybe 0 (const 1) (_tokenFieldCell_readFromPasteboard rec_))
    else if queriedSel == sel_tokenFieldCell_menuForRepresentedObject then pure (maybe 0 (const 1) (_tokenFieldCell_menuForRepresentedObject rec_))
    else if queriedSel == sel_tokenFieldCell_hasMenuForRepresentedObject then pure (maybe 0 (const 1) (_tokenFieldCell_hasMenuForRepresentedObject rec_))
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
newNSTokenFieldCellDelegate :: NSTokenFieldCellDelegateOverrides -> IO RawId
newNSTokenFieldCellDelegate overrides = do
  inst <- class_createInstance nsTokenFieldCellDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
