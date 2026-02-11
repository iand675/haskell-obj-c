{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSToolbarDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSToolbarDelegate defaultNSToolbarDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSToolbarDelegate
  ( NSToolbarDelegateOverrides(..)
  , defaultNSToolbarDelegateOverrides
  , newNSToolbarDelegate
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

-- | Overrides record for @\@protocol NSToolbarDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSToolbarDelegateOverrides = NSToolbarDelegateOverrides
  { _toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar :: !(Maybe (RawId -> RawId -> Bool -> IO RawId))
  , _toolbarDefaultItemIdentifiers :: !(Maybe (RawId -> IO RawId))
  , _toolbarAllowedItemIdentifiers :: !(Maybe (RawId -> IO RawId))
  , _toolbarSelectableItemIdentifiers :: !(Maybe (RawId -> IO RawId))
  , _toolbarImmovableItemIdentifiers :: !(Maybe (RawId -> IO RawId))
  , _toolbar_itemIdentifier_canBeInsertedAtIndex :: !(Maybe (RawId -> RawId -> Int -> IO Bool))
  , _toolbarWillAddItem :: !(Maybe (RawId -> IO ()))
  , _toolbarDidRemoveItem :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSToolbarDelegateOverrides :: NSToolbarDelegateOverrides
defaultNSToolbarDelegateOverrides = NSToolbarDelegateOverrides
  { _toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar = Nothing
  , _toolbarDefaultItemIdentifiers = Nothing
  , _toolbarAllowedItemIdentifiers = Nothing
  , _toolbarSelectableItemIdentifiers = Nothing
  , _toolbarImmovableItemIdentifiers = Nothing
  , _toolbar_itemIdentifier_canBeInsertedAtIndex = Nothing
  , _toolbarWillAddItem = Nothing
  , _toolbarDidRemoveItem = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_B_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsToolbarDelegateDelegateClass #-}
nsToolbarDelegateDelegateClass :: Class
nsToolbarDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSToolbarDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar = unSelector (mkSelector "toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:")
      sel_toolbarDefaultItemIdentifiers = unSelector (mkSelector "toolbarDefaultItemIdentifiers:")
      sel_toolbarAllowedItemIdentifiers = unSelector (mkSelector "toolbarAllowedItemIdentifiers:")
      sel_toolbarSelectableItemIdentifiers = unSelector (mkSelector "toolbarSelectableItemIdentifiers:")
      sel_toolbarImmovableItemIdentifiers = unSelector (mkSelector "toolbarImmovableItemIdentifiers:")
      sel_toolbar_itemIdentifier_canBeInsertedAtIndex = unSelector (mkSelector "toolbar:itemIdentifier:canBeInsertedAtIndex:")
      sel_toolbarWillAddItem = unSelector (mkSelector "toolbarWillAddItem:")
      sel_toolbarDidRemoveItem = unSelector (mkSelector "toolbarDidRemoveItem:")
  -- toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:
  stub_0 <- wrap_at_at_B_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSToolbarDelegateOverrides
    case _toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (arg2 /= 0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:" "@@:@@B" stub_0

  -- toolbarDefaultItemIdentifiers:
  stub_1 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSToolbarDelegateOverrides
    case _toolbarDefaultItemIdentifiers rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "toolbarDefaultItemIdentifiers:" "@@:@" stub_1

  -- toolbarAllowedItemIdentifiers:
  stub_2 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSToolbarDelegateOverrides
    case _toolbarAllowedItemIdentifiers rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "toolbarAllowedItemIdentifiers:" "@@:@" stub_2

  -- toolbarSelectableItemIdentifiers:
  stub_3 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSToolbarDelegateOverrides
    case _toolbarSelectableItemIdentifiers rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "toolbarSelectableItemIdentifiers:" "@@:@" stub_3

  -- toolbarImmovableItemIdentifiers:
  stub_4 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSToolbarDelegateOverrides
    case _toolbarImmovableItemIdentifiers rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "toolbarImmovableItemIdentifiers:" "@@:@" stub_4

  -- toolbar:itemIdentifier:canBeInsertedAtIndex:
  stub_5 <- wrap_at_at_q_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSToolbarDelegateOverrides
    case _toolbar_itemIdentifier_canBeInsertedAtIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "toolbar:itemIdentifier:canBeInsertedAtIndex:" "B@:@@q" stub_5

  -- toolbarWillAddItem:
  stub_6 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSToolbarDelegateOverrides
    case _toolbarWillAddItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "toolbarWillAddItem:" "v@:@" stub_6

  -- toolbarDidRemoveItem:
  stub_7 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSToolbarDelegateOverrides
    case _toolbarDidRemoveItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "toolbarDidRemoveItem:" "v@:@" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSToolbarDelegateOverrides
    if queriedSel == sel_toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar then pure (maybe 0 (const 1) (_toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar rec_))
    else if queriedSel == sel_toolbarDefaultItemIdentifiers then pure (maybe 0 (const 1) (_toolbarDefaultItemIdentifiers rec_))
    else if queriedSel == sel_toolbarAllowedItemIdentifiers then pure (maybe 0 (const 1) (_toolbarAllowedItemIdentifiers rec_))
    else if queriedSel == sel_toolbarSelectableItemIdentifiers then pure (maybe 0 (const 1) (_toolbarSelectableItemIdentifiers rec_))
    else if queriedSel == sel_toolbarImmovableItemIdentifiers then pure (maybe 0 (const 1) (_toolbarImmovableItemIdentifiers rec_))
    else if queriedSel == sel_toolbar_itemIdentifier_canBeInsertedAtIndex then pure (maybe 0 (const 1) (_toolbar_itemIdentifier_canBeInsertedAtIndex rec_))
    else if queriedSel == sel_toolbarWillAddItem then pure (maybe 0 (const 1) (_toolbarWillAddItem rec_))
    else if queriedSel == sel_toolbarDidRemoveItem then pure (maybe 0 (const 1) (_toolbarDidRemoveItem rec_))
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
newNSToolbarDelegate :: NSToolbarDelegateOverrides -> IO RawId
newNSToolbarDelegate overrides = do
  inst <- class_createInstance nsToolbarDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
