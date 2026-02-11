{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSMenuDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSMenuDelegate defaultNSMenuDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSMenuDelegate
  ( NSMenuDelegateOverrides(..)
  , defaultNSMenuDelegateOverrides
  , newNSMenuDelegate
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

-- | Overrides record for @\@protocol NSMenuDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSMenuDelegateOverrides = NSMenuDelegateOverrides
  { _menuNeedsUpdate :: !(Maybe (RawId -> IO ()))
  , _numberOfItemsInMenu :: !(Maybe (RawId -> IO Int))
  , _menu_updateItem_atIndex_shouldCancel :: !(Maybe (RawId -> RawId -> Int -> Bool -> IO Bool))
  , _menuHasKeyEquivalent_forEvent_target_action :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO Bool))
  , _menuWillOpen :: !(Maybe (RawId -> IO ()))
  , _menuDidClose :: !(Maybe (RawId -> IO ()))
  , _menu_willHighlightItem :: !(Maybe (RawId -> RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSMenuDelegateOverrides :: NSMenuDelegateOverrides
defaultNSMenuDelegateOverrides = NSMenuDelegateOverrides
  { _menuNeedsUpdate = Nothing
  , _numberOfItemsInMenu = Nothing
  , _menu_updateItem_atIndex_shouldCancel = Nothing
  , _menuHasKeyEquivalent_forEvent_target_action = Nothing
  , _menuWillOpen = Nothing
  , _menuDidClose = Nothing
  , _menu_willHighlightItem = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_q_B_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> CULong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> CULong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsMenuDelegateDelegateClass #-}
nsMenuDelegateDelegateClass :: Class
nsMenuDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSMenuDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_menuNeedsUpdate = unSelector (mkSelector "menuNeedsUpdate:")
      sel_numberOfItemsInMenu = unSelector (mkSelector "numberOfItemsInMenu:")
      sel_menu_updateItem_atIndex_shouldCancel = unSelector (mkSelector "menu:updateItem:atIndex:shouldCancel:")
      sel_menuHasKeyEquivalent_forEvent_target_action = unSelector (mkSelector "menuHasKeyEquivalent:forEvent:target:action:")
      sel_menuWillOpen = unSelector (mkSelector "menuWillOpen:")
      sel_menuDidClose = unSelector (mkSelector "menuDidClose:")
      sel_menu_willHighlightItem = unSelector (mkSelector "menu:willHighlightItem:")
  -- menuNeedsUpdate:
  stub_0 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSMenuDelegateOverrides
    case _menuNeedsUpdate rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "menuNeedsUpdate:" "v@:@" stub_0

  -- numberOfItemsInMenu:
  stub_1 <- wrap_at_q $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSMenuDelegateOverrides
    case _numberOfItemsInMenu rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (fromIntegral r)
  addObjCMethod cls "numberOfItemsInMenu:" "q@:@" stub_1

  -- menu:updateItem:atIndex:shouldCancel:
  stub_2 <- wrap_at_at_q_B_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSMenuDelegateOverrides
    case _menu_updateItem_atIndex_shouldCancel rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2) (arg3 /= 0)
        pure (if r then 1 else 0)
  addObjCMethod cls "menu:updateItem:atIndex:shouldCancel:" "B@:@@qB" stub_2

  -- menuHasKeyEquivalent:forEvent:target:action:
  stub_3 <- wrap_at_at_at_at_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSMenuDelegateOverrides
    case _menuHasKeyEquivalent_forEvent_target_action rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "menuHasKeyEquivalent:forEvent:target:action:" "B@:@@@@" stub_3

  -- menuWillOpen:
  stub_4 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSMenuDelegateOverrides
    case _menuWillOpen rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "menuWillOpen:" "v@:@" stub_4

  -- menuDidClose:
  stub_5 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSMenuDelegateOverrides
    case _menuDidClose rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "menuDidClose:" "v@:@" stub_5

  -- menu:willHighlightItem:
  stub_6 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSMenuDelegateOverrides
    case _menu_willHighlightItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "menu:willHighlightItem:" "v@:@@" stub_6

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSMenuDelegateOverrides
    if queriedSel == sel_menuNeedsUpdate then pure (maybe 0 (const 1) (_menuNeedsUpdate rec_))
    else if queriedSel == sel_numberOfItemsInMenu then pure (maybe 0 (const 1) (_numberOfItemsInMenu rec_))
    else if queriedSel == sel_menu_updateItem_atIndex_shouldCancel then pure (maybe 0 (const 1) (_menu_updateItem_atIndex_shouldCancel rec_))
    else if queriedSel == sel_menuHasKeyEquivalent_forEvent_target_action then pure (maybe 0 (const 1) (_menuHasKeyEquivalent_forEvent_target_action rec_))
    else if queriedSel == sel_menuWillOpen then pure (maybe 0 (const 1) (_menuWillOpen rec_))
    else if queriedSel == sel_menuDidClose then pure (maybe 0 (const 1) (_menuDidClose rec_))
    else if queriedSel == sel_menu_willHighlightItem then pure (maybe 0 (const 1) (_menu_willHighlightItem rec_))
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
newNSMenuDelegate :: NSMenuDelegateOverrides -> IO RawId
newNSMenuDelegate overrides = do
  inst <- class_createInstance nsMenuDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
