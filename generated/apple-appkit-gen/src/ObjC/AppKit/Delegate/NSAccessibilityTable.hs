{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSAccessibilityTable@.
--
-- Usage:
--
-- @
-- delegate <- newNSAccessibilityTable defaultNSAccessibilityTableOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSAccessibilityTable
  ( NSAccessibilityTableOverrides(..)
  , defaultNSAccessibilityTableOverrides
  , newNSAccessibilityTable
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

-- | Overrides record for @\@protocol NSAccessibilityTable@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSAccessibilityTableOverrides = NSAccessibilityTableOverrides
  { _accessibilityLabel :: !(Maybe (IO RawId))
  , _accessibilityRows :: !(Maybe (IO RawId))
  , _accessibilitySelectedRows :: !(Maybe (IO RawId))
  , _setAccessibilitySelectedRows :: !(Maybe (RawId -> IO ()))
  , _accessibilityVisibleRows :: !(Maybe (IO RawId))
  , _accessibilityColumns :: !(Maybe (IO RawId))
  , _accessibilityVisibleColumns :: !(Maybe (IO RawId))
  , _accessibilitySelectedColumns :: !(Maybe (IO RawId))
  , _accessibilityHeaderGroup :: !(Maybe (IO RawId))
  , _accessibilitySelectedCells :: !(Maybe (IO RawId))
  , _accessibilityVisibleCells :: !(Maybe (IO RawId))
  , _accessibilityRowHeaderUIElements :: !(Maybe (IO RawId))
  , _accessibilityColumnHeaderUIElements :: !(Maybe (IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSAccessibilityTableOverrides :: NSAccessibilityTableOverrides
defaultNSAccessibilityTableOverrides = NSAccessibilityTableOverrides
  { _accessibilityLabel = Nothing
  , _accessibilityRows = Nothing
  , _accessibilitySelectedRows = Nothing
  , _setAccessibilitySelectedRows = Nothing
  , _accessibilityVisibleRows = Nothing
  , _accessibilityColumns = Nothing
  , _accessibilityVisibleColumns = Nothing
  , _accessibilitySelectedColumns = Nothing
  , _accessibilityHeaderGroup = Nothing
  , _accessibilitySelectedCells = Nothing
  , _accessibilityVisibleCells = Nothing
  , _accessibilityRowHeaderUIElements = Nothing
  , _accessibilityColumnHeaderUIElements = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsAccessibilityTableDelegateClass #-}
nsAccessibilityTableDelegateClass :: Class
nsAccessibilityTableDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSAccessibilityTable" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_accessibilityLabel = unSelector (mkSelector "accessibilityLabel")
      sel_accessibilityRows = unSelector (mkSelector "accessibilityRows")
      sel_accessibilitySelectedRows = unSelector (mkSelector "accessibilitySelectedRows")
      sel_setAccessibilitySelectedRows = unSelector (mkSelector "setAccessibilitySelectedRows:")
      sel_accessibilityVisibleRows = unSelector (mkSelector "accessibilityVisibleRows")
      sel_accessibilityColumns = unSelector (mkSelector "accessibilityColumns")
      sel_accessibilityVisibleColumns = unSelector (mkSelector "accessibilityVisibleColumns")
      sel_accessibilitySelectedColumns = unSelector (mkSelector "accessibilitySelectedColumns")
      sel_accessibilityHeaderGroup = unSelector (mkSelector "accessibilityHeaderGroup")
      sel_accessibilitySelectedCells = unSelector (mkSelector "accessibilitySelectedCells")
      sel_accessibilityVisibleCells = unSelector (mkSelector "accessibilityVisibleCells")
      sel_accessibilityRowHeaderUIElements = unSelector (mkSelector "accessibilityRowHeaderUIElements")
      sel_accessibilityColumnHeaderUIElements = unSelector (mkSelector "accessibilityColumnHeaderUIElements")
  -- accessibilityLabel
  stub_0 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _accessibilityLabel rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityLabel" "@@:" stub_0

  -- accessibilityRows
  stub_1 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _accessibilityRows rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityRows" "@@:" stub_1

  -- accessibilitySelectedRows
  stub_2 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _accessibilitySelectedRows rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySelectedRows" "@@:" stub_2

  -- setAccessibilitySelectedRows:
  stub_3 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _setAccessibilitySelectedRows rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "setAccessibilitySelectedRows:" "v@:@" stub_3

  -- accessibilityVisibleRows
  stub_4 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _accessibilityVisibleRows rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityVisibleRows" "@@:" stub_4

  -- accessibilityColumns
  stub_5 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _accessibilityColumns rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityColumns" "@@:" stub_5

  -- accessibilityVisibleColumns
  stub_6 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _accessibilityVisibleColumns rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityVisibleColumns" "@@:" stub_6

  -- accessibilitySelectedColumns
  stub_7 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _accessibilitySelectedColumns rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySelectedColumns" "@@:" stub_7

  -- accessibilityHeaderGroup
  stub_8 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _accessibilityHeaderGroup rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityHeaderGroup" "@@:" stub_8

  -- accessibilitySelectedCells
  stub_9 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _accessibilitySelectedCells rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilitySelectedCells" "@@:" stub_9

  -- accessibilityVisibleCells
  stub_10 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _accessibilityVisibleCells rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityVisibleCells" "@@:" stub_10

  -- accessibilityRowHeaderUIElements
  stub_11 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _accessibilityRowHeaderUIElements rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityRowHeaderUIElements" "@@:" stub_11

  -- accessibilityColumnHeaderUIElements
  stub_12 <- wrap_at $ \self _cmd -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    case _accessibilityColumnHeaderUIElements rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f 
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "accessibilityColumnHeaderUIElements" "@@:" stub_12

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSAccessibilityTableOverrides
    if queriedSel == sel_accessibilityLabel then pure (maybe 0 (const 1) (_accessibilityLabel rec_))
    else if queriedSel == sel_accessibilityRows then pure (maybe 0 (const 1) (_accessibilityRows rec_))
    else if queriedSel == sel_accessibilitySelectedRows then pure (maybe 0 (const 1) (_accessibilitySelectedRows rec_))
    else if queriedSel == sel_setAccessibilitySelectedRows then pure (maybe 0 (const 1) (_setAccessibilitySelectedRows rec_))
    else if queriedSel == sel_accessibilityVisibleRows then pure (maybe 0 (const 1) (_accessibilityVisibleRows rec_))
    else if queriedSel == sel_accessibilityColumns then pure (maybe 0 (const 1) (_accessibilityColumns rec_))
    else if queriedSel == sel_accessibilityVisibleColumns then pure (maybe 0 (const 1) (_accessibilityVisibleColumns rec_))
    else if queriedSel == sel_accessibilitySelectedColumns then pure (maybe 0 (const 1) (_accessibilitySelectedColumns rec_))
    else if queriedSel == sel_accessibilityHeaderGroup then pure (maybe 0 (const 1) (_accessibilityHeaderGroup rec_))
    else if queriedSel == sel_accessibilitySelectedCells then pure (maybe 0 (const 1) (_accessibilitySelectedCells rec_))
    else if queriedSel == sel_accessibilityVisibleCells then pure (maybe 0 (const 1) (_accessibilityVisibleCells rec_))
    else if queriedSel == sel_accessibilityRowHeaderUIElements then pure (maybe 0 (const 1) (_accessibilityRowHeaderUIElements rec_))
    else if queriedSel == sel_accessibilityColumnHeaderUIElements then pure (maybe 0 (const 1) (_accessibilityColumnHeaderUIElements rec_))
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
newNSAccessibilityTable :: NSAccessibilityTableOverrides -> IO RawId
newNSAccessibilityTable overrides = do
  inst <- class_createInstance nsAccessibilityTableDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
