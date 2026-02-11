{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSComboBoxCellDataSource@.
--
-- Usage:
--
-- @
-- delegate <- newNSComboBoxCellDataSource defaultNSComboBoxCellDataSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSComboBoxCellDataSource
  ( NSComboBoxCellDataSourceOverrides(..)
  , defaultNSComboBoxCellDataSourceOverrides
  , newNSComboBoxCellDataSource
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

-- | Overrides record for @\@protocol NSComboBoxCellDataSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSComboBoxCellDataSourceOverrides = NSComboBoxCellDataSourceOverrides
  { _numberOfItemsInComboBoxCell :: !(Maybe (RawId -> IO Int))
  , _comboBoxCell_objectValueForItemAtIndex :: !(Maybe (RawId -> Int -> IO RawId))
  , _comboBoxCell_indexOfItemWithStringValue :: !(Maybe (RawId -> RawId -> IO Int))
  , _comboBoxCell_completedString :: !(Maybe (RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSComboBoxCellDataSourceOverrides :: NSComboBoxCellDataSourceOverrides
defaultNSComboBoxCellDataSourceOverrides = NSComboBoxCellDataSourceOverrides
  { _numberOfItemsInComboBoxCell = Nothing
  , _comboBoxCell_objectValueForItemAtIndex = Nothing
  , _comboBoxCell_indexOfItemWithStringValue = Nothing
  , _comboBoxCell_completedString = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_Q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsComboBoxCellDataSourceDelegateClass #-}
nsComboBoxCellDataSourceDelegateClass :: Class
nsComboBoxCellDataSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSComboBoxCellDataSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_numberOfItemsInComboBoxCell = unSelector (mkSelector "numberOfItemsInComboBoxCell:")
      sel_comboBoxCell_objectValueForItemAtIndex = unSelector (mkSelector "comboBoxCell:objectValueForItemAtIndex:")
      sel_comboBoxCell_indexOfItemWithStringValue = unSelector (mkSelector "comboBoxCell:indexOfItemWithStringValue:")
      sel_comboBoxCell_completedString = unSelector (mkSelector "comboBoxCell:completedString:")
  -- numberOfItemsInComboBoxCell:
  stub_0 <- wrap_at_q $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxCellDataSourceOverrides
    case _numberOfItemsInComboBoxCell rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (fromIntegral r)
  addObjCMethod cls "numberOfItemsInComboBoxCell:" "q@:@" stub_0

  -- comboBoxCell:objectValueForItemAtIndex:
  stub_1 <- wrap_at_q_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxCellDataSourceOverrides
    case _comboBoxCell_objectValueForItemAtIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "comboBoxCell:objectValueForItemAtIndex:" "@@:@q" stub_1

  -- comboBoxCell:indexOfItemWithStringValue:
  stub_2 <- wrap_at_at_Q $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxCellDataSourceOverrides
    case _comboBoxCell_indexOfItemWithStringValue rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (fromIntegral r)
  addObjCMethod cls "comboBoxCell:indexOfItemWithStringValue:" "Q@:@@" stub_2

  -- comboBoxCell:completedString:
  stub_3 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxCellDataSourceOverrides
    case _comboBoxCell_completedString rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "comboBoxCell:completedString:" "@@:@@" stub_3

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSComboBoxCellDataSourceOverrides
    if queriedSel == sel_numberOfItemsInComboBoxCell then pure (maybe 0 (const 1) (_numberOfItemsInComboBoxCell rec_))
    else if queriedSel == sel_comboBoxCell_objectValueForItemAtIndex then pure (maybe 0 (const 1) (_comboBoxCell_objectValueForItemAtIndex rec_))
    else if queriedSel == sel_comboBoxCell_indexOfItemWithStringValue then pure (maybe 0 (const 1) (_comboBoxCell_indexOfItemWithStringValue rec_))
    else if queriedSel == sel_comboBoxCell_completedString then pure (maybe 0 (const 1) (_comboBoxCell_completedString rec_))
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
newNSComboBoxCellDataSource :: NSComboBoxCellDataSourceOverrides -> IO RawId
newNSComboBoxCellDataSource overrides = do
  inst <- class_createInstance nsComboBoxCellDataSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
