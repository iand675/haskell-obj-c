{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTableViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSTableViewDelegate defaultNSTableViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTableViewDelegate
  ( NSTableViewDelegateOverrides(..)
  , defaultNSTableViewDelegateOverrides
  , newNSTableViewDelegate
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

-- | Overrides record for @\@protocol NSTableViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTableViewDelegateOverrides = NSTableViewDelegateOverrides
  { _tableView_viewForTableColumn_row :: !(Maybe (RawId -> RawId -> Int -> IO RawId))
  , _tableView_rowViewForRow :: !(Maybe (RawId -> Int -> IO RawId))
  , _tableView_didAddRowView_forRow :: !(Maybe (RawId -> RawId -> Int -> IO ()))
  , _tableView_didRemoveRowView_forRow :: !(Maybe (RawId -> RawId -> Int -> IO ()))
  , _tableView_willDisplayCell_forTableColumn_row :: !(Maybe (RawId -> RawId -> RawId -> Int -> IO ()))
  , _tableView_shouldEditTableColumn_row :: !(Maybe (RawId -> RawId -> Int -> IO Bool))
  , _tableView_shouldShowCellExpansionForTableColumn_row :: !(Maybe (RawId -> RawId -> Int -> IO Bool))
  , _tableView_shouldTrackCell_forTableColumn_row :: !(Maybe (RawId -> RawId -> RawId -> Int -> IO Bool))
  , _tableView_dataCellForTableColumn_row :: !(Maybe (RawId -> RawId -> Int -> IO RawId))
  , _selectionShouldChangeInTableView :: !(Maybe (RawId -> IO Bool))
  , _tableView_shouldSelectRow :: !(Maybe (RawId -> Int -> IO Bool))
  , _tableView_selectionIndexesForProposedSelection :: !(Maybe (RawId -> RawId -> IO RawId))
  , _tableView_shouldSelectTableColumn :: !(Maybe (RawId -> RawId -> IO Bool))
  , _tableView_mouseDownInHeaderOfTableColumn :: !(Maybe (RawId -> RawId -> IO ()))
  , _tableView_didClickTableColumn :: !(Maybe (RawId -> RawId -> IO ()))
  , _tableView_didDragTableColumn :: !(Maybe (RawId -> RawId -> IO ()))
  , _tableView_heightOfRow :: !(Maybe (RawId -> Int -> IO Double))
  , _tableView_typeSelectStringForTableColumn_row :: !(Maybe (RawId -> RawId -> Int -> IO RawId))
  , _tableView_nextTypeSelectMatchFromRow_toRow_forString :: !(Maybe (RawId -> Int -> Int -> RawId -> IO Int))
  , _tableView_shouldTypeSelectForEvent_withCurrentSearchString :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _tableView_isGroupRow :: !(Maybe (RawId -> Int -> IO Bool))
  , _tableView_sizeToFitWidthOfColumn :: !(Maybe (RawId -> Int -> IO Double))
  , _tableView_shouldReorderColumn_toColumn :: !(Maybe (RawId -> Int -> Int -> IO Bool))
  , _tableView_userCanChangeVisibilityOfTableColumn :: !(Maybe (RawId -> RawId -> IO Bool))
  , _tableView_userDidChangeVisibilityOfTableColumns :: !(Maybe (RawId -> RawId -> IO ()))
  , _tableViewSelectionDidChange :: !(Maybe (RawId -> IO ()))
  , _tableViewColumnDidMove :: !(Maybe (RawId -> IO ()))
  , _tableViewColumnDidResize :: !(Maybe (RawId -> IO ()))
  , _tableViewSelectionIsChanging :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTableViewDelegateOverrides :: NSTableViewDelegateOverrides
defaultNSTableViewDelegateOverrides = NSTableViewDelegateOverrides
  { _tableView_viewForTableColumn_row = Nothing
  , _tableView_rowViewForRow = Nothing
  , _tableView_didAddRowView_forRow = Nothing
  , _tableView_didRemoveRowView_forRow = Nothing
  , _tableView_willDisplayCell_forTableColumn_row = Nothing
  , _tableView_shouldEditTableColumn_row = Nothing
  , _tableView_shouldShowCellExpansionForTableColumn_row = Nothing
  , _tableView_shouldTrackCell_forTableColumn_row = Nothing
  , _tableView_dataCellForTableColumn_row = Nothing
  , _selectionShouldChangeInTableView = Nothing
  , _tableView_shouldSelectRow = Nothing
  , _tableView_selectionIndexesForProposedSelection = Nothing
  , _tableView_shouldSelectTableColumn = Nothing
  , _tableView_mouseDownInHeaderOfTableColumn = Nothing
  , _tableView_didClickTableColumn = Nothing
  , _tableView_didDragTableColumn = Nothing
  , _tableView_heightOfRow = Nothing
  , _tableView_typeSelectStringForTableColumn_row = Nothing
  , _tableView_nextTypeSelectMatchFromRow_toRow_forString = Nothing
  , _tableView_shouldTypeSelectForEvent_withCurrentSearchString = Nothing
  , _tableView_isGroupRow = Nothing
  , _tableView_sizeToFitWidthOfColumn = Nothing
  , _tableView_shouldReorderColumn_toColumn = Nothing
  , _tableView_userCanChangeVisibilityOfTableColumn = Nothing
  , _tableView_userDidChangeVisibilityOfTableColumns = Nothing
  , _tableViewSelectionDidChange = Nothing
  , _tableViewColumnDidMove = Nothing
  , _tableViewColumnDidResize = Nothing
  , _tableViewSelectionIsChanging = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_q_q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_q_q_at_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> Ptr ObjCObject -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> Ptr ObjCObject -> IO CLong))

foreign import ccall "wrapper"
  wrap_at_q_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTableViewDelegateDelegateClass #-}
nsTableViewDelegateDelegateClass :: Class
nsTableViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTableViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_tableView_viewForTableColumn_row = unSelector (mkSelector "tableView:viewForTableColumn:row:")
      sel_tableView_rowViewForRow = unSelector (mkSelector "tableView:rowViewForRow:")
      sel_tableView_didAddRowView_forRow = unSelector (mkSelector "tableView:didAddRowView:forRow:")
      sel_tableView_didRemoveRowView_forRow = unSelector (mkSelector "tableView:didRemoveRowView:forRow:")
      sel_tableView_willDisplayCell_forTableColumn_row = unSelector (mkSelector "tableView:willDisplayCell:forTableColumn:row:")
      sel_tableView_shouldEditTableColumn_row = unSelector (mkSelector "tableView:shouldEditTableColumn:row:")
      sel_tableView_shouldShowCellExpansionForTableColumn_row = unSelector (mkSelector "tableView:shouldShowCellExpansionForTableColumn:row:")
      sel_tableView_shouldTrackCell_forTableColumn_row = unSelector (mkSelector "tableView:shouldTrackCell:forTableColumn:row:")
      sel_tableView_dataCellForTableColumn_row = unSelector (mkSelector "tableView:dataCellForTableColumn:row:")
      sel_selectionShouldChangeInTableView = unSelector (mkSelector "selectionShouldChangeInTableView:")
      sel_tableView_shouldSelectRow = unSelector (mkSelector "tableView:shouldSelectRow:")
      sel_tableView_selectionIndexesForProposedSelection = unSelector (mkSelector "tableView:selectionIndexesForProposedSelection:")
      sel_tableView_shouldSelectTableColumn = unSelector (mkSelector "tableView:shouldSelectTableColumn:")
      sel_tableView_mouseDownInHeaderOfTableColumn = unSelector (mkSelector "tableView:mouseDownInHeaderOfTableColumn:")
      sel_tableView_didClickTableColumn = unSelector (mkSelector "tableView:didClickTableColumn:")
      sel_tableView_didDragTableColumn = unSelector (mkSelector "tableView:didDragTableColumn:")
      sel_tableView_heightOfRow = unSelector (mkSelector "tableView:heightOfRow:")
      sel_tableView_typeSelectStringForTableColumn_row = unSelector (mkSelector "tableView:typeSelectStringForTableColumn:row:")
      sel_tableView_nextTypeSelectMatchFromRow_toRow_forString = unSelector (mkSelector "tableView:nextTypeSelectMatchFromRow:toRow:forString:")
      sel_tableView_shouldTypeSelectForEvent_withCurrentSearchString = unSelector (mkSelector "tableView:shouldTypeSelectForEvent:withCurrentSearchString:")
      sel_tableView_isGroupRow = unSelector (mkSelector "tableView:isGroupRow:")
      sel_tableView_sizeToFitWidthOfColumn = unSelector (mkSelector "tableView:sizeToFitWidthOfColumn:")
      sel_tableView_shouldReorderColumn_toColumn = unSelector (mkSelector "tableView:shouldReorderColumn:toColumn:")
      sel_tableView_userCanChangeVisibilityOfTableColumn = unSelector (mkSelector "tableView:userCanChangeVisibilityOfTableColumn:")
      sel_tableView_userDidChangeVisibilityOfTableColumns = unSelector (mkSelector "tableView:userDidChangeVisibilityOfTableColumns:")
      sel_tableViewSelectionDidChange = unSelector (mkSelector "tableViewSelectionDidChange:")
      sel_tableViewColumnDidMove = unSelector (mkSelector "tableViewColumnDidMove:")
      sel_tableViewColumnDidResize = unSelector (mkSelector "tableViewColumnDidResize:")
      sel_tableViewSelectionIsChanging = unSelector (mkSelector "tableViewSelectionIsChanging:")
  -- tableView:viewForTableColumn:row:
  stub_0 <- wrap_at_at_q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_viewForTableColumn_row rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tableView:viewForTableColumn:row:" "@@:@@q" stub_0

  -- tableView:rowViewForRow:
  stub_1 <- wrap_at_q_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_rowViewForRow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tableView:rowViewForRow:" "@@:@q" stub_1

  -- tableView:didAddRowView:forRow:
  stub_2 <- wrap_at_at_q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_didAddRowView_forRow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2)
  addObjCMethod cls "tableView:didAddRowView:forRow:" "v@:@@q" stub_2

  -- tableView:didRemoveRowView:forRow:
  stub_3 <- wrap_at_at_q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_didRemoveRowView_forRow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2)
  addObjCMethod cls "tableView:didRemoveRowView:forRow:" "v@:@@q" stub_3

  -- tableView:willDisplayCell:forTableColumn:row:
  stub_4 <- wrap_at_at_at_q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_willDisplayCell_forTableColumn_row rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (fromIntegral arg3)
  addObjCMethod cls "tableView:willDisplayCell:forTableColumn:row:" "v@:@@@q" stub_4

  -- tableView:shouldEditTableColumn:row:
  stub_5 <- wrap_at_at_q_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_shouldEditTableColumn_row rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "tableView:shouldEditTableColumn:row:" "B@:@@q" stub_5

  -- tableView:shouldShowCellExpansionForTableColumn:row:
  stub_6 <- wrap_at_at_q_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_shouldShowCellExpansionForTableColumn_row rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "tableView:shouldShowCellExpansionForTableColumn:row:" "B@:@@q" stub_6

  -- tableView:shouldTrackCell:forTableColumn:row:
  stub_7 <- wrap_at_at_at_q_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_shouldTrackCell_forTableColumn_row rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (fromIntegral arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "tableView:shouldTrackCell:forTableColumn:row:" "B@:@@@q" stub_7

  -- tableView:dataCellForTableColumn:row:
  stub_8 <- wrap_at_at_q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_dataCellForTableColumn_row rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tableView:dataCellForTableColumn:row:" "@@:@@q" stub_8

  -- selectionShouldChangeInTableView:
  stub_9 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _selectionShouldChangeInTableView rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "selectionShouldChangeInTableView:" "B@:@" stub_9

  -- tableView:shouldSelectRow:
  stub_10 <- wrap_at_q_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_shouldSelectRow rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "tableView:shouldSelectRow:" "B@:@q" stub_10

  -- tableView:selectionIndexesForProposedSelection:
  stub_11 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_selectionIndexesForProposedSelection rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tableView:selectionIndexesForProposedSelection:" "@@:@@" stub_11

  -- tableView:shouldSelectTableColumn:
  stub_12 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_shouldSelectTableColumn rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "tableView:shouldSelectTableColumn:" "B@:@@" stub_12

  -- tableView:mouseDownInHeaderOfTableColumn:
  stub_13 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_mouseDownInHeaderOfTableColumn rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "tableView:mouseDownInHeaderOfTableColumn:" "v@:@@" stub_13

  -- tableView:didClickTableColumn:
  stub_14 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_didClickTableColumn rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "tableView:didClickTableColumn:" "v@:@@" stub_14

  -- tableView:didDragTableColumn:
  stub_15 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_didDragTableColumn rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "tableView:didDragTableColumn:" "v@:@@" stub_15

  -- tableView:heightOfRow:
  stub_16 <- wrap_at_q_d $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_heightOfRow rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (realToFrac r)
  addObjCMethod cls "tableView:heightOfRow:" "d@:@q" stub_16

  -- tableView:typeSelectStringForTableColumn:row:
  stub_17 <- wrap_at_at_q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_typeSelectStringForTableColumn_row rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tableView:typeSelectStringForTableColumn:row:" "@@:@@q" stub_17

  -- tableView:nextTypeSelectMatchFromRow:toRow:forString:
  stub_18 <- wrap_at_q_q_at_q $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_nextTypeSelectMatchFromRow_toRow_forString rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2) (RawId arg3)
        pure (fromIntegral r)
  addObjCMethod cls "tableView:nextTypeSelectMatchFromRow:toRow:forString:" "q@:@qq@" stub_18

  -- tableView:shouldTypeSelectForEvent:withCurrentSearchString:
  stub_19 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_shouldTypeSelectForEvent_withCurrentSearchString rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "tableView:shouldTypeSelectForEvent:withCurrentSearchString:" "B@:@@@" stub_19

  -- tableView:isGroupRow:
  stub_20 <- wrap_at_q_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_isGroupRow rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "tableView:isGroupRow:" "B@:@q" stub_20

  -- tableView:sizeToFitWidthOfColumn:
  stub_21 <- wrap_at_q_d $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_sizeToFitWidthOfColumn rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (realToFrac r)
  addObjCMethod cls "tableView:sizeToFitWidthOfColumn:" "d@:@q" stub_21

  -- tableView:shouldReorderColumn:toColumn:
  stub_22 <- wrap_at_q_q_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_shouldReorderColumn_toColumn rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "tableView:shouldReorderColumn:toColumn:" "B@:@qq" stub_22

  -- tableView:userCanChangeVisibilityOfTableColumn:
  stub_23 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_userCanChangeVisibilityOfTableColumn rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "tableView:userCanChangeVisibilityOfTableColumn:" "B@:@@" stub_23

  -- tableView:userDidChangeVisibilityOfTableColumns:
  stub_24 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableView_userDidChangeVisibilityOfTableColumns rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "tableView:userDidChangeVisibilityOfTableColumns:" "v@:@@" stub_24

  -- tableViewSelectionDidChange:
  stub_25 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableViewSelectionDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "tableViewSelectionDidChange:" "v@:@" stub_25

  -- tableViewColumnDidMove:
  stub_26 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableViewColumnDidMove rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "tableViewColumnDidMove:" "v@:@" stub_26

  -- tableViewColumnDidResize:
  stub_27 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableViewColumnDidResize rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "tableViewColumnDidResize:" "v@:@" stub_27

  -- tableViewSelectionIsChanging:
  stub_28 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    case _tableViewSelectionIsChanging rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "tableViewSelectionIsChanging:" "v@:@" stub_28

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDelegateOverrides
    if queriedSel == sel_tableView_viewForTableColumn_row then pure (maybe 0 (const 1) (_tableView_viewForTableColumn_row rec_))
    else if queriedSel == sel_tableView_rowViewForRow then pure (maybe 0 (const 1) (_tableView_rowViewForRow rec_))
    else if queriedSel == sel_tableView_didAddRowView_forRow then pure (maybe 0 (const 1) (_tableView_didAddRowView_forRow rec_))
    else if queriedSel == sel_tableView_didRemoveRowView_forRow then pure (maybe 0 (const 1) (_tableView_didRemoveRowView_forRow rec_))
    else if queriedSel == sel_tableView_willDisplayCell_forTableColumn_row then pure (maybe 0 (const 1) (_tableView_willDisplayCell_forTableColumn_row rec_))
    else if queriedSel == sel_tableView_shouldEditTableColumn_row then pure (maybe 0 (const 1) (_tableView_shouldEditTableColumn_row rec_))
    else if queriedSel == sel_tableView_shouldShowCellExpansionForTableColumn_row then pure (maybe 0 (const 1) (_tableView_shouldShowCellExpansionForTableColumn_row rec_))
    else if queriedSel == sel_tableView_shouldTrackCell_forTableColumn_row then pure (maybe 0 (const 1) (_tableView_shouldTrackCell_forTableColumn_row rec_))
    else if queriedSel == sel_tableView_dataCellForTableColumn_row then pure (maybe 0 (const 1) (_tableView_dataCellForTableColumn_row rec_))
    else if queriedSel == sel_selectionShouldChangeInTableView then pure (maybe 0 (const 1) (_selectionShouldChangeInTableView rec_))
    else if queriedSel == sel_tableView_shouldSelectRow then pure (maybe 0 (const 1) (_tableView_shouldSelectRow rec_))
    else if queriedSel == sel_tableView_selectionIndexesForProposedSelection then pure (maybe 0 (const 1) (_tableView_selectionIndexesForProposedSelection rec_))
    else if queriedSel == sel_tableView_shouldSelectTableColumn then pure (maybe 0 (const 1) (_tableView_shouldSelectTableColumn rec_))
    else if queriedSel == sel_tableView_mouseDownInHeaderOfTableColumn then pure (maybe 0 (const 1) (_tableView_mouseDownInHeaderOfTableColumn rec_))
    else if queriedSel == sel_tableView_didClickTableColumn then pure (maybe 0 (const 1) (_tableView_didClickTableColumn rec_))
    else if queriedSel == sel_tableView_didDragTableColumn then pure (maybe 0 (const 1) (_tableView_didDragTableColumn rec_))
    else if queriedSel == sel_tableView_heightOfRow then pure (maybe 0 (const 1) (_tableView_heightOfRow rec_))
    else if queriedSel == sel_tableView_typeSelectStringForTableColumn_row then pure (maybe 0 (const 1) (_tableView_typeSelectStringForTableColumn_row rec_))
    else if queriedSel == sel_tableView_nextTypeSelectMatchFromRow_toRow_forString then pure (maybe 0 (const 1) (_tableView_nextTypeSelectMatchFromRow_toRow_forString rec_))
    else if queriedSel == sel_tableView_shouldTypeSelectForEvent_withCurrentSearchString then pure (maybe 0 (const 1) (_tableView_shouldTypeSelectForEvent_withCurrentSearchString rec_))
    else if queriedSel == sel_tableView_isGroupRow then pure (maybe 0 (const 1) (_tableView_isGroupRow rec_))
    else if queriedSel == sel_tableView_sizeToFitWidthOfColumn then pure (maybe 0 (const 1) (_tableView_sizeToFitWidthOfColumn rec_))
    else if queriedSel == sel_tableView_shouldReorderColumn_toColumn then pure (maybe 0 (const 1) (_tableView_shouldReorderColumn_toColumn rec_))
    else if queriedSel == sel_tableView_userCanChangeVisibilityOfTableColumn then pure (maybe 0 (const 1) (_tableView_userCanChangeVisibilityOfTableColumn rec_))
    else if queriedSel == sel_tableView_userDidChangeVisibilityOfTableColumns then pure (maybe 0 (const 1) (_tableView_userDidChangeVisibilityOfTableColumns rec_))
    else if queriedSel == sel_tableViewSelectionDidChange then pure (maybe 0 (const 1) (_tableViewSelectionDidChange rec_))
    else if queriedSel == sel_tableViewColumnDidMove then pure (maybe 0 (const 1) (_tableViewColumnDidMove rec_))
    else if queriedSel == sel_tableViewColumnDidResize then pure (maybe 0 (const 1) (_tableViewColumnDidResize rec_))
    else if queriedSel == sel_tableViewSelectionIsChanging then pure (maybe 0 (const 1) (_tableViewSelectionIsChanging rec_))
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
newNSTableViewDelegate :: NSTableViewDelegateOverrides -> IO RawId
newNSTableViewDelegate overrides = do
  inst <- class_createInstance nsTableViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
