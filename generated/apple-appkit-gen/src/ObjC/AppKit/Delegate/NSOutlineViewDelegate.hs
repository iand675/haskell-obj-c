{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSOutlineViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSOutlineViewDelegate defaultNSOutlineViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSOutlineViewDelegate
  ( NSOutlineViewDelegateOverrides(..)
  , defaultNSOutlineViewDelegateOverrides
  , newNSOutlineViewDelegate
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

-- | Overrides record for @\@protocol NSOutlineViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSOutlineViewDelegateOverrides = NSOutlineViewDelegateOverrides
  { _outlineView_viewForTableColumn_item :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _outlineView_rowViewForItem :: !(Maybe (RawId -> RawId -> IO RawId))
  , _outlineView_didAddRowView_forRow :: !(Maybe (RawId -> RawId -> Int -> IO ()))
  , _outlineView_didRemoveRowView_forRow :: !(Maybe (RawId -> RawId -> Int -> IO ()))
  , _outlineView_willDisplayCell_forTableColumn_item :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _outlineView_shouldEditTableColumn_item :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _selectionShouldChangeInOutlineView :: !(Maybe (RawId -> IO Bool))
  , _outlineView_shouldSelectItem :: !(Maybe (RawId -> RawId -> IO Bool))
  , _outlineView_selectionIndexesForProposedSelection :: !(Maybe (RawId -> RawId -> IO RawId))
  , _outlineView_shouldSelectTableColumn :: !(Maybe (RawId -> RawId -> IO Bool))
  , _outlineView_mouseDownInHeaderOfTableColumn :: !(Maybe (RawId -> RawId -> IO ()))
  , _outlineView_didClickTableColumn :: !(Maybe (RawId -> RawId -> IO ()))
  , _outlineView_didDragTableColumn :: !(Maybe (RawId -> RawId -> IO ()))
  , _outlineView_heightOfRowByItem :: !(Maybe (RawId -> RawId -> IO Double))
  , _outlineView_tintConfigurationForItem :: !(Maybe (RawId -> RawId -> IO RawId))
  , _outlineView_typeSelectStringForTableColumn_item :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _outlineView_nextTypeSelectMatchFromItem_toItem_forString :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO RawId))
  , _outlineView_shouldTypeSelectForEvent_withCurrentSearchString :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _outlineView_shouldShowCellExpansionForTableColumn_item :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _outlineView_shouldTrackCell_forTableColumn_item :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO Bool))
  , _outlineView_dataCellForTableColumn_item :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _outlineView_isGroupItem :: !(Maybe (RawId -> RawId -> IO Bool))
  , _outlineView_shouldExpandItem :: !(Maybe (RawId -> RawId -> IO Bool))
  , _outlineView_shouldCollapseItem :: !(Maybe (RawId -> RawId -> IO Bool))
  , _outlineView_willDisplayOutlineCell_forTableColumn_item :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _outlineView_sizeToFitWidthOfColumn :: !(Maybe (RawId -> Int -> IO Double))
  , _outlineView_shouldReorderColumn_toColumn :: !(Maybe (RawId -> Int -> Int -> IO Bool))
  , _outlineView_shouldShowOutlineCellForItem :: !(Maybe (RawId -> RawId -> IO Bool))
  , _outlineView_userCanChangeVisibilityOfTableColumn :: !(Maybe (RawId -> RawId -> IO Bool))
  , _outlineView_userDidChangeVisibilityOfTableColumns :: !(Maybe (RawId -> RawId -> IO ()))
  , _outlineViewSelectionDidChange :: !(Maybe (RawId -> IO ()))
  , _outlineViewColumnDidMove :: !(Maybe (RawId -> IO ()))
  , _outlineViewColumnDidResize :: !(Maybe (RawId -> IO ()))
  , _outlineViewSelectionIsChanging :: !(Maybe (RawId -> IO ()))
  , _outlineViewItemWillExpand :: !(Maybe (RawId -> IO ()))
  , _outlineViewItemDidExpand :: !(Maybe (RawId -> IO ()))
  , _outlineViewItemWillCollapse :: !(Maybe (RawId -> IO ()))
  , _outlineViewItemDidCollapse :: !(Maybe (RawId -> IO ()))
  }

-- | Default overrides with all methods unimplemented.
defaultNSOutlineViewDelegateOverrides :: NSOutlineViewDelegateOverrides
defaultNSOutlineViewDelegateOverrides = NSOutlineViewDelegateOverrides
  { _outlineView_viewForTableColumn_item = Nothing
  , _outlineView_rowViewForItem = Nothing
  , _outlineView_didAddRowView_forRow = Nothing
  , _outlineView_didRemoveRowView_forRow = Nothing
  , _outlineView_willDisplayCell_forTableColumn_item = Nothing
  , _outlineView_shouldEditTableColumn_item = Nothing
  , _selectionShouldChangeInOutlineView = Nothing
  , _outlineView_shouldSelectItem = Nothing
  , _outlineView_selectionIndexesForProposedSelection = Nothing
  , _outlineView_shouldSelectTableColumn = Nothing
  , _outlineView_mouseDownInHeaderOfTableColumn = Nothing
  , _outlineView_didClickTableColumn = Nothing
  , _outlineView_didDragTableColumn = Nothing
  , _outlineView_heightOfRowByItem = Nothing
  , _outlineView_tintConfigurationForItem = Nothing
  , _outlineView_typeSelectStringForTableColumn_item = Nothing
  , _outlineView_nextTypeSelectMatchFromItem_toItem_forString = Nothing
  , _outlineView_shouldTypeSelectForEvent_withCurrentSearchString = Nothing
  , _outlineView_shouldShowCellExpansionForTableColumn_item = Nothing
  , _outlineView_shouldTrackCell_forTableColumn_item = Nothing
  , _outlineView_dataCellForTableColumn_item = Nothing
  , _outlineView_isGroupItem = Nothing
  , _outlineView_shouldExpandItem = Nothing
  , _outlineView_shouldCollapseItem = Nothing
  , _outlineView_willDisplayOutlineCell_forTableColumn_item = Nothing
  , _outlineView_sizeToFitWidthOfColumn = Nothing
  , _outlineView_shouldReorderColumn_toColumn = Nothing
  , _outlineView_shouldShowOutlineCellForItem = Nothing
  , _outlineView_userCanChangeVisibilityOfTableColumn = Nothing
  , _outlineView_userDidChangeVisibilityOfTableColumns = Nothing
  , _outlineViewSelectionDidChange = Nothing
  , _outlineViewColumnDidMove = Nothing
  , _outlineViewColumnDidResize = Nothing
  , _outlineViewSelectionIsChanging = Nothing
  , _outlineViewItemWillExpand = Nothing
  , _outlineViewItemDidExpand = Nothing
  , _outlineViewItemWillCollapse = Nothing
  , _outlineViewItemDidCollapse = Nothing
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
  wrap_at_q_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsOutlineViewDelegateDelegateClass #-}
nsOutlineViewDelegateDelegateClass :: Class
nsOutlineViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSOutlineViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_outlineView_viewForTableColumn_item = unSelector (mkSelector "outlineView:viewForTableColumn:item:")
      sel_outlineView_rowViewForItem = unSelector (mkSelector "outlineView:rowViewForItem:")
      sel_outlineView_didAddRowView_forRow = unSelector (mkSelector "outlineView:didAddRowView:forRow:")
      sel_outlineView_didRemoveRowView_forRow = unSelector (mkSelector "outlineView:didRemoveRowView:forRow:")
      sel_outlineView_willDisplayCell_forTableColumn_item = unSelector (mkSelector "outlineView:willDisplayCell:forTableColumn:item:")
      sel_outlineView_shouldEditTableColumn_item = unSelector (mkSelector "outlineView:shouldEditTableColumn:item:")
      sel_selectionShouldChangeInOutlineView = unSelector (mkSelector "selectionShouldChangeInOutlineView:")
      sel_outlineView_shouldSelectItem = unSelector (mkSelector "outlineView:shouldSelectItem:")
      sel_outlineView_selectionIndexesForProposedSelection = unSelector (mkSelector "outlineView:selectionIndexesForProposedSelection:")
      sel_outlineView_shouldSelectTableColumn = unSelector (mkSelector "outlineView:shouldSelectTableColumn:")
      sel_outlineView_mouseDownInHeaderOfTableColumn = unSelector (mkSelector "outlineView:mouseDownInHeaderOfTableColumn:")
      sel_outlineView_didClickTableColumn = unSelector (mkSelector "outlineView:didClickTableColumn:")
      sel_outlineView_didDragTableColumn = unSelector (mkSelector "outlineView:didDragTableColumn:")
      sel_outlineView_heightOfRowByItem = unSelector (mkSelector "outlineView:heightOfRowByItem:")
      sel_outlineView_tintConfigurationForItem = unSelector (mkSelector "outlineView:tintConfigurationForItem:")
      sel_outlineView_typeSelectStringForTableColumn_item = unSelector (mkSelector "outlineView:typeSelectStringForTableColumn:item:")
      sel_outlineView_nextTypeSelectMatchFromItem_toItem_forString = unSelector (mkSelector "outlineView:nextTypeSelectMatchFromItem:toItem:forString:")
      sel_outlineView_shouldTypeSelectForEvent_withCurrentSearchString = unSelector (mkSelector "outlineView:shouldTypeSelectForEvent:withCurrentSearchString:")
      sel_outlineView_shouldShowCellExpansionForTableColumn_item = unSelector (mkSelector "outlineView:shouldShowCellExpansionForTableColumn:item:")
      sel_outlineView_shouldTrackCell_forTableColumn_item = unSelector (mkSelector "outlineView:shouldTrackCell:forTableColumn:item:")
      sel_outlineView_dataCellForTableColumn_item = unSelector (mkSelector "outlineView:dataCellForTableColumn:item:")
      sel_outlineView_isGroupItem = unSelector (mkSelector "outlineView:isGroupItem:")
      sel_outlineView_shouldExpandItem = unSelector (mkSelector "outlineView:shouldExpandItem:")
      sel_outlineView_shouldCollapseItem = unSelector (mkSelector "outlineView:shouldCollapseItem:")
      sel_outlineView_willDisplayOutlineCell_forTableColumn_item = unSelector (mkSelector "outlineView:willDisplayOutlineCell:forTableColumn:item:")
      sel_outlineView_sizeToFitWidthOfColumn = unSelector (mkSelector "outlineView:sizeToFitWidthOfColumn:")
      sel_outlineView_shouldReorderColumn_toColumn = unSelector (mkSelector "outlineView:shouldReorderColumn:toColumn:")
      sel_outlineView_shouldShowOutlineCellForItem = unSelector (mkSelector "outlineView:shouldShowOutlineCellForItem:")
      sel_outlineView_userCanChangeVisibilityOfTableColumn = unSelector (mkSelector "outlineView:userCanChangeVisibilityOfTableColumn:")
      sel_outlineView_userDidChangeVisibilityOfTableColumns = unSelector (mkSelector "outlineView:userDidChangeVisibilityOfTableColumns:")
      sel_outlineViewSelectionDidChange = unSelector (mkSelector "outlineViewSelectionDidChange:")
      sel_outlineViewColumnDidMove = unSelector (mkSelector "outlineViewColumnDidMove:")
      sel_outlineViewColumnDidResize = unSelector (mkSelector "outlineViewColumnDidResize:")
      sel_outlineViewSelectionIsChanging = unSelector (mkSelector "outlineViewSelectionIsChanging:")
      sel_outlineViewItemWillExpand = unSelector (mkSelector "outlineViewItemWillExpand:")
      sel_outlineViewItemDidExpand = unSelector (mkSelector "outlineViewItemDidExpand:")
      sel_outlineViewItemWillCollapse = unSelector (mkSelector "outlineViewItemWillCollapse:")
      sel_outlineViewItemDidCollapse = unSelector (mkSelector "outlineViewItemDidCollapse:")
  -- outlineView:viewForTableColumn:item:
  stub_0 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_viewForTableColumn_item rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:viewForTableColumn:item:" "@@:@@@" stub_0

  -- outlineView:rowViewForItem:
  stub_1 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_rowViewForItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:rowViewForItem:" "@@:@@" stub_1

  -- outlineView:didAddRowView:forRow:
  stub_2 <- wrap_at_at_q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_didAddRowView_forRow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2)
  addObjCMethod cls "outlineView:didAddRowView:forRow:" "v@:@@q" stub_2

  -- outlineView:didRemoveRowView:forRow:
  stub_3 <- wrap_at_at_q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_didRemoveRowView_forRow rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2)
  addObjCMethod cls "outlineView:didRemoveRowView:forRow:" "v@:@@q" stub_3

  -- outlineView:willDisplayCell:forTableColumn:item:
  stub_4 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_willDisplayCell_forTableColumn_item rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "outlineView:willDisplayCell:forTableColumn:item:" "v@:@@@@" stub_4

  -- outlineView:shouldEditTableColumn:item:
  stub_5 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_shouldEditTableColumn_item rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:shouldEditTableColumn:item:" "B@:@@@" stub_5

  -- selectionShouldChangeInOutlineView:
  stub_6 <- wrap_at_B $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _selectionShouldChangeInOutlineView rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (if r then 1 else 0)
  addObjCMethod cls "selectionShouldChangeInOutlineView:" "B@:@" stub_6

  -- outlineView:shouldSelectItem:
  stub_7 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_shouldSelectItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:shouldSelectItem:" "B@:@@" stub_7

  -- outlineView:selectionIndexesForProposedSelection:
  stub_8 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_selectionIndexesForProposedSelection rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:selectionIndexesForProposedSelection:" "@@:@@" stub_8

  -- outlineView:shouldSelectTableColumn:
  stub_9 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_shouldSelectTableColumn rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:shouldSelectTableColumn:" "B@:@@" stub_9

  -- outlineView:mouseDownInHeaderOfTableColumn:
  stub_10 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_mouseDownInHeaderOfTableColumn rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "outlineView:mouseDownInHeaderOfTableColumn:" "v@:@@" stub_10

  -- outlineView:didClickTableColumn:
  stub_11 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_didClickTableColumn rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "outlineView:didClickTableColumn:" "v@:@@" stub_11

  -- outlineView:didDragTableColumn:
  stub_12 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_didDragTableColumn rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "outlineView:didDragTableColumn:" "v@:@@" stub_12

  -- outlineView:heightOfRowByItem:
  stub_13 <- wrap_at_at_d $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_heightOfRowByItem rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (realToFrac r)
  addObjCMethod cls "outlineView:heightOfRowByItem:" "d@:@@" stub_13

  -- outlineView:tintConfigurationForItem:
  stub_14 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_tintConfigurationForItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:tintConfigurationForItem:" "@@:@@" stub_14

  -- outlineView:typeSelectStringForTableColumn:item:
  stub_15 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_typeSelectStringForTableColumn_item rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:typeSelectStringForTableColumn:item:" "@@:@@@" stub_15

  -- outlineView:nextTypeSelectMatchFromItem:toItem:forString:
  stub_16 <- wrap_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_nextTypeSelectMatchFromItem_toItem_forString rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:nextTypeSelectMatchFromItem:toItem:forString:" "@@:@@@@" stub_16

  -- outlineView:shouldTypeSelectForEvent:withCurrentSearchString:
  stub_17 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_shouldTypeSelectForEvent_withCurrentSearchString rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:shouldTypeSelectForEvent:withCurrentSearchString:" "B@:@@@" stub_17

  -- outlineView:shouldShowCellExpansionForTableColumn:item:
  stub_18 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_shouldShowCellExpansionForTableColumn_item rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:shouldShowCellExpansionForTableColumn:item:" "B@:@@@" stub_18

  -- outlineView:shouldTrackCell:forTableColumn:item:
  stub_19 <- wrap_at_at_at_at_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_shouldTrackCell_forTableColumn_item rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:shouldTrackCell:forTableColumn:item:" "B@:@@@@" stub_19

  -- outlineView:dataCellForTableColumn:item:
  stub_20 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_dataCellForTableColumn_item rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:dataCellForTableColumn:item:" "@@:@@@" stub_20

  -- outlineView:isGroupItem:
  stub_21 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_isGroupItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:isGroupItem:" "B@:@@" stub_21

  -- outlineView:shouldExpandItem:
  stub_22 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_shouldExpandItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:shouldExpandItem:" "B@:@@" stub_22

  -- outlineView:shouldCollapseItem:
  stub_23 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_shouldCollapseItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:shouldCollapseItem:" "B@:@@" stub_23

  -- outlineView:willDisplayOutlineCell:forTableColumn:item:
  stub_24 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_willDisplayOutlineCell_forTableColumn_item rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "outlineView:willDisplayOutlineCell:forTableColumn:item:" "v@:@@@@" stub_24

  -- outlineView:sizeToFitWidthOfColumn:
  stub_25 <- wrap_at_q_d $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_sizeToFitWidthOfColumn rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (realToFrac r)
  addObjCMethod cls "outlineView:sizeToFitWidthOfColumn:" "d@:@q" stub_25

  -- outlineView:shouldReorderColumn:toColumn:
  stub_26 <- wrap_at_q_q_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_shouldReorderColumn_toColumn rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:shouldReorderColumn:toColumn:" "B@:@qq" stub_26

  -- outlineView:shouldShowOutlineCellForItem:
  stub_27 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_shouldShowOutlineCellForItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:shouldShowOutlineCellForItem:" "B@:@@" stub_27

  -- outlineView:userCanChangeVisibilityOfTableColumn:
  stub_28 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_userCanChangeVisibilityOfTableColumn rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:userCanChangeVisibilityOfTableColumn:" "B@:@@" stub_28

  -- outlineView:userDidChangeVisibilityOfTableColumns:
  stub_29 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineView_userDidChangeVisibilityOfTableColumns rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "outlineView:userDidChangeVisibilityOfTableColumns:" "v@:@@" stub_29

  -- outlineViewSelectionDidChange:
  stub_30 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineViewSelectionDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "outlineViewSelectionDidChange:" "v@:@" stub_30

  -- outlineViewColumnDidMove:
  stub_31 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineViewColumnDidMove rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "outlineViewColumnDidMove:" "v@:@" stub_31

  -- outlineViewColumnDidResize:
  stub_32 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineViewColumnDidResize rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "outlineViewColumnDidResize:" "v@:@" stub_32

  -- outlineViewSelectionIsChanging:
  stub_33 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineViewSelectionIsChanging rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "outlineViewSelectionIsChanging:" "v@:@" stub_33

  -- outlineViewItemWillExpand:
  stub_34 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineViewItemWillExpand rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "outlineViewItemWillExpand:" "v@:@" stub_34

  -- outlineViewItemDidExpand:
  stub_35 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineViewItemDidExpand rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "outlineViewItemDidExpand:" "v@:@" stub_35

  -- outlineViewItemWillCollapse:
  stub_36 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineViewItemWillCollapse rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "outlineViewItemWillCollapse:" "v@:@" stub_36

  -- outlineViewItemDidCollapse:
  stub_37 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    case _outlineViewItemDidCollapse rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "outlineViewItemDidCollapse:" "v@:@" stub_37

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDelegateOverrides
    if queriedSel == sel_outlineView_viewForTableColumn_item then pure (maybe 0 (const 1) (_outlineView_viewForTableColumn_item rec_))
    else if queriedSel == sel_outlineView_rowViewForItem then pure (maybe 0 (const 1) (_outlineView_rowViewForItem rec_))
    else if queriedSel == sel_outlineView_didAddRowView_forRow then pure (maybe 0 (const 1) (_outlineView_didAddRowView_forRow rec_))
    else if queriedSel == sel_outlineView_didRemoveRowView_forRow then pure (maybe 0 (const 1) (_outlineView_didRemoveRowView_forRow rec_))
    else if queriedSel == sel_outlineView_willDisplayCell_forTableColumn_item then pure (maybe 0 (const 1) (_outlineView_willDisplayCell_forTableColumn_item rec_))
    else if queriedSel == sel_outlineView_shouldEditTableColumn_item then pure (maybe 0 (const 1) (_outlineView_shouldEditTableColumn_item rec_))
    else if queriedSel == sel_selectionShouldChangeInOutlineView then pure (maybe 0 (const 1) (_selectionShouldChangeInOutlineView rec_))
    else if queriedSel == sel_outlineView_shouldSelectItem then pure (maybe 0 (const 1) (_outlineView_shouldSelectItem rec_))
    else if queriedSel == sel_outlineView_selectionIndexesForProposedSelection then pure (maybe 0 (const 1) (_outlineView_selectionIndexesForProposedSelection rec_))
    else if queriedSel == sel_outlineView_shouldSelectTableColumn then pure (maybe 0 (const 1) (_outlineView_shouldSelectTableColumn rec_))
    else if queriedSel == sel_outlineView_mouseDownInHeaderOfTableColumn then pure (maybe 0 (const 1) (_outlineView_mouseDownInHeaderOfTableColumn rec_))
    else if queriedSel == sel_outlineView_didClickTableColumn then pure (maybe 0 (const 1) (_outlineView_didClickTableColumn rec_))
    else if queriedSel == sel_outlineView_didDragTableColumn then pure (maybe 0 (const 1) (_outlineView_didDragTableColumn rec_))
    else if queriedSel == sel_outlineView_heightOfRowByItem then pure (maybe 0 (const 1) (_outlineView_heightOfRowByItem rec_))
    else if queriedSel == sel_outlineView_tintConfigurationForItem then pure (maybe 0 (const 1) (_outlineView_tintConfigurationForItem rec_))
    else if queriedSel == sel_outlineView_typeSelectStringForTableColumn_item then pure (maybe 0 (const 1) (_outlineView_typeSelectStringForTableColumn_item rec_))
    else if queriedSel == sel_outlineView_nextTypeSelectMatchFromItem_toItem_forString then pure (maybe 0 (const 1) (_outlineView_nextTypeSelectMatchFromItem_toItem_forString rec_))
    else if queriedSel == sel_outlineView_shouldTypeSelectForEvent_withCurrentSearchString then pure (maybe 0 (const 1) (_outlineView_shouldTypeSelectForEvent_withCurrentSearchString rec_))
    else if queriedSel == sel_outlineView_shouldShowCellExpansionForTableColumn_item then pure (maybe 0 (const 1) (_outlineView_shouldShowCellExpansionForTableColumn_item rec_))
    else if queriedSel == sel_outlineView_shouldTrackCell_forTableColumn_item then pure (maybe 0 (const 1) (_outlineView_shouldTrackCell_forTableColumn_item rec_))
    else if queriedSel == sel_outlineView_dataCellForTableColumn_item then pure (maybe 0 (const 1) (_outlineView_dataCellForTableColumn_item rec_))
    else if queriedSel == sel_outlineView_isGroupItem then pure (maybe 0 (const 1) (_outlineView_isGroupItem rec_))
    else if queriedSel == sel_outlineView_shouldExpandItem then pure (maybe 0 (const 1) (_outlineView_shouldExpandItem rec_))
    else if queriedSel == sel_outlineView_shouldCollapseItem then pure (maybe 0 (const 1) (_outlineView_shouldCollapseItem rec_))
    else if queriedSel == sel_outlineView_willDisplayOutlineCell_forTableColumn_item then pure (maybe 0 (const 1) (_outlineView_willDisplayOutlineCell_forTableColumn_item rec_))
    else if queriedSel == sel_outlineView_sizeToFitWidthOfColumn then pure (maybe 0 (const 1) (_outlineView_sizeToFitWidthOfColumn rec_))
    else if queriedSel == sel_outlineView_shouldReorderColumn_toColumn then pure (maybe 0 (const 1) (_outlineView_shouldReorderColumn_toColumn rec_))
    else if queriedSel == sel_outlineView_shouldShowOutlineCellForItem then pure (maybe 0 (const 1) (_outlineView_shouldShowOutlineCellForItem rec_))
    else if queriedSel == sel_outlineView_userCanChangeVisibilityOfTableColumn then pure (maybe 0 (const 1) (_outlineView_userCanChangeVisibilityOfTableColumn rec_))
    else if queriedSel == sel_outlineView_userDidChangeVisibilityOfTableColumns then pure (maybe 0 (const 1) (_outlineView_userDidChangeVisibilityOfTableColumns rec_))
    else if queriedSel == sel_outlineViewSelectionDidChange then pure (maybe 0 (const 1) (_outlineViewSelectionDidChange rec_))
    else if queriedSel == sel_outlineViewColumnDidMove then pure (maybe 0 (const 1) (_outlineViewColumnDidMove rec_))
    else if queriedSel == sel_outlineViewColumnDidResize then pure (maybe 0 (const 1) (_outlineViewColumnDidResize rec_))
    else if queriedSel == sel_outlineViewSelectionIsChanging then pure (maybe 0 (const 1) (_outlineViewSelectionIsChanging rec_))
    else if queriedSel == sel_outlineViewItemWillExpand then pure (maybe 0 (const 1) (_outlineViewItemWillExpand rec_))
    else if queriedSel == sel_outlineViewItemDidExpand then pure (maybe 0 (const 1) (_outlineViewItemDidExpand rec_))
    else if queriedSel == sel_outlineViewItemWillCollapse then pure (maybe 0 (const 1) (_outlineViewItemWillCollapse rec_))
    else if queriedSel == sel_outlineViewItemDidCollapse then pure (maybe 0 (const 1) (_outlineViewItemDidCollapse rec_))
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
newNSOutlineViewDelegate :: NSOutlineViewDelegateOverrides -> IO RawId
newNSOutlineViewDelegate overrides = do
  inst <- class_createInstance nsOutlineViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
