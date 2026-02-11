{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSBrowserDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSBrowserDelegate defaultNSBrowserDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSBrowserDelegate
  ( NSBrowserDelegateOverrides(..)
  , defaultNSBrowserDelegateOverrides
  , newNSBrowserDelegate
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

-- | Overrides record for @\@protocol NSBrowserDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSBrowserDelegateOverrides = NSBrowserDelegateOverrides
  { _browser_numberOfRowsInColumn :: !(Maybe (RawId -> Int -> IO Int))
  , _browser_createRowsForColumn_inMatrix :: !(Maybe (RawId -> Int -> RawId -> IO ()))
  , _browser_numberOfChildrenOfItem :: !(Maybe (RawId -> RawId -> IO Int))
  , _browser_child_ofItem :: !(Maybe (RawId -> Int -> RawId -> IO RawId))
  , _browser_isLeafItem :: !(Maybe (RawId -> RawId -> IO Bool))
  , _browser_objectValueForItem :: !(Maybe (RawId -> RawId -> IO RawId))
  , _browser_heightOfRow_inColumn :: !(Maybe (RawId -> Int -> Int -> IO Double))
  , _rootItemForBrowser :: !(Maybe (RawId -> IO RawId))
  , _browser_setObjectValue_forItem :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _browser_shouldEditItem :: !(Maybe (RawId -> RawId -> IO Bool))
  , _browser_willDisplayCell_atRow_column :: !(Maybe (RawId -> RawId -> Int -> Int -> IO ()))
  , _browser_titleOfColumn :: !(Maybe (RawId -> Int -> IO RawId))
  , _browser_selectCellWithString_inColumn :: !(Maybe (RawId -> RawId -> Int -> IO Bool))
  , _browser_selectRow_inColumn :: !(Maybe (RawId -> Int -> Int -> IO Bool))
  , _browser_isColumnValid :: !(Maybe (RawId -> Int -> IO Bool))
  , _browserWillScroll :: !(Maybe (RawId -> IO ()))
  , _browserDidScroll :: !(Maybe (RawId -> IO ()))
  , _browser_shouldSizeColumn_forUserResize_toWidth :: !(Maybe (RawId -> Int -> Bool -> Double -> IO Double))
  , _browser_sizeToFitWidthOfColumn :: !(Maybe (RawId -> Int -> IO Double))
  , _browserColumnConfigurationDidChange :: !(Maybe (RawId -> IO ()))
  , _browser_shouldShowCellExpansionForRow_column :: !(Maybe (RawId -> Int -> Int -> IO Bool))
  , _browser_writeRowsWithIndexes_inColumn_toPasteboard :: !(Maybe (RawId -> RawId -> Int -> RawId -> IO Bool))
  , _browser_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes_inColumn :: !(Maybe (RawId -> RawId -> RawId -> Int -> IO RawId))
  , _browser_canDragRowsWithIndexes_inColumn_withEvent :: !(Maybe (RawId -> RawId -> Int -> RawId -> IO Bool))
  , _browser_draggingImageForRowsWithIndexes_inColumn_withEvent_offset :: !(Maybe (RawId -> RawId -> Int -> RawId -> RawId -> IO RawId))
  , _browser_typeSelectStringForRow_inColumn :: !(Maybe (RawId -> Int -> Int -> IO RawId))
  , _browser_shouldTypeSelectForEvent_withCurrentSearchString :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _browser_nextTypeSelectMatchFromRow_toRow_inColumn_forString :: !(Maybe (RawId -> Int -> Int -> Int -> RawId -> IO Int))
  , _browser_previewViewControllerForLeafItem :: !(Maybe (RawId -> RawId -> IO RawId))
  , _browser_headerViewControllerForItem :: !(Maybe (RawId -> RawId -> IO RawId))
  , _browser_didChangeLastColumn_toColumn :: !(Maybe (RawId -> Int -> Int -> IO ()))
  , _browser_selectionIndexesForProposedSelection_inColumn :: !(Maybe (RawId -> RawId -> Int -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSBrowserDelegateOverrides :: NSBrowserDelegateOverrides
defaultNSBrowserDelegateOverrides = NSBrowserDelegateOverrides
  { _browser_numberOfRowsInColumn = Nothing
  , _browser_createRowsForColumn_inMatrix = Nothing
  , _browser_numberOfChildrenOfItem = Nothing
  , _browser_child_ofItem = Nothing
  , _browser_isLeafItem = Nothing
  , _browser_objectValueForItem = Nothing
  , _browser_heightOfRow_inColumn = Nothing
  , _rootItemForBrowser = Nothing
  , _browser_setObjectValue_forItem = Nothing
  , _browser_shouldEditItem = Nothing
  , _browser_willDisplayCell_atRow_column = Nothing
  , _browser_titleOfColumn = Nothing
  , _browser_selectCellWithString_inColumn = Nothing
  , _browser_selectRow_inColumn = Nothing
  , _browser_isColumnValid = Nothing
  , _browserWillScroll = Nothing
  , _browserDidScroll = Nothing
  , _browser_shouldSizeColumn_forUserResize_toWidth = Nothing
  , _browser_sizeToFitWidthOfColumn = Nothing
  , _browserColumnConfigurationDidChange = Nothing
  , _browser_shouldShowCellExpansionForRow_column = Nothing
  , _browser_writeRowsWithIndexes_inColumn_toPasteboard = Nothing
  , _browser_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes_inColumn = Nothing
  , _browser_canDragRowsWithIndexes_inColumn_withEvent = Nothing
  , _browser_draggingImageForRowsWithIndexes_inColumn_withEvent_offset = Nothing
  , _browser_typeSelectStringForRow_inColumn = Nothing
  , _browser_shouldTypeSelectForEvent_withCurrentSearchString = Nothing
  , _browser_nextTypeSelectMatchFromRow_toRow_inColumn_forString = Nothing
  , _browser_previewViewControllerForLeafItem = Nothing
  , _browser_headerViewControllerForItem = Nothing
  , _browser_didChangeLastColumn_toColumn = Nothing
  , _browser_selectionIndexesForProposedSelection_inColumn = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_q_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_q_q_q_at_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> CLong -> Ptr ObjCObject -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> CLong -> Ptr ObjCObject -> IO CLong))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_q_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_q_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_q_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_q_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at_q_B_d_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CULong -> CDouble -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CULong -> CDouble -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_q_q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_q_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_q_q_d
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO CDouble)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> CLong -> IO CDouble))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_q_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO CLong))

foreign import ccall "wrapper"
  wrap_at_q_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_q_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO CLong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsBrowserDelegateDelegateClass #-}
nsBrowserDelegateDelegateClass :: Class
nsBrowserDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSBrowserDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_browser_numberOfRowsInColumn = unSelector (mkSelector "browser:numberOfRowsInColumn:")
      sel_browser_createRowsForColumn_inMatrix = unSelector (mkSelector "browser:createRowsForColumn:inMatrix:")
      sel_browser_numberOfChildrenOfItem = unSelector (mkSelector "browser:numberOfChildrenOfItem:")
      sel_browser_child_ofItem = unSelector (mkSelector "browser:child:ofItem:")
      sel_browser_isLeafItem = unSelector (mkSelector "browser:isLeafItem:")
      sel_browser_objectValueForItem = unSelector (mkSelector "browser:objectValueForItem:")
      sel_browser_heightOfRow_inColumn = unSelector (mkSelector "browser:heightOfRow:inColumn:")
      sel_rootItemForBrowser = unSelector (mkSelector "rootItemForBrowser:")
      sel_browser_setObjectValue_forItem = unSelector (mkSelector "browser:setObjectValue:forItem:")
      sel_browser_shouldEditItem = unSelector (mkSelector "browser:shouldEditItem:")
      sel_browser_willDisplayCell_atRow_column = unSelector (mkSelector "browser:willDisplayCell:atRow:column:")
      sel_browser_titleOfColumn = unSelector (mkSelector "browser:titleOfColumn:")
      sel_browser_selectCellWithString_inColumn = unSelector (mkSelector "browser:selectCellWithString:inColumn:")
      sel_browser_selectRow_inColumn = unSelector (mkSelector "browser:selectRow:inColumn:")
      sel_browser_isColumnValid = unSelector (mkSelector "browser:isColumnValid:")
      sel_browserWillScroll = unSelector (mkSelector "browserWillScroll:")
      sel_browserDidScroll = unSelector (mkSelector "browserDidScroll:")
      sel_browser_shouldSizeColumn_forUserResize_toWidth = unSelector (mkSelector "browser:shouldSizeColumn:forUserResize:toWidth:")
      sel_browser_sizeToFitWidthOfColumn = unSelector (mkSelector "browser:sizeToFitWidthOfColumn:")
      sel_browserColumnConfigurationDidChange = unSelector (mkSelector "browserColumnConfigurationDidChange:")
      sel_browser_shouldShowCellExpansionForRow_column = unSelector (mkSelector "browser:shouldShowCellExpansionForRow:column:")
      sel_browser_writeRowsWithIndexes_inColumn_toPasteboard = unSelector (mkSelector "browser:writeRowsWithIndexes:inColumn:toPasteboard:")
      sel_browser_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes_inColumn = unSelector (mkSelector "browser:namesOfPromisedFilesDroppedAtDestination:forDraggedRowsWithIndexes:inColumn:")
      sel_browser_canDragRowsWithIndexes_inColumn_withEvent = unSelector (mkSelector "browser:canDragRowsWithIndexes:inColumn:withEvent:")
      sel_browser_draggingImageForRowsWithIndexes_inColumn_withEvent_offset = unSelector (mkSelector "browser:draggingImageForRowsWithIndexes:inColumn:withEvent:offset:")
      sel_browser_typeSelectStringForRow_inColumn = unSelector (mkSelector "browser:typeSelectStringForRow:inColumn:")
      sel_browser_shouldTypeSelectForEvent_withCurrentSearchString = unSelector (mkSelector "browser:shouldTypeSelectForEvent:withCurrentSearchString:")
      sel_browser_nextTypeSelectMatchFromRow_toRow_inColumn_forString = unSelector (mkSelector "browser:nextTypeSelectMatchFromRow:toRow:inColumn:forString:")
      sel_browser_previewViewControllerForLeafItem = unSelector (mkSelector "browser:previewViewControllerForLeafItem:")
      sel_browser_headerViewControllerForItem = unSelector (mkSelector "browser:headerViewControllerForItem:")
      sel_browser_didChangeLastColumn_toColumn = unSelector (mkSelector "browser:didChangeLastColumn:toColumn:")
      sel_browser_selectionIndexesForProposedSelection_inColumn = unSelector (mkSelector "browser:selectionIndexesForProposedSelection:inColumn:")
  -- browser:numberOfRowsInColumn:
  stub_0 <- wrap_at_q_q $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_numberOfRowsInColumn rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (fromIntegral r)
  addObjCMethod cls "browser:numberOfRowsInColumn:" "q@:@q" stub_0

  -- browser:createRowsForColumn:inMatrix:
  stub_1 <- wrap_at_q_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_createRowsForColumn_inMatrix rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (RawId arg2)
  addObjCMethod cls "browser:createRowsForColumn:inMatrix:" "v@:@q@" stub_1

  -- browser:numberOfChildrenOfItem:
  stub_2 <- wrap_at_at_q $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_numberOfChildrenOfItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (fromIntegral r)
  addObjCMethod cls "browser:numberOfChildrenOfItem:" "q@:@@" stub_2

  -- browser:child:ofItem:
  stub_3 <- wrap_at_q_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_child_ofItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "browser:child:ofItem:" "@@:@q@" stub_3

  -- browser:isLeafItem:
  stub_4 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_isLeafItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "browser:isLeafItem:" "B@:@@" stub_4

  -- browser:objectValueForItem:
  stub_5 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_objectValueForItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "browser:objectValueForItem:" "@@:@@" stub_5

  -- browser:heightOfRow:inColumn:
  stub_6 <- wrap_at_q_q_d $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_heightOfRow_inColumn rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
        pure (realToFrac r)
  addObjCMethod cls "browser:heightOfRow:inColumn:" "d@:@qq" stub_6

  -- rootItemForBrowser:
  stub_7 <- wrap_at_at $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _rootItemForBrowser rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "rootItemForBrowser:" "@@:@" stub_7

  -- browser:setObjectValue:forItem:
  stub_8 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_setObjectValue_forItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "browser:setObjectValue:forItem:" "v@:@@@" stub_8

  -- browser:shouldEditItem:
  stub_9 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_shouldEditItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "browser:shouldEditItem:" "B@:@@" stub_9

  -- browser:willDisplayCell:atRow:column:
  stub_10 <- wrap_at_at_q_q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_willDisplayCell_atRow_column rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (fromIntegral arg2) (fromIntegral arg3)
  addObjCMethod cls "browser:willDisplayCell:atRow:column:" "v@:@@qq" stub_10

  -- browser:titleOfColumn:
  stub_11 <- wrap_at_q_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_titleOfColumn rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "browser:titleOfColumn:" "@@:@q" stub_11

  -- browser:selectCellWithString:inColumn:
  stub_12 <- wrap_at_at_q_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_selectCellWithString_inColumn rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "browser:selectCellWithString:inColumn:" "B@:@@q" stub_12

  -- browser:selectRow:inColumn:
  stub_13 <- wrap_at_q_q_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_selectRow_inColumn rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "browser:selectRow:inColumn:" "B@:@qq" stub_13

  -- browser:isColumnValid:
  stub_14 <- wrap_at_q_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_isColumnValid rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "browser:isColumnValid:" "B@:@q" stub_14

  -- browserWillScroll:
  stub_15 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browserWillScroll rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "browserWillScroll:" "v@:@" stub_15

  -- browserDidScroll:
  stub_16 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browserDidScroll rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "browserDidScroll:" "v@:@" stub_16

  -- browser:shouldSizeColumn:forUserResize:toWidth:
  stub_17 <- wrap_at_q_B_d_d $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_shouldSizeColumn_forUserResize_toWidth rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (arg2 /= 0) (realToFrac arg3)
        pure (realToFrac r)
  addObjCMethod cls "browser:shouldSizeColumn:forUserResize:toWidth:" "d@:@qBd" stub_17

  -- browser:sizeToFitWidthOfColumn:
  stub_18 <- wrap_at_q_d $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_sizeToFitWidthOfColumn rec_ of
      Nothing -> pure 0.0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (realToFrac r)
  addObjCMethod cls "browser:sizeToFitWidthOfColumn:" "d@:@q" stub_18

  -- browserColumnConfigurationDidChange:
  stub_19 <- wrap_at_v $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browserColumnConfigurationDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0)
  addObjCMethod cls "browserColumnConfigurationDidChange:" "v@:@" stub_19

  -- browser:shouldShowCellExpansionForRow:column:
  stub_20 <- wrap_at_q_q_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_shouldShowCellExpansionForRow_column rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "browser:shouldShowCellExpansionForRow:column:" "B@:@qq" stub_20

  -- browser:writeRowsWithIndexes:inColumn:toPasteboard:
  stub_21 <- wrap_at_at_q_at_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_writeRowsWithIndexes_inColumn_toPasteboard rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2) (RawId arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "browser:writeRowsWithIndexes:inColumn:toPasteboard:" "B@:@@q@" stub_21

  -- browser:namesOfPromisedFilesDroppedAtDestination:forDraggedRowsWithIndexes:inColumn:
  stub_22 <- wrap_at_at_at_q_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes_inColumn rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (fromIntegral arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "browser:namesOfPromisedFilesDroppedAtDestination:forDraggedRowsWithIndexes:inColumn:" "@@:@@@q" stub_22

  -- browser:canDragRowsWithIndexes:inColumn:withEvent:
  stub_23 <- wrap_at_at_q_at_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_canDragRowsWithIndexes_inColumn_withEvent rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2) (RawId arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "browser:canDragRowsWithIndexes:inColumn:withEvent:" "B@:@@q@" stub_23

  -- browser:draggingImageForRowsWithIndexes:inColumn:withEvent:offset:
  stub_24 <- wrap_at_at_q_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_draggingImageForRowsWithIndexes_inColumn_withEvent_offset rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2) (RawId arg3) (RawId arg4)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "browser:draggingImageForRowsWithIndexes:inColumn:withEvent:offset:" "@@:@@q@@" stub_24

  -- browser:typeSelectStringForRow:inColumn:
  stub_25 <- wrap_at_q_q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_typeSelectStringForRow_inColumn rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "browser:typeSelectStringForRow:inColumn:" "@@:@qq" stub_25

  -- browser:shouldTypeSelectForEvent:withCurrentSearchString:
  stub_26 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_shouldTypeSelectForEvent_withCurrentSearchString rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "browser:shouldTypeSelectForEvent:withCurrentSearchString:" "B@:@@@" stub_26

  -- browser:nextTypeSelectMatchFromRow:toRow:inColumn:forString:
  stub_27 <- wrap_at_q_q_q_at_q $ \self _cmd arg0 arg1 arg2 arg3 arg4 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_nextTypeSelectMatchFromRow_toRow_inColumn_forString rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2) (fromIntegral arg3) (RawId arg4)
        pure (fromIntegral r)
  addObjCMethod cls "browser:nextTypeSelectMatchFromRow:toRow:inColumn:forString:" "q@:@qqq@" stub_27

  -- browser:previewViewControllerForLeafItem:
  stub_28 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_previewViewControllerForLeafItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "browser:previewViewControllerForLeafItem:" "@@:@@" stub_28

  -- browser:headerViewControllerForItem:
  stub_29 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_headerViewControllerForItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "browser:headerViewControllerForItem:" "@@:@@" stub_29

  -- browser:didChangeLastColumn:toColumn:
  stub_30 <- wrap_at_q_q_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_didChangeLastColumn_toColumn rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (fromIntegral arg1) (fromIntegral arg2)
  addObjCMethod cls "browser:didChangeLastColumn:toColumn:" "v@:@qq" stub_30

  -- browser:selectionIndexesForProposedSelection:inColumn:
  stub_31 <- wrap_at_at_q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    case _browser_selectionIndexesForProposedSelection_inColumn rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "browser:selectionIndexesForProposedSelection:inColumn:" "@@:@@q" stub_31

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSBrowserDelegateOverrides
    if queriedSel == sel_browser_numberOfRowsInColumn then pure (maybe 0 (const 1) (_browser_numberOfRowsInColumn rec_))
    else if queriedSel == sel_browser_createRowsForColumn_inMatrix then pure (maybe 0 (const 1) (_browser_createRowsForColumn_inMatrix rec_))
    else if queriedSel == sel_browser_numberOfChildrenOfItem then pure (maybe 0 (const 1) (_browser_numberOfChildrenOfItem rec_))
    else if queriedSel == sel_browser_child_ofItem then pure (maybe 0 (const 1) (_browser_child_ofItem rec_))
    else if queriedSel == sel_browser_isLeafItem then pure (maybe 0 (const 1) (_browser_isLeafItem rec_))
    else if queriedSel == sel_browser_objectValueForItem then pure (maybe 0 (const 1) (_browser_objectValueForItem rec_))
    else if queriedSel == sel_browser_heightOfRow_inColumn then pure (maybe 0 (const 1) (_browser_heightOfRow_inColumn rec_))
    else if queriedSel == sel_rootItemForBrowser then pure (maybe 0 (const 1) (_rootItemForBrowser rec_))
    else if queriedSel == sel_browser_setObjectValue_forItem then pure (maybe 0 (const 1) (_browser_setObjectValue_forItem rec_))
    else if queriedSel == sel_browser_shouldEditItem then pure (maybe 0 (const 1) (_browser_shouldEditItem rec_))
    else if queriedSel == sel_browser_willDisplayCell_atRow_column then pure (maybe 0 (const 1) (_browser_willDisplayCell_atRow_column rec_))
    else if queriedSel == sel_browser_titleOfColumn then pure (maybe 0 (const 1) (_browser_titleOfColumn rec_))
    else if queriedSel == sel_browser_selectCellWithString_inColumn then pure (maybe 0 (const 1) (_browser_selectCellWithString_inColumn rec_))
    else if queriedSel == sel_browser_selectRow_inColumn then pure (maybe 0 (const 1) (_browser_selectRow_inColumn rec_))
    else if queriedSel == sel_browser_isColumnValid then pure (maybe 0 (const 1) (_browser_isColumnValid rec_))
    else if queriedSel == sel_browserWillScroll then pure (maybe 0 (const 1) (_browserWillScroll rec_))
    else if queriedSel == sel_browserDidScroll then pure (maybe 0 (const 1) (_browserDidScroll rec_))
    else if queriedSel == sel_browser_shouldSizeColumn_forUserResize_toWidth then pure (maybe 0 (const 1) (_browser_shouldSizeColumn_forUserResize_toWidth rec_))
    else if queriedSel == sel_browser_sizeToFitWidthOfColumn then pure (maybe 0 (const 1) (_browser_sizeToFitWidthOfColumn rec_))
    else if queriedSel == sel_browserColumnConfigurationDidChange then pure (maybe 0 (const 1) (_browserColumnConfigurationDidChange rec_))
    else if queriedSel == sel_browser_shouldShowCellExpansionForRow_column then pure (maybe 0 (const 1) (_browser_shouldShowCellExpansionForRow_column rec_))
    else if queriedSel == sel_browser_writeRowsWithIndexes_inColumn_toPasteboard then pure (maybe 0 (const 1) (_browser_writeRowsWithIndexes_inColumn_toPasteboard rec_))
    else if queriedSel == sel_browser_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes_inColumn then pure (maybe 0 (const 1) (_browser_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes_inColumn rec_))
    else if queriedSel == sel_browser_canDragRowsWithIndexes_inColumn_withEvent then pure (maybe 0 (const 1) (_browser_canDragRowsWithIndexes_inColumn_withEvent rec_))
    else if queriedSel == sel_browser_draggingImageForRowsWithIndexes_inColumn_withEvent_offset then pure (maybe 0 (const 1) (_browser_draggingImageForRowsWithIndexes_inColumn_withEvent_offset rec_))
    else if queriedSel == sel_browser_typeSelectStringForRow_inColumn then pure (maybe 0 (const 1) (_browser_typeSelectStringForRow_inColumn rec_))
    else if queriedSel == sel_browser_shouldTypeSelectForEvent_withCurrentSearchString then pure (maybe 0 (const 1) (_browser_shouldTypeSelectForEvent_withCurrentSearchString rec_))
    else if queriedSel == sel_browser_nextTypeSelectMatchFromRow_toRow_inColumn_forString then pure (maybe 0 (const 1) (_browser_nextTypeSelectMatchFromRow_toRow_inColumn_forString rec_))
    else if queriedSel == sel_browser_previewViewControllerForLeafItem then pure (maybe 0 (const 1) (_browser_previewViewControllerForLeafItem rec_))
    else if queriedSel == sel_browser_headerViewControllerForItem then pure (maybe 0 (const 1) (_browser_headerViewControllerForItem rec_))
    else if queriedSel == sel_browser_didChangeLastColumn_toColumn then pure (maybe 0 (const 1) (_browser_didChangeLastColumn_toColumn rec_))
    else if queriedSel == sel_browser_selectionIndexesForProposedSelection_inColumn then pure (maybe 0 (const 1) (_browser_selectionIndexesForProposedSelection_inColumn rec_))
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
newNSBrowserDelegate :: NSBrowserDelegateOverrides -> IO RawId
newNSBrowserDelegate overrides = do
  inst <- class_createInstance nsBrowserDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
