{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSTableViewDataSource@.
--
-- Usage:
--
-- @
-- delegate <- newNSTableViewDataSource defaultNSTableViewDataSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSTableViewDataSource
  ( NSTableViewDataSourceOverrides(..)
  , defaultNSTableViewDataSourceOverrides
  , newNSTableViewDataSource
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

-- | Overrides record for @\@protocol NSTableViewDataSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSTableViewDataSourceOverrides = NSTableViewDataSourceOverrides
  { _numberOfRowsInTableView :: !(Maybe (RawId -> IO Int))
  , _tableView_objectValueForTableColumn_row :: !(Maybe (RawId -> RawId -> Int -> IO RawId))
  , _tableView_setObjectValue_forTableColumn_row :: !(Maybe (RawId -> RawId -> RawId -> Int -> IO ()))
  , _tableView_sortDescriptorsDidChange :: !(Maybe (RawId -> RawId -> IO ()))
  , _tableView_pasteboardWriterForRow :: !(Maybe (RawId -> Int -> IO RawId))
  , _tableView_updateDraggingItemsForDrag :: !(Maybe (RawId -> RawId -> IO ()))
  , _tableView_writeRowsWithIndexes_toPasteboard :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _tableView_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSTableViewDataSourceOverrides :: NSTableViewDataSourceOverrides
defaultNSTableViewDataSourceOverrides = NSTableViewDataSourceOverrides
  { _numberOfRowsInTableView = Nothing
  , _tableView_objectValueForTableColumn_row = Nothing
  , _tableView_setObjectValue_forTableColumn_row = Nothing
  , _tableView_sortDescriptorsDidChange = Nothing
  , _tableView_pasteboardWriterForRow = Nothing
  , _tableView_updateDraggingItemsForDrag = Nothing
  , _tableView_writeRowsWithIndexes_toPasteboard = Nothing
  , _tableView_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_q_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_q
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> IO CLong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsTableViewDataSourceDelegateClass #-}
nsTableViewDataSourceDelegateClass :: Class
nsTableViewDataSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSTableViewDataSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_numberOfRowsInTableView = unSelector (mkSelector "numberOfRowsInTableView:")
      sel_tableView_objectValueForTableColumn_row = unSelector (mkSelector "tableView:objectValueForTableColumn:row:")
      sel_tableView_setObjectValue_forTableColumn_row = unSelector (mkSelector "tableView:setObjectValue:forTableColumn:row:")
      sel_tableView_sortDescriptorsDidChange = unSelector (mkSelector "tableView:sortDescriptorsDidChange:")
      sel_tableView_pasteboardWriterForRow = unSelector (mkSelector "tableView:pasteboardWriterForRow:")
      sel_tableView_updateDraggingItemsForDrag = unSelector (mkSelector "tableView:updateDraggingItemsForDrag:")
      sel_tableView_writeRowsWithIndexes_toPasteboard = unSelector (mkSelector "tableView:writeRowsWithIndexes:toPasteboard:")
      sel_tableView_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes = unSelector (mkSelector "tableView:namesOfPromisedFilesDroppedAtDestination:forDraggedRowsWithIndexes:")
  -- numberOfRowsInTableView:
  stub_0 <- wrap_at_q $ \self _cmd arg0 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDataSourceOverrides
    case _numberOfRowsInTableView rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0)
        pure (fromIntegral r)
  addObjCMethod cls "numberOfRowsInTableView:" "q@:@" stub_0

  -- tableView:objectValueForTableColumn:row:
  stub_1 <- wrap_at_at_q_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDataSourceOverrides
    case _tableView_objectValueForTableColumn_row rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (fromIntegral arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tableView:objectValueForTableColumn:row:" "@@:@@q" stub_1

  -- tableView:setObjectValue:forTableColumn:row:
  stub_2 <- wrap_at_at_at_q_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDataSourceOverrides
    case _tableView_setObjectValue_forTableColumn_row rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (fromIntegral arg3)
  addObjCMethod cls "tableView:setObjectValue:forTableColumn:row:" "v@:@@@q" stub_2

  -- tableView:sortDescriptorsDidChange:
  stub_3 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDataSourceOverrides
    case _tableView_sortDescriptorsDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "tableView:sortDescriptorsDidChange:" "v@:@@" stub_3

  -- tableView:pasteboardWriterForRow:
  stub_4 <- wrap_at_q_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDataSourceOverrides
    case _tableView_pasteboardWriterForRow rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tableView:pasteboardWriterForRow:" "@@:@q" stub_4

  -- tableView:updateDraggingItemsForDrag:
  stub_5 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDataSourceOverrides
    case _tableView_updateDraggingItemsForDrag rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "tableView:updateDraggingItemsForDrag:" "v@:@@" stub_5

  -- tableView:writeRowsWithIndexes:toPasteboard:
  stub_6 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDataSourceOverrides
    case _tableView_writeRowsWithIndexes_toPasteboard rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "tableView:writeRowsWithIndexes:toPasteboard:" "B@:@@@" stub_6

  -- tableView:namesOfPromisedFilesDroppedAtDestination:forDraggedRowsWithIndexes:
  stub_7 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDataSourceOverrides
    case _tableView_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "tableView:namesOfPromisedFilesDroppedAtDestination:forDraggedRowsWithIndexes:" "@@:@@@" stub_7

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSTableViewDataSourceOverrides
    if queriedSel == sel_numberOfRowsInTableView then pure (maybe 0 (const 1) (_numberOfRowsInTableView rec_))
    else if queriedSel == sel_tableView_objectValueForTableColumn_row then pure (maybe 0 (const 1) (_tableView_objectValueForTableColumn_row rec_))
    else if queriedSel == sel_tableView_setObjectValue_forTableColumn_row then pure (maybe 0 (const 1) (_tableView_setObjectValue_forTableColumn_row rec_))
    else if queriedSel == sel_tableView_sortDescriptorsDidChange then pure (maybe 0 (const 1) (_tableView_sortDescriptorsDidChange rec_))
    else if queriedSel == sel_tableView_pasteboardWriterForRow then pure (maybe 0 (const 1) (_tableView_pasteboardWriterForRow rec_))
    else if queriedSel == sel_tableView_updateDraggingItemsForDrag then pure (maybe 0 (const 1) (_tableView_updateDraggingItemsForDrag rec_))
    else if queriedSel == sel_tableView_writeRowsWithIndexes_toPasteboard then pure (maybe 0 (const 1) (_tableView_writeRowsWithIndexes_toPasteboard rec_))
    else if queriedSel == sel_tableView_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes then pure (maybe 0 (const 1) (_tableView_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes rec_))
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
newNSTableViewDataSource :: NSTableViewDataSourceOverrides -> IO RawId
newNSTableViewDataSource overrides = do
  inst <- class_createInstance nsTableViewDataSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
