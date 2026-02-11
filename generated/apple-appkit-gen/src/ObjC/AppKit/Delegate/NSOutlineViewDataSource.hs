{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSOutlineViewDataSource@.
--
-- Usage:
--
-- @
-- delegate <- newNSOutlineViewDataSource defaultNSOutlineViewDataSourceOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSOutlineViewDataSource
  ( NSOutlineViewDataSourceOverrides(..)
  , defaultNSOutlineViewDataSourceOverrides
  , newNSOutlineViewDataSource
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

-- | Overrides record for @\@protocol NSOutlineViewDataSource@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSOutlineViewDataSourceOverrides = NSOutlineViewDataSourceOverrides
  { _outlineView_numberOfChildrenOfItem :: !(Maybe (RawId -> RawId -> IO Int))
  , _outlineView_child_ofItem :: !(Maybe (RawId -> Int -> RawId -> IO RawId))
  , _outlineView_isItemExpandable :: !(Maybe (RawId -> RawId -> IO Bool))
  , _outlineView_objectValueForTableColumn_byItem :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _outlineView_setObjectValue_forTableColumn_byItem :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _outlineView_itemForPersistentObject :: !(Maybe (RawId -> RawId -> IO RawId))
  , _outlineView_persistentObjectForItem :: !(Maybe (RawId -> RawId -> IO RawId))
  , _outlineView_sortDescriptorsDidChange :: !(Maybe (RawId -> RawId -> IO ()))
  , _outlineView_pasteboardWriterForItem :: !(Maybe (RawId -> RawId -> IO RawId))
  , _outlineView_writeItems_toPasteboard :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _outlineView_updateDraggingItemsForDrag :: !(Maybe (RawId -> RawId -> IO ()))
  , _outlineView_acceptDrop_item_childIndex :: !(Maybe (RawId -> RawId -> RawId -> Int -> IO Bool))
  , _outlineView_namesOfPromisedFilesDroppedAtDestination_forDraggedItems :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSOutlineViewDataSourceOverrides :: NSOutlineViewDataSourceOverrides
defaultNSOutlineViewDataSourceOverrides = NSOutlineViewDataSourceOverrides
  { _outlineView_numberOfChildrenOfItem = Nothing
  , _outlineView_child_ofItem = Nothing
  , _outlineView_isItemExpandable = Nothing
  , _outlineView_objectValueForTableColumn_byItem = Nothing
  , _outlineView_setObjectValue_forTableColumn_byItem = Nothing
  , _outlineView_itemForPersistentObject = Nothing
  , _outlineView_persistentObjectForItem = Nothing
  , _outlineView_sortDescriptorsDidChange = Nothing
  , _outlineView_pasteboardWriterForItem = Nothing
  , _outlineView_writeItems_toPasteboard = Nothing
  , _outlineView_updateDraggingItemsForDrag = Nothing
  , _outlineView_acceptDrop_item_childIndex = Nothing
  , _outlineView_namesOfPromisedFilesDroppedAtDestination_forDraggedItems = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_q_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> CLong -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

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
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsOutlineViewDataSourceDelegateClass #-}
nsOutlineViewDataSourceDelegateClass :: Class
nsOutlineViewDataSourceDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSOutlineViewDataSource" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_outlineView_numberOfChildrenOfItem = unSelector (mkSelector "outlineView:numberOfChildrenOfItem:")
      sel_outlineView_child_ofItem = unSelector (mkSelector "outlineView:child:ofItem:")
      sel_outlineView_isItemExpandable = unSelector (mkSelector "outlineView:isItemExpandable:")
      sel_outlineView_objectValueForTableColumn_byItem = unSelector (mkSelector "outlineView:objectValueForTableColumn:byItem:")
      sel_outlineView_setObjectValue_forTableColumn_byItem = unSelector (mkSelector "outlineView:setObjectValue:forTableColumn:byItem:")
      sel_outlineView_itemForPersistentObject = unSelector (mkSelector "outlineView:itemForPersistentObject:")
      sel_outlineView_persistentObjectForItem = unSelector (mkSelector "outlineView:persistentObjectForItem:")
      sel_outlineView_sortDescriptorsDidChange = unSelector (mkSelector "outlineView:sortDescriptorsDidChange:")
      sel_outlineView_pasteboardWriterForItem = unSelector (mkSelector "outlineView:pasteboardWriterForItem:")
      sel_outlineView_writeItems_toPasteboard = unSelector (mkSelector "outlineView:writeItems:toPasteboard:")
      sel_outlineView_updateDraggingItemsForDrag = unSelector (mkSelector "outlineView:updateDraggingItemsForDrag:")
      sel_outlineView_acceptDrop_item_childIndex = unSelector (mkSelector "outlineView:acceptDrop:item:childIndex:")
      sel_outlineView_namesOfPromisedFilesDroppedAtDestination_forDraggedItems = unSelector (mkSelector "outlineView:namesOfPromisedFilesDroppedAtDestination:forDraggedItems:")
  -- outlineView:numberOfChildrenOfItem:
  stub_0 <- wrap_at_at_q $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_numberOfChildrenOfItem rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (fromIntegral r)
  addObjCMethod cls "outlineView:numberOfChildrenOfItem:" "q@:@@" stub_0

  -- outlineView:child:ofItem:
  stub_1 <- wrap_at_q_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_child_ofItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:child:ofItem:" "@@:@q@" stub_1

  -- outlineView:isItemExpandable:
  stub_2 <- wrap_at_at_B $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_isItemExpandable rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:isItemExpandable:" "B@:@@" stub_2

  -- outlineView:objectValueForTableColumn:byItem:
  stub_3 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_objectValueForTableColumn_byItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:objectValueForTableColumn:byItem:" "@@:@@@" stub_3

  -- outlineView:setObjectValue:forTableColumn:byItem:
  stub_4 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_setObjectValue_forTableColumn_byItem rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "outlineView:setObjectValue:forTableColumn:byItem:" "v@:@@@@" stub_4

  -- outlineView:itemForPersistentObject:
  stub_5 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_itemForPersistentObject rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:itemForPersistentObject:" "@@:@@" stub_5

  -- outlineView:persistentObjectForItem:
  stub_6 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_persistentObjectForItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:persistentObjectForItem:" "@@:@@" stub_6

  -- outlineView:sortDescriptorsDidChange:
  stub_7 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_sortDescriptorsDidChange rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "outlineView:sortDescriptorsDidChange:" "v@:@@" stub_7

  -- outlineView:pasteboardWriterForItem:
  stub_8 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_pasteboardWriterForItem rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:pasteboardWriterForItem:" "@@:@@" stub_8

  -- outlineView:writeItems:toPasteboard:
  stub_9 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_writeItems_toPasteboard rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:writeItems:toPasteboard:" "B@:@@@" stub_9

  -- outlineView:updateDraggingItemsForDrag:
  stub_10 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_updateDraggingItemsForDrag rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "outlineView:updateDraggingItemsForDrag:" "v@:@@" stub_10

  -- outlineView:acceptDrop:item:childIndex:
  stub_11 <- wrap_at_at_at_q_B $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_acceptDrop_item_childIndex rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (fromIntegral arg3)
        pure (if r then 1 else 0)
  addObjCMethod cls "outlineView:acceptDrop:item:childIndex:" "B@:@@@q" stub_11

  -- outlineView:namesOfPromisedFilesDroppedAtDestination:forDraggedItems:
  stub_12 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    case _outlineView_namesOfPromisedFilesDroppedAtDestination_forDraggedItems rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "outlineView:namesOfPromisedFilesDroppedAtDestination:forDraggedItems:" "@@:@@@" stub_12

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSOutlineViewDataSourceOverrides
    if queriedSel == sel_outlineView_numberOfChildrenOfItem then pure (maybe 0 (const 1) (_outlineView_numberOfChildrenOfItem rec_))
    else if queriedSel == sel_outlineView_child_ofItem then pure (maybe 0 (const 1) (_outlineView_child_ofItem rec_))
    else if queriedSel == sel_outlineView_isItemExpandable then pure (maybe 0 (const 1) (_outlineView_isItemExpandable rec_))
    else if queriedSel == sel_outlineView_objectValueForTableColumn_byItem then pure (maybe 0 (const 1) (_outlineView_objectValueForTableColumn_byItem rec_))
    else if queriedSel == sel_outlineView_setObjectValue_forTableColumn_byItem then pure (maybe 0 (const 1) (_outlineView_setObjectValue_forTableColumn_byItem rec_))
    else if queriedSel == sel_outlineView_itemForPersistentObject then pure (maybe 0 (const 1) (_outlineView_itemForPersistentObject rec_))
    else if queriedSel == sel_outlineView_persistentObjectForItem then pure (maybe 0 (const 1) (_outlineView_persistentObjectForItem rec_))
    else if queriedSel == sel_outlineView_sortDescriptorsDidChange then pure (maybe 0 (const 1) (_outlineView_sortDescriptorsDidChange rec_))
    else if queriedSel == sel_outlineView_pasteboardWriterForItem then pure (maybe 0 (const 1) (_outlineView_pasteboardWriterForItem rec_))
    else if queriedSel == sel_outlineView_writeItems_toPasteboard then pure (maybe 0 (const 1) (_outlineView_writeItems_toPasteboard rec_))
    else if queriedSel == sel_outlineView_updateDraggingItemsForDrag then pure (maybe 0 (const 1) (_outlineView_updateDraggingItemsForDrag rec_))
    else if queriedSel == sel_outlineView_acceptDrop_item_childIndex then pure (maybe 0 (const 1) (_outlineView_acceptDrop_item_childIndex rec_))
    else if queriedSel == sel_outlineView_namesOfPromisedFilesDroppedAtDestination_forDraggedItems then pure (maybe 0 (const 1) (_outlineView_namesOfPromisedFilesDroppedAtDestination_forDraggedItems rec_))
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
newNSOutlineViewDataSource :: NSOutlineViewDataSourceOverrides -> IO RawId
newNSOutlineViewDataSource overrides = do
  inst <- class_createInstance nsOutlineViewDataSourceDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
