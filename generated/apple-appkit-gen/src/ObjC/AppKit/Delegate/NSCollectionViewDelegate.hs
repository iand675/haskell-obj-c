{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Generated delegate overrides for @\@protocol NSCollectionViewDelegate@.
--
-- Usage:
--
-- @
-- delegate <- newNSCollectionViewDelegate defaultNSCollectionViewDelegateOverrides
--   { ... = Just $ \\arg0 -> ...
--   }
-- @
module ObjC.AppKit.Delegate.NSCollectionViewDelegate
  ( NSCollectionViewDelegateOverrides(..)
  , defaultNSCollectionViewDelegateOverrides
  , newNSCollectionViewDelegate
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

-- | Overrides record for @\@protocol NSCollectionViewDelegate@.
--
-- Each field corresponds to a protocol method.  'Nothing' means the
-- method is not implemented (the object will not respond to that
-- selector).  'Just' provides the Haskell implementation.
data NSCollectionViewDelegateOverrides = NSCollectionViewDelegateOverrides
  { _collectionView_canDragItemsAtIndexPaths_withEvent :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _collectionView_canDragItemsAtIndexes_withEvent :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _collectionView_writeItemsAtIndexPaths_toPasteboard :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _collectionView_writeItemsAtIndexes_toPasteboard :: !(Maybe (RawId -> RawId -> RawId -> IO Bool))
  , _collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexPaths :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexes :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  , _collectionView_draggingImageForItemsAtIndexPaths_withEvent_offset :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO RawId))
  , _collectionView_draggingImageForItemsAtIndexes_withEvent_offset :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO RawId))
  , _collectionView_pasteboardWriterForItemAtIndexPath :: !(Maybe (RawId -> RawId -> IO RawId))
  , _collectionView_pasteboardWriterForItemAtIndex :: !(Maybe (RawId -> Int -> IO RawId))
  , _collectionView_updateDraggingItemsForDrag :: !(Maybe (RawId -> RawId -> IO ()))
  , _collectionView_shouldSelectItemsAtIndexPaths :: !(Maybe (RawId -> RawId -> IO RawId))
  , _collectionView_shouldDeselectItemsAtIndexPaths :: !(Maybe (RawId -> RawId -> IO RawId))
  , _collectionView_didSelectItemsAtIndexPaths :: !(Maybe (RawId -> RawId -> IO ()))
  , _collectionView_didDeselectItemsAtIndexPaths :: !(Maybe (RawId -> RawId -> IO ()))
  , _collectionView_willDisplayItem_forRepresentedObjectAtIndexPath :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _collectionView_willDisplaySupplementaryView_forElementKind_atIndexPath :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _collectionView_didEndDisplayingItem_forRepresentedObjectAtIndexPath :: !(Maybe (RawId -> RawId -> RawId -> IO ()))
  , _collectionView_didEndDisplayingSupplementaryView_forElementOfKind_atIndexPath :: !(Maybe (RawId -> RawId -> RawId -> RawId -> IO ()))
  , _collectionView_transitionLayoutForOldLayout_newLayout :: !(Maybe (RawId -> RawId -> RawId -> IO RawId))
  }

-- | Default overrides with all methods unimplemented.
defaultNSCollectionViewDelegateOverrides :: NSCollectionViewDelegateOverrides
defaultNSCollectionViewDelegateOverrides = NSCollectionViewDelegateOverrides
  { _collectionView_canDragItemsAtIndexPaths_withEvent = Nothing
  , _collectionView_canDragItemsAtIndexes_withEvent = Nothing
  , _collectionView_writeItemsAtIndexPaths_toPasteboard = Nothing
  , _collectionView_writeItemsAtIndexes_toPasteboard = Nothing
  , _collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexPaths = Nothing
  , _collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexes = Nothing
  , _collectionView_draggingImageForItemsAtIndexPaths_withEvent_offset = Nothing
  , _collectionView_draggingImageForItemsAtIndexes_withEvent_offset = Nothing
  , _collectionView_pasteboardWriterForItemAtIndexPath = Nothing
  , _collectionView_pasteboardWriterForItemAtIndex = Nothing
  , _collectionView_updateDraggingItemsForDrag = Nothing
  , _collectionView_shouldSelectItemsAtIndexPaths = Nothing
  , _collectionView_shouldDeselectItemsAtIndexPaths = Nothing
  , _collectionView_didSelectItemsAtIndexPaths = Nothing
  , _collectionView_didDeselectItemsAtIndexPaths = Nothing
  , _collectionView_willDisplayItem_forRepresentedObjectAtIndexPath = Nothing
  , _collectionView_willDisplaySupplementaryView_forElementKind_atIndexPath = Nothing
  , _collectionView_didEndDisplayingItem_forRepresentedObjectAtIndexPath = Nothing
  , _collectionView_didEndDisplayingSupplementaryView_forElementOfKind_atIndexPath = Nothing
  , _collectionView_transitionLayoutForOldLayout_newLayout = Nothing
  }

foreign import ccall "wrapper"
  wrap_at_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_at_v
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ())
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO ()))

foreign import ccall "wrapper"
  wrap_at_Q_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> CULong -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_at
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject))
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO (Ptr ObjCObject)))

foreign import ccall "wrapper"
  wrap_at_at_at_B
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCObject -> Ptr ObjCObject -> Ptr ObjCObject -> IO CULong))


foreign import ccall "wrapper"
  wrap_respondsToSel
    :: (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong)
    -> IO (FunPtr (Ptr ObjCObject -> Ptr ObjCSel -> Ptr ObjCSel -> IO CULong))

-- | The ObjC class for this delegate.  Created once, shared by all instances.
{-# NOINLINE nsCollectionViewDelegateDelegateClass #-}
nsCollectionViewDelegateDelegateClass :: Class
nsCollectionViewDelegateDelegateClass = unsafePerformIO $ do
  superCls <- getRequiredClass "NSObject"
  cls <- withCString "HsNSCollectionViewDelegate" $ \n ->
    objc_allocateClassPair superCls n 0
  addHsDataIvar cls
  let sel_collectionView_canDragItemsAtIndexPaths_withEvent = unSelector (mkSelector "collectionView:canDragItemsAtIndexPaths:withEvent:")
      sel_collectionView_canDragItemsAtIndexes_withEvent = unSelector (mkSelector "collectionView:canDragItemsAtIndexes:withEvent:")
      sel_collectionView_writeItemsAtIndexPaths_toPasteboard = unSelector (mkSelector "collectionView:writeItemsAtIndexPaths:toPasteboard:")
      sel_collectionView_writeItemsAtIndexes_toPasteboard = unSelector (mkSelector "collectionView:writeItemsAtIndexes:toPasteboard:")
      sel_collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexPaths = unSelector (mkSelector "collectionView:namesOfPromisedFilesDroppedAtDestination:forDraggedItemsAtIndexPaths:")
      sel_collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexes = unSelector (mkSelector "collectionView:namesOfPromisedFilesDroppedAtDestination:forDraggedItemsAtIndexes:")
      sel_collectionView_draggingImageForItemsAtIndexPaths_withEvent_offset = unSelector (mkSelector "collectionView:draggingImageForItemsAtIndexPaths:withEvent:offset:")
      sel_collectionView_draggingImageForItemsAtIndexes_withEvent_offset = unSelector (mkSelector "collectionView:draggingImageForItemsAtIndexes:withEvent:offset:")
      sel_collectionView_pasteboardWriterForItemAtIndexPath = unSelector (mkSelector "collectionView:pasteboardWriterForItemAtIndexPath:")
      sel_collectionView_pasteboardWriterForItemAtIndex = unSelector (mkSelector "collectionView:pasteboardWriterForItemAtIndex:")
      sel_collectionView_updateDraggingItemsForDrag = unSelector (mkSelector "collectionView:updateDraggingItemsForDrag:")
      sel_collectionView_shouldSelectItemsAtIndexPaths = unSelector (mkSelector "collectionView:shouldSelectItemsAtIndexPaths:")
      sel_collectionView_shouldDeselectItemsAtIndexPaths = unSelector (mkSelector "collectionView:shouldDeselectItemsAtIndexPaths:")
      sel_collectionView_didSelectItemsAtIndexPaths = unSelector (mkSelector "collectionView:didSelectItemsAtIndexPaths:")
      sel_collectionView_didDeselectItemsAtIndexPaths = unSelector (mkSelector "collectionView:didDeselectItemsAtIndexPaths:")
      sel_collectionView_willDisplayItem_forRepresentedObjectAtIndexPath = unSelector (mkSelector "collectionView:willDisplayItem:forRepresentedObjectAtIndexPath:")
      sel_collectionView_willDisplaySupplementaryView_forElementKind_atIndexPath = unSelector (mkSelector "collectionView:willDisplaySupplementaryView:forElementKind:atIndexPath:")
      sel_collectionView_didEndDisplayingItem_forRepresentedObjectAtIndexPath = unSelector (mkSelector "collectionView:didEndDisplayingItem:forRepresentedObjectAtIndexPath:")
      sel_collectionView_didEndDisplayingSupplementaryView_forElementOfKind_atIndexPath = unSelector (mkSelector "collectionView:didEndDisplayingSupplementaryView:forElementOfKind:atIndexPath:")
      sel_collectionView_transitionLayoutForOldLayout_newLayout = unSelector (mkSelector "collectionView:transitionLayoutForOldLayout:newLayout:")
  -- collectionView:canDragItemsAtIndexPaths:withEvent:
  stub_0 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_canDragItemsAtIndexPaths_withEvent rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "collectionView:canDragItemsAtIndexPaths:withEvent:" "B@:@@@" stub_0

  -- collectionView:canDragItemsAtIndexes:withEvent:
  stub_1 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_canDragItemsAtIndexes_withEvent rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "collectionView:canDragItemsAtIndexes:withEvent:" "B@:@@@" stub_1

  -- collectionView:writeItemsAtIndexPaths:toPasteboard:
  stub_2 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_writeItemsAtIndexPaths_toPasteboard rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "collectionView:writeItemsAtIndexPaths:toPasteboard:" "B@:@@@" stub_2

  -- collectionView:writeItemsAtIndexes:toPasteboard:
  stub_3 <- wrap_at_at_at_B $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_writeItemsAtIndexes_toPasteboard rec_ of
      Nothing -> pure 0
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (if r then 1 else 0)
  addObjCMethod cls "collectionView:writeItemsAtIndexes:toPasteboard:" "B@:@@@" stub_3

  -- collectionView:namesOfPromisedFilesDroppedAtDestination:forDraggedItemsAtIndexPaths:
  stub_4 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexPaths rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "collectionView:namesOfPromisedFilesDroppedAtDestination:forDraggedItemsAtIndexPaths:" "@@:@@@" stub_4

  -- collectionView:namesOfPromisedFilesDroppedAtDestination:forDraggedItemsAtIndexes:
  stub_5 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexes rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "collectionView:namesOfPromisedFilesDroppedAtDestination:forDraggedItemsAtIndexes:" "@@:@@@" stub_5

  -- collectionView:draggingImageForItemsAtIndexPaths:withEvent:offset:
  stub_6 <- wrap_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_draggingImageForItemsAtIndexPaths_withEvent_offset rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "collectionView:draggingImageForItemsAtIndexPaths:withEvent:offset:" "@@:@@@@" stub_6

  -- collectionView:draggingImageForItemsAtIndexes:withEvent:offset:
  stub_7 <- wrap_at_at_at_at_at $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_draggingImageForItemsAtIndexes_withEvent_offset rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "collectionView:draggingImageForItemsAtIndexes:withEvent:offset:" "@@:@@@@" stub_7

  -- collectionView:pasteboardWriterForItemAtIndexPath:
  stub_8 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_pasteboardWriterForItemAtIndexPath rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "collectionView:pasteboardWriterForItemAtIndexPath:" "@@:@@" stub_8

  -- collectionView:pasteboardWriterForItemAtIndex:
  stub_9 <- wrap_at_Q_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_pasteboardWriterForItemAtIndex rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (fromIntegral arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "collectionView:pasteboardWriterForItemAtIndex:" "@@:@Q" stub_9

  -- collectionView:updateDraggingItemsForDrag:
  stub_10 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_updateDraggingItemsForDrag rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "collectionView:updateDraggingItemsForDrag:" "v@:@@" stub_10

  -- collectionView:shouldSelectItemsAtIndexPaths:
  stub_11 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_shouldSelectItemsAtIndexPaths rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "collectionView:shouldSelectItemsAtIndexPaths:" "@@:@@" stub_11

  -- collectionView:shouldDeselectItemsAtIndexPaths:
  stub_12 <- wrap_at_at_at $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_shouldDeselectItemsAtIndexPaths rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "collectionView:shouldDeselectItemsAtIndexPaths:" "@@:@@" stub_12

  -- collectionView:didSelectItemsAtIndexPaths:
  stub_13 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_didSelectItemsAtIndexPaths rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "collectionView:didSelectItemsAtIndexPaths:" "v@:@@" stub_13

  -- collectionView:didDeselectItemsAtIndexPaths:
  stub_14 <- wrap_at_at_v $ \self _cmd arg0 arg1 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_didDeselectItemsAtIndexPaths rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1)
  addObjCMethod cls "collectionView:didDeselectItemsAtIndexPaths:" "v@:@@" stub_14

  -- collectionView:willDisplayItem:forRepresentedObjectAtIndexPath:
  stub_15 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_willDisplayItem_forRepresentedObjectAtIndexPath rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "collectionView:willDisplayItem:forRepresentedObjectAtIndexPath:" "v@:@@@" stub_15

  -- collectionView:willDisplaySupplementaryView:forElementKind:atIndexPath:
  stub_16 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_willDisplaySupplementaryView_forElementKind_atIndexPath rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "collectionView:willDisplaySupplementaryView:forElementKind:atIndexPath:" "v@:@@@@" stub_16

  -- collectionView:didEndDisplayingItem:forRepresentedObjectAtIndexPath:
  stub_17 <- wrap_at_at_at_v $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_didEndDisplayingItem_forRepresentedObjectAtIndexPath rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2)
  addObjCMethod cls "collectionView:didEndDisplayingItem:forRepresentedObjectAtIndexPath:" "v@:@@@" stub_17

  -- collectionView:didEndDisplayingSupplementaryView:forElementOfKind:atIndexPath:
  stub_18 <- wrap_at_at_at_at_v $ \self _cmd arg0 arg1 arg2 arg3 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_didEndDisplayingSupplementaryView_forElementOfKind_atIndexPath rec_ of
      Nothing -> pure ()
      Just f -> f (RawId arg0) (RawId arg1) (RawId arg2) (RawId arg3)
  addObjCMethod cls "collectionView:didEndDisplayingSupplementaryView:forElementOfKind:atIndexPath:" "v@:@@@@" stub_18

  -- collectionView:transitionLayoutForOldLayout:newLayout:
  stub_19 <- wrap_at_at_at_at $ \self _cmd arg0 arg1 arg2 -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    case _collectionView_transitionLayoutForOldLayout_newLayout rec_ of
      Nothing -> pure nullPtr
      Just f -> do
        r <- f (RawId arg0) (RawId arg1) (RawId arg2)
        pure (castPtr (unRawId r) :: Ptr ObjCObject)
  addObjCMethod cls "collectionView:transitionLayoutForOldLayout:newLayout:" "@@:@@@" stub_19

  -- respondsToSelector: override
  rtsStub <- wrap_respondsToSel $ \self _cmd queriedSel -> do
    sp <- readHsData self
    rec_ <- deRefStablePtr sp :: IO NSCollectionViewDelegateOverrides
    if queriedSel == sel_collectionView_canDragItemsAtIndexPaths_withEvent then pure (maybe 0 (const 1) (_collectionView_canDragItemsAtIndexPaths_withEvent rec_))
    else if queriedSel == sel_collectionView_canDragItemsAtIndexes_withEvent then pure (maybe 0 (const 1) (_collectionView_canDragItemsAtIndexes_withEvent rec_))
    else if queriedSel == sel_collectionView_writeItemsAtIndexPaths_toPasteboard then pure (maybe 0 (const 1) (_collectionView_writeItemsAtIndexPaths_toPasteboard rec_))
    else if queriedSel == sel_collectionView_writeItemsAtIndexes_toPasteboard then pure (maybe 0 (const 1) (_collectionView_writeItemsAtIndexes_toPasteboard rec_))
    else if queriedSel == sel_collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexPaths then pure (maybe 0 (const 1) (_collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexPaths rec_))
    else if queriedSel == sel_collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexes then pure (maybe 0 (const 1) (_collectionView_namesOfPromisedFilesDroppedAtDestination_forDraggedItemsAtIndexes rec_))
    else if queriedSel == sel_collectionView_draggingImageForItemsAtIndexPaths_withEvent_offset then pure (maybe 0 (const 1) (_collectionView_draggingImageForItemsAtIndexPaths_withEvent_offset rec_))
    else if queriedSel == sel_collectionView_draggingImageForItemsAtIndexes_withEvent_offset then pure (maybe 0 (const 1) (_collectionView_draggingImageForItemsAtIndexes_withEvent_offset rec_))
    else if queriedSel == sel_collectionView_pasteboardWriterForItemAtIndexPath then pure (maybe 0 (const 1) (_collectionView_pasteboardWriterForItemAtIndexPath rec_))
    else if queriedSel == sel_collectionView_pasteboardWriterForItemAtIndex then pure (maybe 0 (const 1) (_collectionView_pasteboardWriterForItemAtIndex rec_))
    else if queriedSel == sel_collectionView_updateDraggingItemsForDrag then pure (maybe 0 (const 1) (_collectionView_updateDraggingItemsForDrag rec_))
    else if queriedSel == sel_collectionView_shouldSelectItemsAtIndexPaths then pure (maybe 0 (const 1) (_collectionView_shouldSelectItemsAtIndexPaths rec_))
    else if queriedSel == sel_collectionView_shouldDeselectItemsAtIndexPaths then pure (maybe 0 (const 1) (_collectionView_shouldDeselectItemsAtIndexPaths rec_))
    else if queriedSel == sel_collectionView_didSelectItemsAtIndexPaths then pure (maybe 0 (const 1) (_collectionView_didSelectItemsAtIndexPaths rec_))
    else if queriedSel == sel_collectionView_didDeselectItemsAtIndexPaths then pure (maybe 0 (const 1) (_collectionView_didDeselectItemsAtIndexPaths rec_))
    else if queriedSel == sel_collectionView_willDisplayItem_forRepresentedObjectAtIndexPath then pure (maybe 0 (const 1) (_collectionView_willDisplayItem_forRepresentedObjectAtIndexPath rec_))
    else if queriedSel == sel_collectionView_willDisplaySupplementaryView_forElementKind_atIndexPath then pure (maybe 0 (const 1) (_collectionView_willDisplaySupplementaryView_forElementKind_atIndexPath rec_))
    else if queriedSel == sel_collectionView_didEndDisplayingItem_forRepresentedObjectAtIndexPath then pure (maybe 0 (const 1) (_collectionView_didEndDisplayingItem_forRepresentedObjectAtIndexPath rec_))
    else if queriedSel == sel_collectionView_didEndDisplayingSupplementaryView_forElementOfKind_atIndexPath then pure (maybe 0 (const 1) (_collectionView_didEndDisplayingSupplementaryView_forElementOfKind_atIndexPath rec_))
    else if queriedSel == sel_collectionView_transitionLayoutForOldLayout_newLayout then pure (maybe 0 (const 1) (_collectionView_transitionLayoutForOldLayout_newLayout rec_))
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
newNSCollectionViewDelegate :: NSCollectionViewDelegateOverrides -> IO RawId
newNSCollectionViewDelegate overrides = do
  inst <- class_createInstance nsCollectionViewDelegateDelegateClass 0
  sp <- newStablePtr overrides
  writeHsData inst sp
  pure inst
