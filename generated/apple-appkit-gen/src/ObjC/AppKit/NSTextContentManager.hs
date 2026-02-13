{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextContentManager@.
module ObjC.AppKit.NSTextContentManager
  ( NSTextContentManager
  , IsNSTextContentManager(..)
  , init_
  , initWithCoder
  , addTextLayoutManager
  , removeTextLayoutManager
  , synchronizeTextLayoutManagers
  , textElementsForRange
  , performEditingTransactionUsingBlock
  , recordEditActionInRange_newTextRange
  , delegate
  , setDelegate
  , textLayoutManagers
  , primaryTextLayoutManager
  , setPrimaryTextLayoutManager
  , hasEditingTransaction
  , automaticallySynchronizesTextLayoutManagers
  , setAutomaticallySynchronizesTextLayoutManagers
  , automaticallySynchronizesToBackingStore
  , setAutomaticallySynchronizesToBackingStore
  , addTextLayoutManagerSelector
  , automaticallySynchronizesTextLayoutManagersSelector
  , automaticallySynchronizesToBackingStoreSelector
  , delegateSelector
  , hasEditingTransactionSelector
  , initSelector
  , initWithCoderSelector
  , performEditingTransactionUsingBlockSelector
  , primaryTextLayoutManagerSelector
  , recordEditActionInRange_newTextRangeSelector
  , removeTextLayoutManagerSelector
  , setAutomaticallySynchronizesTextLayoutManagersSelector
  , setAutomaticallySynchronizesToBackingStoreSelector
  , setDelegateSelector
  , setPrimaryTextLayoutManagerSelector
  , synchronizeTextLayoutManagersSelector
  , textElementsForRangeSelector
  , textLayoutManagersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO (Id NSTextContentManager)
init_ nsTextContentManager =
  sendOwnedMessage nsTextContentManager initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextContentManager nsTextContentManager, IsNSCoder coder) => nsTextContentManager -> coder -> IO (Id NSTextContentManager)
initWithCoder nsTextContentManager coder =
  sendOwnedMessage nsTextContentManager initWithCoderSelector (toNSCoder coder)

-- | @- addTextLayoutManager:@
addTextLayoutManager :: (IsNSTextContentManager nsTextContentManager, IsNSTextLayoutManager textLayoutManager) => nsTextContentManager -> textLayoutManager -> IO ()
addTextLayoutManager nsTextContentManager textLayoutManager =
  sendMessage nsTextContentManager addTextLayoutManagerSelector (toNSTextLayoutManager textLayoutManager)

-- | @- removeTextLayoutManager:@
removeTextLayoutManager :: (IsNSTextContentManager nsTextContentManager, IsNSTextLayoutManager textLayoutManager) => nsTextContentManager -> textLayoutManager -> IO ()
removeTextLayoutManager nsTextContentManager textLayoutManager =
  sendMessage nsTextContentManager removeTextLayoutManagerSelector (toNSTextLayoutManager textLayoutManager)

-- | @- synchronizeTextLayoutManagers:@
synchronizeTextLayoutManagers :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> Ptr () -> IO ()
synchronizeTextLayoutManagers nsTextContentManager completionHandler =
  sendMessage nsTextContentManager synchronizeTextLayoutManagersSelector completionHandler

-- | @- textElementsForRange:@
textElementsForRange :: (IsNSTextContentManager nsTextContentManager, IsNSTextRange range) => nsTextContentManager -> range -> IO (Id NSArray)
textElementsForRange nsTextContentManager range =
  sendMessage nsTextContentManager textElementsForRangeSelector (toNSTextRange range)

-- | @- performEditingTransactionUsingBlock:@
performEditingTransactionUsingBlock :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> Ptr () -> IO ()
performEditingTransactionUsingBlock nsTextContentManager transaction =
  sendMessage nsTextContentManager performEditingTransactionUsingBlockSelector transaction

-- | @- recordEditActionInRange:newTextRange:@
recordEditActionInRange_newTextRange :: (IsNSTextContentManager nsTextContentManager, IsNSTextRange originalTextRange, IsNSTextRange newTextRange) => nsTextContentManager -> originalTextRange -> newTextRange -> IO ()
recordEditActionInRange_newTextRange nsTextContentManager originalTextRange newTextRange =
  sendMessage nsTextContentManager recordEditActionInRange_newTextRangeSelector (toNSTextRange originalTextRange) (toNSTextRange newTextRange)

-- | @- delegate@
delegate :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO RawId
delegate nsTextContentManager =
  sendMessage nsTextContentManager delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> RawId -> IO ()
setDelegate nsTextContentManager value =
  sendMessage nsTextContentManager setDelegateSelector value

-- | @- textLayoutManagers@
textLayoutManagers :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO (Id NSArray)
textLayoutManagers nsTextContentManager =
  sendMessage nsTextContentManager textLayoutManagersSelector

-- | @- primaryTextLayoutManager@
primaryTextLayoutManager :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO (Id NSTextLayoutManager)
primaryTextLayoutManager nsTextContentManager =
  sendMessage nsTextContentManager primaryTextLayoutManagerSelector

-- | @- setPrimaryTextLayoutManager:@
setPrimaryTextLayoutManager :: (IsNSTextContentManager nsTextContentManager, IsNSTextLayoutManager value) => nsTextContentManager -> value -> IO ()
setPrimaryTextLayoutManager nsTextContentManager value =
  sendMessage nsTextContentManager setPrimaryTextLayoutManagerSelector (toNSTextLayoutManager value)

-- | @- hasEditingTransaction@
hasEditingTransaction :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO Bool
hasEditingTransaction nsTextContentManager =
  sendMessage nsTextContentManager hasEditingTransactionSelector

-- | @- automaticallySynchronizesTextLayoutManagers@
automaticallySynchronizesTextLayoutManagers :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO Bool
automaticallySynchronizesTextLayoutManagers nsTextContentManager =
  sendMessage nsTextContentManager automaticallySynchronizesTextLayoutManagersSelector

-- | @- setAutomaticallySynchronizesTextLayoutManagers:@
setAutomaticallySynchronizesTextLayoutManagers :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> Bool -> IO ()
setAutomaticallySynchronizesTextLayoutManagers nsTextContentManager value =
  sendMessage nsTextContentManager setAutomaticallySynchronizesTextLayoutManagersSelector value

-- | @- automaticallySynchronizesToBackingStore@
automaticallySynchronizesToBackingStore :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO Bool
automaticallySynchronizesToBackingStore nsTextContentManager =
  sendMessage nsTextContentManager automaticallySynchronizesToBackingStoreSelector

-- | @- setAutomaticallySynchronizesToBackingStore:@
setAutomaticallySynchronizesToBackingStore :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> Bool -> IO ()
setAutomaticallySynchronizesToBackingStore nsTextContentManager value =
  sendMessage nsTextContentManager setAutomaticallySynchronizesToBackingStoreSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextContentManager)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTextContentManager)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @addTextLayoutManager:@
addTextLayoutManagerSelector :: Selector '[Id NSTextLayoutManager] ()
addTextLayoutManagerSelector = mkSelector "addTextLayoutManager:"

-- | @Selector@ for @removeTextLayoutManager:@
removeTextLayoutManagerSelector :: Selector '[Id NSTextLayoutManager] ()
removeTextLayoutManagerSelector = mkSelector "removeTextLayoutManager:"

-- | @Selector@ for @synchronizeTextLayoutManagers:@
synchronizeTextLayoutManagersSelector :: Selector '[Ptr ()] ()
synchronizeTextLayoutManagersSelector = mkSelector "synchronizeTextLayoutManagers:"

-- | @Selector@ for @textElementsForRange:@
textElementsForRangeSelector :: Selector '[Id NSTextRange] (Id NSArray)
textElementsForRangeSelector = mkSelector "textElementsForRange:"

-- | @Selector@ for @performEditingTransactionUsingBlock:@
performEditingTransactionUsingBlockSelector :: Selector '[Ptr ()] ()
performEditingTransactionUsingBlockSelector = mkSelector "performEditingTransactionUsingBlock:"

-- | @Selector@ for @recordEditActionInRange:newTextRange:@
recordEditActionInRange_newTextRangeSelector :: Selector '[Id NSTextRange, Id NSTextRange] ()
recordEditActionInRange_newTextRangeSelector = mkSelector "recordEditActionInRange:newTextRange:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @textLayoutManagers@
textLayoutManagersSelector :: Selector '[] (Id NSArray)
textLayoutManagersSelector = mkSelector "textLayoutManagers"

-- | @Selector@ for @primaryTextLayoutManager@
primaryTextLayoutManagerSelector :: Selector '[] (Id NSTextLayoutManager)
primaryTextLayoutManagerSelector = mkSelector "primaryTextLayoutManager"

-- | @Selector@ for @setPrimaryTextLayoutManager:@
setPrimaryTextLayoutManagerSelector :: Selector '[Id NSTextLayoutManager] ()
setPrimaryTextLayoutManagerSelector = mkSelector "setPrimaryTextLayoutManager:"

-- | @Selector@ for @hasEditingTransaction@
hasEditingTransactionSelector :: Selector '[] Bool
hasEditingTransactionSelector = mkSelector "hasEditingTransaction"

-- | @Selector@ for @automaticallySynchronizesTextLayoutManagers@
automaticallySynchronizesTextLayoutManagersSelector :: Selector '[] Bool
automaticallySynchronizesTextLayoutManagersSelector = mkSelector "automaticallySynchronizesTextLayoutManagers"

-- | @Selector@ for @setAutomaticallySynchronizesTextLayoutManagers:@
setAutomaticallySynchronizesTextLayoutManagersSelector :: Selector '[Bool] ()
setAutomaticallySynchronizesTextLayoutManagersSelector = mkSelector "setAutomaticallySynchronizesTextLayoutManagers:"

-- | @Selector@ for @automaticallySynchronizesToBackingStore@
automaticallySynchronizesToBackingStoreSelector :: Selector '[] Bool
automaticallySynchronizesToBackingStoreSelector = mkSelector "automaticallySynchronizesToBackingStore"

-- | @Selector@ for @setAutomaticallySynchronizesToBackingStore:@
setAutomaticallySynchronizesToBackingStoreSelector :: Selector '[Bool] ()
setAutomaticallySynchronizesToBackingStoreSelector = mkSelector "setAutomaticallySynchronizesToBackingStore:"

