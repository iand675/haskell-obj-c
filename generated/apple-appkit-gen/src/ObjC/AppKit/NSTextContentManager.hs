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
  , initSelector
  , initWithCoderSelector
  , addTextLayoutManagerSelector
  , removeTextLayoutManagerSelector
  , synchronizeTextLayoutManagersSelector
  , textElementsForRangeSelector
  , performEditingTransactionUsingBlockSelector
  , recordEditActionInRange_newTextRangeSelector
  , delegateSelector
  , setDelegateSelector
  , textLayoutManagersSelector
  , primaryTextLayoutManagerSelector
  , setPrimaryTextLayoutManagerSelector
  , hasEditingTransactionSelector
  , automaticallySynchronizesTextLayoutManagersSelector
  , setAutomaticallySynchronizesTextLayoutManagersSelector
  , automaticallySynchronizesToBackingStoreSelector
  , setAutomaticallySynchronizesToBackingStoreSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO (Id NSTextContentManager)
init_ nsTextContentManager  =
    sendMsg nsTextContentManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextContentManager nsTextContentManager, IsNSCoder coder) => nsTextContentManager -> coder -> IO (Id NSTextContentManager)
initWithCoder nsTextContentManager  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsTextContentManager (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- addTextLayoutManager:@
addTextLayoutManager :: (IsNSTextContentManager nsTextContentManager, IsNSTextLayoutManager textLayoutManager) => nsTextContentManager -> textLayoutManager -> IO ()
addTextLayoutManager nsTextContentManager  textLayoutManager =
  withObjCPtr textLayoutManager $ \raw_textLayoutManager ->
      sendMsg nsTextContentManager (mkSelector "addTextLayoutManager:") retVoid [argPtr (castPtr raw_textLayoutManager :: Ptr ())]

-- | @- removeTextLayoutManager:@
removeTextLayoutManager :: (IsNSTextContentManager nsTextContentManager, IsNSTextLayoutManager textLayoutManager) => nsTextContentManager -> textLayoutManager -> IO ()
removeTextLayoutManager nsTextContentManager  textLayoutManager =
  withObjCPtr textLayoutManager $ \raw_textLayoutManager ->
      sendMsg nsTextContentManager (mkSelector "removeTextLayoutManager:") retVoid [argPtr (castPtr raw_textLayoutManager :: Ptr ())]

-- | @- synchronizeTextLayoutManagers:@
synchronizeTextLayoutManagers :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> Ptr () -> IO ()
synchronizeTextLayoutManagers nsTextContentManager  completionHandler =
    sendMsg nsTextContentManager (mkSelector "synchronizeTextLayoutManagers:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- textElementsForRange:@
textElementsForRange :: (IsNSTextContentManager nsTextContentManager, IsNSTextRange range) => nsTextContentManager -> range -> IO (Id NSArray)
textElementsForRange nsTextContentManager  range =
  withObjCPtr range $ \raw_range ->
      sendMsg nsTextContentManager (mkSelector "textElementsForRange:") (retPtr retVoid) [argPtr (castPtr raw_range :: Ptr ())] >>= retainedObject . castPtr

-- | @- performEditingTransactionUsingBlock:@
performEditingTransactionUsingBlock :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> Ptr () -> IO ()
performEditingTransactionUsingBlock nsTextContentManager  transaction =
    sendMsg nsTextContentManager (mkSelector "performEditingTransactionUsingBlock:") retVoid [argPtr (castPtr transaction :: Ptr ())]

-- | @- recordEditActionInRange:newTextRange:@
recordEditActionInRange_newTextRange :: (IsNSTextContentManager nsTextContentManager, IsNSTextRange originalTextRange, IsNSTextRange newTextRange) => nsTextContentManager -> originalTextRange -> newTextRange -> IO ()
recordEditActionInRange_newTextRange nsTextContentManager  originalTextRange newTextRange =
  withObjCPtr originalTextRange $ \raw_originalTextRange ->
    withObjCPtr newTextRange $ \raw_newTextRange ->
        sendMsg nsTextContentManager (mkSelector "recordEditActionInRange:newTextRange:") retVoid [argPtr (castPtr raw_originalTextRange :: Ptr ()), argPtr (castPtr raw_newTextRange :: Ptr ())]

-- | @- delegate@
delegate :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO RawId
delegate nsTextContentManager  =
    fmap (RawId . castPtr) $ sendMsg nsTextContentManager (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> RawId -> IO ()
setDelegate nsTextContentManager  value =
    sendMsg nsTextContentManager (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- textLayoutManagers@
textLayoutManagers :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO (Id NSArray)
textLayoutManagers nsTextContentManager  =
    sendMsg nsTextContentManager (mkSelector "textLayoutManagers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- primaryTextLayoutManager@
primaryTextLayoutManager :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO (Id NSTextLayoutManager)
primaryTextLayoutManager nsTextContentManager  =
    sendMsg nsTextContentManager (mkSelector "primaryTextLayoutManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPrimaryTextLayoutManager:@
setPrimaryTextLayoutManager :: (IsNSTextContentManager nsTextContentManager, IsNSTextLayoutManager value) => nsTextContentManager -> value -> IO ()
setPrimaryTextLayoutManager nsTextContentManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsTextContentManager (mkSelector "setPrimaryTextLayoutManager:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hasEditingTransaction@
hasEditingTransaction :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO Bool
hasEditingTransaction nsTextContentManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextContentManager (mkSelector "hasEditingTransaction") retCULong []

-- | @- automaticallySynchronizesTextLayoutManagers@
automaticallySynchronizesTextLayoutManagers :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO Bool
automaticallySynchronizesTextLayoutManagers nsTextContentManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextContentManager (mkSelector "automaticallySynchronizesTextLayoutManagers") retCULong []

-- | @- setAutomaticallySynchronizesTextLayoutManagers:@
setAutomaticallySynchronizesTextLayoutManagers :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> Bool -> IO ()
setAutomaticallySynchronizesTextLayoutManagers nsTextContentManager  value =
    sendMsg nsTextContentManager (mkSelector "setAutomaticallySynchronizesTextLayoutManagers:") retVoid [argCULong (if value then 1 else 0)]

-- | @- automaticallySynchronizesToBackingStore@
automaticallySynchronizesToBackingStore :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> IO Bool
automaticallySynchronizesToBackingStore nsTextContentManager  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextContentManager (mkSelector "automaticallySynchronizesToBackingStore") retCULong []

-- | @- setAutomaticallySynchronizesToBackingStore:@
setAutomaticallySynchronizesToBackingStore :: IsNSTextContentManager nsTextContentManager => nsTextContentManager -> Bool -> IO ()
setAutomaticallySynchronizesToBackingStore nsTextContentManager  value =
    sendMsg nsTextContentManager (mkSelector "setAutomaticallySynchronizesToBackingStore:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @addTextLayoutManager:@
addTextLayoutManagerSelector :: Selector
addTextLayoutManagerSelector = mkSelector "addTextLayoutManager:"

-- | @Selector@ for @removeTextLayoutManager:@
removeTextLayoutManagerSelector :: Selector
removeTextLayoutManagerSelector = mkSelector "removeTextLayoutManager:"

-- | @Selector@ for @synchronizeTextLayoutManagers:@
synchronizeTextLayoutManagersSelector :: Selector
synchronizeTextLayoutManagersSelector = mkSelector "synchronizeTextLayoutManagers:"

-- | @Selector@ for @textElementsForRange:@
textElementsForRangeSelector :: Selector
textElementsForRangeSelector = mkSelector "textElementsForRange:"

-- | @Selector@ for @performEditingTransactionUsingBlock:@
performEditingTransactionUsingBlockSelector :: Selector
performEditingTransactionUsingBlockSelector = mkSelector "performEditingTransactionUsingBlock:"

-- | @Selector@ for @recordEditActionInRange:newTextRange:@
recordEditActionInRange_newTextRangeSelector :: Selector
recordEditActionInRange_newTextRangeSelector = mkSelector "recordEditActionInRange:newTextRange:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @textLayoutManagers@
textLayoutManagersSelector :: Selector
textLayoutManagersSelector = mkSelector "textLayoutManagers"

-- | @Selector@ for @primaryTextLayoutManager@
primaryTextLayoutManagerSelector :: Selector
primaryTextLayoutManagerSelector = mkSelector "primaryTextLayoutManager"

-- | @Selector@ for @setPrimaryTextLayoutManager:@
setPrimaryTextLayoutManagerSelector :: Selector
setPrimaryTextLayoutManagerSelector = mkSelector "setPrimaryTextLayoutManager:"

-- | @Selector@ for @hasEditingTransaction@
hasEditingTransactionSelector :: Selector
hasEditingTransactionSelector = mkSelector "hasEditingTransaction"

-- | @Selector@ for @automaticallySynchronizesTextLayoutManagers@
automaticallySynchronizesTextLayoutManagersSelector :: Selector
automaticallySynchronizesTextLayoutManagersSelector = mkSelector "automaticallySynchronizesTextLayoutManagers"

-- | @Selector@ for @setAutomaticallySynchronizesTextLayoutManagers:@
setAutomaticallySynchronizesTextLayoutManagersSelector :: Selector
setAutomaticallySynchronizesTextLayoutManagersSelector = mkSelector "setAutomaticallySynchronizesTextLayoutManagers:"

-- | @Selector@ for @automaticallySynchronizesToBackingStore@
automaticallySynchronizesToBackingStoreSelector :: Selector
automaticallySynchronizesToBackingStoreSelector = mkSelector "automaticallySynchronizesToBackingStore"

-- | @Selector@ for @setAutomaticallySynchronizesToBackingStore:@
setAutomaticallySynchronizesToBackingStoreSelector :: Selector
setAutomaticallySynchronizesToBackingStoreSelector = mkSelector "setAutomaticallySynchronizesToBackingStore:"

