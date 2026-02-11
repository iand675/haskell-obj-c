{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WebHistory
--
-- WebHistory is used to track pages that have been loaded    by WebKit.
--
-- Generated bindings for @WebHistory@.
module ObjC.WebKit.WebHistory
  ( WebHistory
  , IsWebHistory(..)
  , optionalSharedHistory
  , setOptionalSharedHistory
  , loadFromURL_error
  , saveToURL_error
  , addItems
  , removeItems
  , removeAllItems
  , orderedItemsLastVisitedOnDay
  , itemForURL
  , orderedLastVisitedDays
  , historyItemLimit
  , setHistoryItemLimit
  , historyAgeInDaysLimit
  , setHistoryAgeInDaysLimit
  , optionalSharedHistorySelector
  , setOptionalSharedHistorySelector
  , loadFromURL_errorSelector
  , saveToURL_errorSelector
  , addItemsSelector
  , removeItemsSelector
  , removeAllItemsSelector
  , orderedItemsLastVisitedOnDaySelector
  , itemForURLSelector
  , orderedLastVisitedDaysSelector
  , historyItemLimitSelector
  , setHistoryItemLimitSelector
  , historyAgeInDaysLimitSelector
  , setHistoryAgeInDaysLimitSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | optionalSharedHistory
--
-- Returns a shared WebHistory instance initialized with the default history file.
--
-- Returns: A WebHistory object.
--
-- ObjC selector: @+ optionalSharedHistory@
optionalSharedHistory :: IO (Id WebHistory)
optionalSharedHistory  =
  do
    cls' <- getRequiredClass "WebHistory"
    sendClassMsg cls' (mkSelector "optionalSharedHistory") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setOptionalSharedHistory:
--
-- @history@ — The history to use for the global WebHistory.
--
-- ObjC selector: @+ setOptionalSharedHistory:@
setOptionalSharedHistory :: IsWebHistory history => history -> IO ()
setOptionalSharedHistory history =
  do
    cls' <- getRequiredClass "WebHistory"
    withObjCPtr history $ \raw_history ->
      sendClassMsg cls' (mkSelector "setOptionalSharedHistory:") retVoid [argPtr (castPtr raw_history :: Ptr ())]

-- | loadFromURL:error:
--
-- @URL@ — The URL to use to initialize the WebHistory.
--
-- @error@ — Set to nil or an NSError instance if an error occurred.
--
-- The designated initializer for WebHistory.
--
-- Returns: Returns YES if successful, NO otherwise.
--
-- ObjC selector: @- loadFromURL:error:@
loadFromURL_error :: (IsWebHistory webHistory, IsNSURL url, IsNSError error_) => webHistory -> url -> error_ -> IO Bool
loadFromURL_error webHistory  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg webHistory (mkSelector "loadFromURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | saveToURL:error:
--
-- Save history to URL. It is the client's responsibility to call this at appropriate times.
--
-- @URL@ — The URL to use to save the WebHistory.
--
-- @error@ — Set to nil or an NSError instance if an error occurred.
--
-- Returns: Returns YES if successful, NO otherwise.
--
-- ObjC selector: @- saveToURL:error:@
saveToURL_error :: (IsWebHistory webHistory, IsNSURL url, IsNSError error_) => webHistory -> url -> error_ -> IO Bool
saveToURL_error webHistory  url error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg webHistory (mkSelector "saveToURL:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | addItems:
--
-- @newItems@ — An array of WebHistoryItems to add to the WebHistory.
--
-- ObjC selector: @- addItems:@
addItems :: (IsWebHistory webHistory, IsNSArray newItems) => webHistory -> newItems -> IO ()
addItems webHistory  newItems =
withObjCPtr newItems $ \raw_newItems ->
    sendMsg webHistory (mkSelector "addItems:") retVoid [argPtr (castPtr raw_newItems :: Ptr ())]

-- | removeItems:
--
-- @items@ — An array of WebHistoryItems to remove from the WebHistory.
--
-- ObjC selector: @- removeItems:@
removeItems :: (IsWebHistory webHistory, IsNSArray items) => webHistory -> items -> IO ()
removeItems webHistory  items =
withObjCPtr items $ \raw_items ->
    sendMsg webHistory (mkSelector "removeItems:") retVoid [argPtr (castPtr raw_items :: Ptr ())]

-- | removeAllItems
--
-- ObjC selector: @- removeAllItems@
removeAllItems :: IsWebHistory webHistory => webHistory -> IO ()
removeAllItems webHistory  =
  sendMsg webHistory (mkSelector "removeAllItems") retVoid []

-- | @- orderedItemsLastVisitedOnDay:@
orderedItemsLastVisitedOnDay :: (IsWebHistory webHistory, IsNSCalendarDate calendarDate) => webHistory -> calendarDate -> IO (Id NSArray)
orderedItemsLastVisitedOnDay webHistory  calendarDate =
withObjCPtr calendarDate $ \raw_calendarDate ->
    sendMsg webHistory (mkSelector "orderedItemsLastVisitedOnDay:") (retPtr retVoid) [argPtr (castPtr raw_calendarDate :: Ptr ())] >>= retainedObject . castPtr

-- | itemForURL:
--
-- Get an item for a specific URL
--
-- @URL@ — The URL of the history item to search for
--
-- Returns: Returns an item matching the URL
--
-- ObjC selector: @- itemForURL:@
itemForURL :: (IsWebHistory webHistory, IsNSURL url) => webHistory -> url -> IO (Id WebHistoryItem)
itemForURL webHistory  url =
withObjCPtr url $ \raw_url ->
    sendMsg webHistory (mkSelector "itemForURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | orderedLastVisitedDays
--
-- An array of NSCalendarDates for which history items exist in the WebHistory.
--
-- An array of NSCalendarDates, each one representing a unique day that contains one    or more history items, ordered from most recent to oldest.
--
-- ObjC selector: @- orderedLastVisitedDays@
orderedLastVisitedDays :: IsWebHistory webHistory => webHistory -> IO (Id NSArray)
orderedLastVisitedDays webHistory  =
  sendMsg webHistory (mkSelector "orderedLastVisitedDays") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | historyItemLimit
--
-- The maximum number of items that will be stored by the WebHistory.
--
-- ObjC selector: @- historyItemLimit@
historyItemLimit :: IsWebHistory webHistory => webHistory -> IO CInt
historyItemLimit webHistory  =
  sendMsg webHistory (mkSelector "historyItemLimit") retCInt []

-- | historyItemLimit
--
-- The maximum number of items that will be stored by the WebHistory.
--
-- ObjC selector: @- setHistoryItemLimit:@
setHistoryItemLimit :: IsWebHistory webHistory => webHistory -> CInt -> IO ()
setHistoryItemLimit webHistory  value =
  sendMsg webHistory (mkSelector "setHistoryItemLimit:") retVoid [argCInt (fromIntegral value)]

-- | historyAgeInDaysLimit
--
-- The maximum number of days to be read from stored history.
--
-- ObjC selector: @- historyAgeInDaysLimit@
historyAgeInDaysLimit :: IsWebHistory webHistory => webHistory -> IO CInt
historyAgeInDaysLimit webHistory  =
  sendMsg webHistory (mkSelector "historyAgeInDaysLimit") retCInt []

-- | historyAgeInDaysLimit
--
-- The maximum number of days to be read from stored history.
--
-- ObjC selector: @- setHistoryAgeInDaysLimit:@
setHistoryAgeInDaysLimit :: IsWebHistory webHistory => webHistory -> CInt -> IO ()
setHistoryAgeInDaysLimit webHistory  value =
  sendMsg webHistory (mkSelector "setHistoryAgeInDaysLimit:") retVoid [argCInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @optionalSharedHistory@
optionalSharedHistorySelector :: Selector
optionalSharedHistorySelector = mkSelector "optionalSharedHistory"

-- | @Selector@ for @setOptionalSharedHistory:@
setOptionalSharedHistorySelector :: Selector
setOptionalSharedHistorySelector = mkSelector "setOptionalSharedHistory:"

-- | @Selector@ for @loadFromURL:error:@
loadFromURL_errorSelector :: Selector
loadFromURL_errorSelector = mkSelector "loadFromURL:error:"

-- | @Selector@ for @saveToURL:error:@
saveToURL_errorSelector :: Selector
saveToURL_errorSelector = mkSelector "saveToURL:error:"

-- | @Selector@ for @addItems:@
addItemsSelector :: Selector
addItemsSelector = mkSelector "addItems:"

-- | @Selector@ for @removeItems:@
removeItemsSelector :: Selector
removeItemsSelector = mkSelector "removeItems:"

-- | @Selector@ for @removeAllItems@
removeAllItemsSelector :: Selector
removeAllItemsSelector = mkSelector "removeAllItems"

-- | @Selector@ for @orderedItemsLastVisitedOnDay:@
orderedItemsLastVisitedOnDaySelector :: Selector
orderedItemsLastVisitedOnDaySelector = mkSelector "orderedItemsLastVisitedOnDay:"

-- | @Selector@ for @itemForURL:@
itemForURLSelector :: Selector
itemForURLSelector = mkSelector "itemForURL:"

-- | @Selector@ for @orderedLastVisitedDays@
orderedLastVisitedDaysSelector :: Selector
orderedLastVisitedDaysSelector = mkSelector "orderedLastVisitedDays"

-- | @Selector@ for @historyItemLimit@
historyItemLimitSelector :: Selector
historyItemLimitSelector = mkSelector "historyItemLimit"

-- | @Selector@ for @setHistoryItemLimit:@
setHistoryItemLimitSelector :: Selector
setHistoryItemLimitSelector = mkSelector "setHistoryItemLimit:"

-- | @Selector@ for @historyAgeInDaysLimit@
historyAgeInDaysLimitSelector :: Selector
historyAgeInDaysLimitSelector = mkSelector "historyAgeInDaysLimit"

-- | @Selector@ for @setHistoryAgeInDaysLimit:@
setHistoryAgeInDaysLimitSelector :: Selector
setHistoryAgeInDaysLimitSelector = mkSelector "setHistoryAgeInDaysLimit:"

