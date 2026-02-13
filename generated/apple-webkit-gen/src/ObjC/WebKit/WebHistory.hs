{-# LANGUAGE DataKinds #-}
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
  , addItemsSelector
  , historyAgeInDaysLimitSelector
  , historyItemLimitSelector
  , itemForURLSelector
  , loadFromURL_errorSelector
  , optionalSharedHistorySelector
  , orderedItemsLastVisitedOnDaySelector
  , orderedLastVisitedDaysSelector
  , removeAllItemsSelector
  , removeItemsSelector
  , saveToURL_errorSelector
  , setHistoryAgeInDaysLimitSelector
  , setHistoryItemLimitSelector
  , setOptionalSharedHistorySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' optionalSharedHistorySelector

-- | setOptionalSharedHistory:
--
-- @history@ — The history to use for the global WebHistory.
--
-- ObjC selector: @+ setOptionalSharedHistory:@
setOptionalSharedHistory :: IsWebHistory history => history -> IO ()
setOptionalSharedHistory history =
  do
    cls' <- getRequiredClass "WebHistory"
    sendClassMessage cls' setOptionalSharedHistorySelector (toWebHistory history)

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
loadFromURL_error webHistory url error_ =
  sendMessage webHistory loadFromURL_errorSelector (toNSURL url) (toNSError error_)

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
saveToURL_error webHistory url error_ =
  sendMessage webHistory saveToURL_errorSelector (toNSURL url) (toNSError error_)

-- | addItems:
--
-- @newItems@ — An array of WebHistoryItems to add to the WebHistory.
--
-- ObjC selector: @- addItems:@
addItems :: (IsWebHistory webHistory, IsNSArray newItems) => webHistory -> newItems -> IO ()
addItems webHistory newItems =
  sendMessage webHistory addItemsSelector (toNSArray newItems)

-- | removeItems:
--
-- @items@ — An array of WebHistoryItems to remove from the WebHistory.
--
-- ObjC selector: @- removeItems:@
removeItems :: (IsWebHistory webHistory, IsNSArray items) => webHistory -> items -> IO ()
removeItems webHistory items =
  sendMessage webHistory removeItemsSelector (toNSArray items)

-- | removeAllItems
--
-- ObjC selector: @- removeAllItems@
removeAllItems :: IsWebHistory webHistory => webHistory -> IO ()
removeAllItems webHistory =
  sendMessage webHistory removeAllItemsSelector

-- | @- orderedItemsLastVisitedOnDay:@
orderedItemsLastVisitedOnDay :: (IsWebHistory webHistory, IsNSCalendarDate calendarDate) => webHistory -> calendarDate -> IO (Id NSArray)
orderedItemsLastVisitedOnDay webHistory calendarDate =
  sendMessage webHistory orderedItemsLastVisitedOnDaySelector (toNSCalendarDate calendarDate)

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
itemForURL webHistory url =
  sendMessage webHistory itemForURLSelector (toNSURL url)

-- | orderedLastVisitedDays
--
-- An array of NSCalendarDates for which history items exist in the WebHistory.
--
-- An array of NSCalendarDates, each one representing a unique day that contains one    or more history items, ordered from most recent to oldest.
--
-- ObjC selector: @- orderedLastVisitedDays@
orderedLastVisitedDays :: IsWebHistory webHistory => webHistory -> IO (Id NSArray)
orderedLastVisitedDays webHistory =
  sendMessage webHistory orderedLastVisitedDaysSelector

-- | historyItemLimit
--
-- The maximum number of items that will be stored by the WebHistory.
--
-- ObjC selector: @- historyItemLimit@
historyItemLimit :: IsWebHistory webHistory => webHistory -> IO CInt
historyItemLimit webHistory =
  sendMessage webHistory historyItemLimitSelector

-- | historyItemLimit
--
-- The maximum number of items that will be stored by the WebHistory.
--
-- ObjC selector: @- setHistoryItemLimit:@
setHistoryItemLimit :: IsWebHistory webHistory => webHistory -> CInt -> IO ()
setHistoryItemLimit webHistory value =
  sendMessage webHistory setHistoryItemLimitSelector value

-- | historyAgeInDaysLimit
--
-- The maximum number of days to be read from stored history.
--
-- ObjC selector: @- historyAgeInDaysLimit@
historyAgeInDaysLimit :: IsWebHistory webHistory => webHistory -> IO CInt
historyAgeInDaysLimit webHistory =
  sendMessage webHistory historyAgeInDaysLimitSelector

-- | historyAgeInDaysLimit
--
-- The maximum number of days to be read from stored history.
--
-- ObjC selector: @- setHistoryAgeInDaysLimit:@
setHistoryAgeInDaysLimit :: IsWebHistory webHistory => webHistory -> CInt -> IO ()
setHistoryAgeInDaysLimit webHistory value =
  sendMessage webHistory setHistoryAgeInDaysLimitSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @optionalSharedHistory@
optionalSharedHistorySelector :: Selector '[] (Id WebHistory)
optionalSharedHistorySelector = mkSelector "optionalSharedHistory"

-- | @Selector@ for @setOptionalSharedHistory:@
setOptionalSharedHistorySelector :: Selector '[Id WebHistory] ()
setOptionalSharedHistorySelector = mkSelector "setOptionalSharedHistory:"

-- | @Selector@ for @loadFromURL:error:@
loadFromURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
loadFromURL_errorSelector = mkSelector "loadFromURL:error:"

-- | @Selector@ for @saveToURL:error:@
saveToURL_errorSelector :: Selector '[Id NSURL, Id NSError] Bool
saveToURL_errorSelector = mkSelector "saveToURL:error:"

-- | @Selector@ for @addItems:@
addItemsSelector :: Selector '[Id NSArray] ()
addItemsSelector = mkSelector "addItems:"

-- | @Selector@ for @removeItems:@
removeItemsSelector :: Selector '[Id NSArray] ()
removeItemsSelector = mkSelector "removeItems:"

-- | @Selector@ for @removeAllItems@
removeAllItemsSelector :: Selector '[] ()
removeAllItemsSelector = mkSelector "removeAllItems"

-- | @Selector@ for @orderedItemsLastVisitedOnDay:@
orderedItemsLastVisitedOnDaySelector :: Selector '[Id NSCalendarDate] (Id NSArray)
orderedItemsLastVisitedOnDaySelector = mkSelector "orderedItemsLastVisitedOnDay:"

-- | @Selector@ for @itemForURL:@
itemForURLSelector :: Selector '[Id NSURL] (Id WebHistoryItem)
itemForURLSelector = mkSelector "itemForURL:"

-- | @Selector@ for @orderedLastVisitedDays@
orderedLastVisitedDaysSelector :: Selector '[] (Id NSArray)
orderedLastVisitedDaysSelector = mkSelector "orderedLastVisitedDays"

-- | @Selector@ for @historyItemLimit@
historyItemLimitSelector :: Selector '[] CInt
historyItemLimitSelector = mkSelector "historyItemLimit"

-- | @Selector@ for @setHistoryItemLimit:@
setHistoryItemLimitSelector :: Selector '[CInt] ()
setHistoryItemLimitSelector = mkSelector "setHistoryItemLimit:"

-- | @Selector@ for @historyAgeInDaysLimit@
historyAgeInDaysLimitSelector :: Selector '[] CInt
historyAgeInDaysLimitSelector = mkSelector "historyAgeInDaysLimit"

-- | @Selector@ for @setHistoryAgeInDaysLimit:@
setHistoryAgeInDaysLimitSelector :: Selector '[CInt] ()
setHistoryAgeInDaysLimitSelector = mkSelector "setHistoryAgeInDaysLimit:"

