{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WebHistoryItem
--
-- WebHistoryItems are created by WebKit to represent pages visited.    The WebBackForwardList and WebHistory classes both use WebHistoryItems to represent    pages visited.  With the exception of the displayTitle, the properties of     WebHistoryItems are set by WebKit.  WebHistoryItems are normally never created directly.
--
-- Generated bindings for @WebHistoryItem@.
module ObjC.WebKit.WebHistoryItem
  ( WebHistoryItem
  , IsWebHistoryItem(..)
  , initWithURLString_title_lastVisitedTimeInterval
  , originalURLString
  , urlString
  , title
  , lastVisitedTimeInterval
  , alternateTitle
  , setAlternateTitle
  , icon
  , alternateTitleSelector
  , iconSelector
  , initWithURLString_title_lastVisitedTimeIntervalSelector
  , lastVisitedTimeIntervalSelector
  , originalURLStringSelector
  , setAlternateTitleSelector
  , titleSelector
  , urlStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithURLString:title:lastVisitedTimeInterval:
--
-- @URLString@ — The URL string for the item.
--
-- @title@ — The title to use for the item.  This is normally the <title> of a page.
--
-- @time@ — The time used to indicate when the item was used.
--
-- Initialize a new WebHistoryItem
--
-- WebHistoryItems are normally created for you by the WebKit.    You may use this method to prepopulate a WebBackForwardList, or create    'artificial' items to add to a WebBackForwardList.  When first initialized    the URLString and originalURLString will be the same.
--
-- ObjC selector: @- initWithURLString:title:lastVisitedTimeInterval:@
initWithURLString_title_lastVisitedTimeInterval :: (IsWebHistoryItem webHistoryItem, IsNSString urlString, IsNSString title) => webHistoryItem -> urlString -> title -> CDouble -> IO (Id WebHistoryItem)
initWithURLString_title_lastVisitedTimeInterval webHistoryItem urlString title time =
  sendOwnedMessage webHistoryItem initWithURLString_title_lastVisitedTimeIntervalSelector (toNSString urlString) (toNSString title) time

-- | originalURLString
--
-- The string representation of the initial URL of this item.    This value is normally set by the WebKit.
--
-- ObjC selector: @- originalURLString@
originalURLString :: IsWebHistoryItem webHistoryItem => webHistoryItem -> IO (Id NSString)
originalURLString webHistoryItem =
  sendMessage webHistoryItem originalURLStringSelector

-- | URLString
--
-- The string representation of the URL represented by this item.
--
-- The URLString may be different than the originalURLString if the page    redirected to a new location.  This value is normally set by the WebKit.
--
-- ObjC selector: @- URLString@
urlString :: IsWebHistoryItem webHistoryItem => webHistoryItem -> IO (Id NSString)
urlString webHistoryItem =
  sendMessage webHistoryItem urlStringSelector

-- | title
--
-- The title of the page represented by this item.
--
-- This title cannot be changed by the client.  This value    is normally set by the WebKit when a page title for the item is received.
--
-- ObjC selector: @- title@
title :: IsWebHistoryItem webHistoryItem => webHistoryItem -> IO (Id NSString)
title webHistoryItem =
  sendMessage webHistoryItem titleSelector

-- | lastVisitedTimeInterval
--
-- The last time the page represented by this item was visited. The interval    is since the reference date as determined by NSDate.  This value is normally set by    the WebKit.
--
-- ObjC selector: @- lastVisitedTimeInterval@
lastVisitedTimeInterval :: IsWebHistoryItem webHistoryItem => webHistoryItem -> IO CDouble
lastVisitedTimeInterval webHistoryItem =
  sendMessage webHistoryItem lastVisitedTimeIntervalSelector

-- | @- alternateTitle@
alternateTitle :: IsWebHistoryItem webHistoryItem => webHistoryItem -> IO (Id NSString)
alternateTitle webHistoryItem =
  sendMessage webHistoryItem alternateTitleSelector

-- | @- setAlternateTitle:@
setAlternateTitle :: (IsWebHistoryItem webHistoryItem, IsNSString value) => webHistoryItem -> value -> IO ()
setAlternateTitle webHistoryItem value =
  sendMessage webHistoryItem setAlternateTitleSelector (toNSString value)

-- | icon
--
-- The favorite icon of the page represented by this item.
--
-- This icon returned will be determined by the WebKit.
--
-- ObjC selector: @- icon@
icon :: IsWebHistoryItem webHistoryItem => webHistoryItem -> IO (Id NSImage)
icon webHistoryItem =
  sendMessage webHistoryItem iconSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURLString:title:lastVisitedTimeInterval:@
initWithURLString_title_lastVisitedTimeIntervalSelector :: Selector '[Id NSString, Id NSString, CDouble] (Id WebHistoryItem)
initWithURLString_title_lastVisitedTimeIntervalSelector = mkSelector "initWithURLString:title:lastVisitedTimeInterval:"

-- | @Selector@ for @originalURLString@
originalURLStringSelector :: Selector '[] (Id NSString)
originalURLStringSelector = mkSelector "originalURLString"

-- | @Selector@ for @URLString@
urlStringSelector :: Selector '[] (Id NSString)
urlStringSelector = mkSelector "URLString"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @lastVisitedTimeInterval@
lastVisitedTimeIntervalSelector :: Selector '[] CDouble
lastVisitedTimeIntervalSelector = mkSelector "lastVisitedTimeInterval"

-- | @Selector@ for @alternateTitle@
alternateTitleSelector :: Selector '[] (Id NSString)
alternateTitleSelector = mkSelector "alternateTitle"

-- | @Selector@ for @setAlternateTitle:@
setAlternateTitleSelector :: Selector '[Id NSString] ()
setAlternateTitleSelector = mkSelector "setAlternateTitle:"

-- | @Selector@ for @icon@
iconSelector :: Selector '[] (Id NSImage)
iconSelector = mkSelector "icon"

