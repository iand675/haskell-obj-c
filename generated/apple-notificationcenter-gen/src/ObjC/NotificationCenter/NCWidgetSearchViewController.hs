{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NCWidgetSearchViewController@.
module ObjC.NotificationCenter.NCWidgetSearchViewController
  ( NCWidgetSearchViewController
  , IsNCWidgetSearchViewController(..)
  , delegate
  , setDelegate
  , searchResults
  , setSearchResults
  , searchDescription
  , setSearchDescription
  , searchResultsPlaceholderString
  , setSearchResultsPlaceholderString
  , searchResultKeyPath
  , setSearchResultKeyPath
  , delegateSelector
  , searchDescriptionSelector
  , searchResultKeyPathSelector
  , searchResultsPlaceholderStringSelector
  , searchResultsSelector
  , setDelegateSelector
  , setSearchDescriptionSelector
  , setSearchResultKeyPathSelector
  , setSearchResultsPlaceholderStringSelector
  , setSearchResultsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NotificationCenter.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- delegate@
delegate :: IsNCWidgetSearchViewController ncWidgetSearchViewController => ncWidgetSearchViewController -> IO RawId
delegate ncWidgetSearchViewController =
  sendMessage ncWidgetSearchViewController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNCWidgetSearchViewController ncWidgetSearchViewController => ncWidgetSearchViewController -> RawId -> IO ()
setDelegate ncWidgetSearchViewController value =
  sendMessage ncWidgetSearchViewController setDelegateSelector value

-- | @- searchResults@
searchResults :: IsNCWidgetSearchViewController ncWidgetSearchViewController => ncWidgetSearchViewController -> IO (Id NSArray)
searchResults ncWidgetSearchViewController =
  sendMessage ncWidgetSearchViewController searchResultsSelector

-- | @- setSearchResults:@
setSearchResults :: (IsNCWidgetSearchViewController ncWidgetSearchViewController, IsNSArray value) => ncWidgetSearchViewController -> value -> IO ()
setSearchResults ncWidgetSearchViewController value =
  sendMessage ncWidgetSearchViewController setSearchResultsSelector (toNSArray value)

-- | @- searchDescription@
searchDescription :: IsNCWidgetSearchViewController ncWidgetSearchViewController => ncWidgetSearchViewController -> IO (Id NSString)
searchDescription ncWidgetSearchViewController =
  sendMessage ncWidgetSearchViewController searchDescriptionSelector

-- | @- setSearchDescription:@
setSearchDescription :: (IsNCWidgetSearchViewController ncWidgetSearchViewController, IsNSString value) => ncWidgetSearchViewController -> value -> IO ()
setSearchDescription ncWidgetSearchViewController value =
  sendMessage ncWidgetSearchViewController setSearchDescriptionSelector (toNSString value)

-- | @- searchResultsPlaceholderString@
searchResultsPlaceholderString :: IsNCWidgetSearchViewController ncWidgetSearchViewController => ncWidgetSearchViewController -> IO (Id NSString)
searchResultsPlaceholderString ncWidgetSearchViewController =
  sendMessage ncWidgetSearchViewController searchResultsPlaceholderStringSelector

-- | @- setSearchResultsPlaceholderString:@
setSearchResultsPlaceholderString :: (IsNCWidgetSearchViewController ncWidgetSearchViewController, IsNSString value) => ncWidgetSearchViewController -> value -> IO ()
setSearchResultsPlaceholderString ncWidgetSearchViewController value =
  sendMessage ncWidgetSearchViewController setSearchResultsPlaceholderStringSelector (toNSString value)

-- | @- searchResultKeyPath@
searchResultKeyPath :: IsNCWidgetSearchViewController ncWidgetSearchViewController => ncWidgetSearchViewController -> IO (Id NSString)
searchResultKeyPath ncWidgetSearchViewController =
  sendMessage ncWidgetSearchViewController searchResultKeyPathSelector

-- | @- setSearchResultKeyPath:@
setSearchResultKeyPath :: (IsNCWidgetSearchViewController ncWidgetSearchViewController, IsNSString value) => ncWidgetSearchViewController -> value -> IO ()
setSearchResultKeyPath ncWidgetSearchViewController value =
  sendMessage ncWidgetSearchViewController setSearchResultKeyPathSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @searchResults@
searchResultsSelector :: Selector '[] (Id NSArray)
searchResultsSelector = mkSelector "searchResults"

-- | @Selector@ for @setSearchResults:@
setSearchResultsSelector :: Selector '[Id NSArray] ()
setSearchResultsSelector = mkSelector "setSearchResults:"

-- | @Selector@ for @searchDescription@
searchDescriptionSelector :: Selector '[] (Id NSString)
searchDescriptionSelector = mkSelector "searchDescription"

-- | @Selector@ for @setSearchDescription:@
setSearchDescriptionSelector :: Selector '[Id NSString] ()
setSearchDescriptionSelector = mkSelector "setSearchDescription:"

-- | @Selector@ for @searchResultsPlaceholderString@
searchResultsPlaceholderStringSelector :: Selector '[] (Id NSString)
searchResultsPlaceholderStringSelector = mkSelector "searchResultsPlaceholderString"

-- | @Selector@ for @setSearchResultsPlaceholderString:@
setSearchResultsPlaceholderStringSelector :: Selector '[Id NSString] ()
setSearchResultsPlaceholderStringSelector = mkSelector "setSearchResultsPlaceholderString:"

-- | @Selector@ for @searchResultKeyPath@
searchResultKeyPathSelector :: Selector '[] (Id NSString)
searchResultKeyPathSelector = mkSelector "searchResultKeyPath"

-- | @Selector@ for @setSearchResultKeyPath:@
setSearchResultKeyPathSelector :: Selector '[Id NSString] ()
setSearchResultKeyPathSelector = mkSelector "setSearchResultKeyPath:"

