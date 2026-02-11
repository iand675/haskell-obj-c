{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NCWidgetSearchViewController@.
module ObjC.NotificationCenter.NCWidgetSearchViewController
  ( NCWidgetSearchViewController
  , IsNCWidgetSearchViewController(..)
  , searchResults
  , setSearchResults
  , searchDescription
  , setSearchDescription
  , searchResultsPlaceholderString
  , setSearchResultsPlaceholderString
  , searchResultKeyPath
  , setSearchResultKeyPath
  , searchResultsSelector
  , setSearchResultsSelector
  , searchDescriptionSelector
  , setSearchDescriptionSelector
  , searchResultsPlaceholderStringSelector
  , setSearchResultsPlaceholderStringSelector
  , searchResultKeyPathSelector
  , setSearchResultKeyPathSelector


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

import ObjC.NotificationCenter.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- searchResults@
searchResults :: IsNCWidgetSearchViewController ncWidgetSearchViewController => ncWidgetSearchViewController -> IO (Id NSArray)
searchResults ncWidgetSearchViewController  =
  sendMsg ncWidgetSearchViewController (mkSelector "searchResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSearchResults:@
setSearchResults :: (IsNCWidgetSearchViewController ncWidgetSearchViewController, IsNSArray value) => ncWidgetSearchViewController -> value -> IO ()
setSearchResults ncWidgetSearchViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg ncWidgetSearchViewController (mkSelector "setSearchResults:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- searchDescription@
searchDescription :: IsNCWidgetSearchViewController ncWidgetSearchViewController => ncWidgetSearchViewController -> IO (Id NSString)
searchDescription ncWidgetSearchViewController  =
  sendMsg ncWidgetSearchViewController (mkSelector "searchDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSearchDescription:@
setSearchDescription :: (IsNCWidgetSearchViewController ncWidgetSearchViewController, IsNSString value) => ncWidgetSearchViewController -> value -> IO ()
setSearchDescription ncWidgetSearchViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg ncWidgetSearchViewController (mkSelector "setSearchDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- searchResultsPlaceholderString@
searchResultsPlaceholderString :: IsNCWidgetSearchViewController ncWidgetSearchViewController => ncWidgetSearchViewController -> IO (Id NSString)
searchResultsPlaceholderString ncWidgetSearchViewController  =
  sendMsg ncWidgetSearchViewController (mkSelector "searchResultsPlaceholderString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSearchResultsPlaceholderString:@
setSearchResultsPlaceholderString :: (IsNCWidgetSearchViewController ncWidgetSearchViewController, IsNSString value) => ncWidgetSearchViewController -> value -> IO ()
setSearchResultsPlaceholderString ncWidgetSearchViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg ncWidgetSearchViewController (mkSelector "setSearchResultsPlaceholderString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- searchResultKeyPath@
searchResultKeyPath :: IsNCWidgetSearchViewController ncWidgetSearchViewController => ncWidgetSearchViewController -> IO (Id NSString)
searchResultKeyPath ncWidgetSearchViewController  =
  sendMsg ncWidgetSearchViewController (mkSelector "searchResultKeyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSearchResultKeyPath:@
setSearchResultKeyPath :: (IsNCWidgetSearchViewController ncWidgetSearchViewController, IsNSString value) => ncWidgetSearchViewController -> value -> IO ()
setSearchResultKeyPath ncWidgetSearchViewController  value =
withObjCPtr value $ \raw_value ->
    sendMsg ncWidgetSearchViewController (mkSelector "setSearchResultKeyPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @searchResults@
searchResultsSelector :: Selector
searchResultsSelector = mkSelector "searchResults"

-- | @Selector@ for @setSearchResults:@
setSearchResultsSelector :: Selector
setSearchResultsSelector = mkSelector "setSearchResults:"

-- | @Selector@ for @searchDescription@
searchDescriptionSelector :: Selector
searchDescriptionSelector = mkSelector "searchDescription"

-- | @Selector@ for @setSearchDescription:@
setSearchDescriptionSelector :: Selector
setSearchDescriptionSelector = mkSelector "setSearchDescription:"

-- | @Selector@ for @searchResultsPlaceholderString@
searchResultsPlaceholderStringSelector :: Selector
searchResultsPlaceholderStringSelector = mkSelector "searchResultsPlaceholderString"

-- | @Selector@ for @setSearchResultsPlaceholderString:@
setSearchResultsPlaceholderStringSelector :: Selector
setSearchResultsPlaceholderStringSelector = mkSelector "setSearchResultsPlaceholderString:"

-- | @Selector@ for @searchResultKeyPath@
searchResultKeyPathSelector :: Selector
searchResultKeyPathSelector = mkSelector "searchResultKeyPath"

-- | @Selector@ for @setSearchResultKeyPath:@
setSearchResultKeyPathSelector :: Selector
setSearchResultKeyPathSelector = mkSelector "setSearchResultKeyPath:"

