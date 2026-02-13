{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextList@.
module ObjC.AppKit.NSTextList
  ( NSTextList
  , IsNSTextList(..)
  , initWithMarkerFormat_options_startingItemNumber
  , initWithMarkerFormat_options
  , initWithCoder
  , markerForItemNumber
  , markerFormat
  , listOptions
  , startingItemNumber
  , setStartingItemNumber
  , ordered
  , includesTextListMarkers
  , includesTextListMarkersSelector
  , initWithCoderSelector
  , initWithMarkerFormat_optionsSelector
  , initWithMarkerFormat_options_startingItemNumberSelector
  , listOptionsSelector
  , markerForItemNumberSelector
  , markerFormatSelector
  , orderedSelector
  , setStartingItemNumberSelector
  , startingItemNumberSelector

  -- * Enum types
  , NSTextListOptions(NSTextListOptions)
  , pattern NSTextListPrependEnclosingMarker

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithMarkerFormat:options:startingItemNumber:@
initWithMarkerFormat_options_startingItemNumber :: (IsNSTextList nsTextList, IsNSString markerFormat) => nsTextList -> markerFormat -> NSTextListOptions -> CLong -> IO (Id NSTextList)
initWithMarkerFormat_options_startingItemNumber nsTextList markerFormat options startingItemNumber =
  sendOwnedMessage nsTextList initWithMarkerFormat_options_startingItemNumberSelector (toNSString markerFormat) options startingItemNumber

-- | @- initWithMarkerFormat:options:@
initWithMarkerFormat_options :: (IsNSTextList nsTextList, IsNSString markerFormat) => nsTextList -> markerFormat -> CULong -> IO (Id NSTextList)
initWithMarkerFormat_options nsTextList markerFormat options =
  sendOwnedMessage nsTextList initWithMarkerFormat_optionsSelector (toNSString markerFormat) options

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextList nsTextList, IsNSCoder coder) => nsTextList -> coder -> IO (Id NSTextList)
initWithCoder nsTextList coder =
  sendOwnedMessage nsTextList initWithCoderSelector (toNSCoder coder)

-- | @- markerForItemNumber:@
markerForItemNumber :: IsNSTextList nsTextList => nsTextList -> CLong -> IO (Id NSString)
markerForItemNumber nsTextList itemNumber =
  sendMessage nsTextList markerForItemNumberSelector itemNumber

-- | @- markerFormat@
markerFormat :: IsNSTextList nsTextList => nsTextList -> IO (Id NSString)
markerFormat nsTextList =
  sendMessage nsTextList markerFormatSelector

-- | @- listOptions@
listOptions :: IsNSTextList nsTextList => nsTextList -> IO NSTextListOptions
listOptions nsTextList =
  sendMessage nsTextList listOptionsSelector

-- | @- startingItemNumber@
startingItemNumber :: IsNSTextList nsTextList => nsTextList -> IO CLong
startingItemNumber nsTextList =
  sendMessage nsTextList startingItemNumberSelector

-- | @- setStartingItemNumber:@
setStartingItemNumber :: IsNSTextList nsTextList => nsTextList -> CLong -> IO ()
setStartingItemNumber nsTextList value =
  sendMessage nsTextList setStartingItemNumberSelector value

-- | @- ordered@
ordered :: IsNSTextList nsTextList => nsTextList -> IO Bool
ordered nsTextList =
  sendMessage nsTextList orderedSelector

-- | @+ includesTextListMarkers@
includesTextListMarkers :: IO Bool
includesTextListMarkers  =
  do
    cls' <- getRequiredClass "NSTextList"
    sendClassMessage cls' includesTextListMarkersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMarkerFormat:options:startingItemNumber:@
initWithMarkerFormat_options_startingItemNumberSelector :: Selector '[Id NSString, NSTextListOptions, CLong] (Id NSTextList)
initWithMarkerFormat_options_startingItemNumberSelector = mkSelector "initWithMarkerFormat:options:startingItemNumber:"

-- | @Selector@ for @initWithMarkerFormat:options:@
initWithMarkerFormat_optionsSelector :: Selector '[Id NSString, CULong] (Id NSTextList)
initWithMarkerFormat_optionsSelector = mkSelector "initWithMarkerFormat:options:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTextList)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @markerForItemNumber:@
markerForItemNumberSelector :: Selector '[CLong] (Id NSString)
markerForItemNumberSelector = mkSelector "markerForItemNumber:"

-- | @Selector@ for @markerFormat@
markerFormatSelector :: Selector '[] (Id NSString)
markerFormatSelector = mkSelector "markerFormat"

-- | @Selector@ for @listOptions@
listOptionsSelector :: Selector '[] NSTextListOptions
listOptionsSelector = mkSelector "listOptions"

-- | @Selector@ for @startingItemNumber@
startingItemNumberSelector :: Selector '[] CLong
startingItemNumberSelector = mkSelector "startingItemNumber"

-- | @Selector@ for @setStartingItemNumber:@
setStartingItemNumberSelector :: Selector '[CLong] ()
setStartingItemNumberSelector = mkSelector "setStartingItemNumber:"

-- | @Selector@ for @ordered@
orderedSelector :: Selector '[] Bool
orderedSelector = mkSelector "ordered"

-- | @Selector@ for @includesTextListMarkers@
includesTextListMarkersSelector :: Selector '[] Bool
includesTextListMarkersSelector = mkSelector "includesTextListMarkers"

