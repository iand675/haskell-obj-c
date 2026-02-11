{-# LANGUAGE PatternSynonyms #-}
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
  , initWithMarkerFormat_options_startingItemNumberSelector
  , initWithMarkerFormat_optionsSelector
  , initWithCoderSelector
  , markerForItemNumberSelector
  , markerFormatSelector
  , listOptionsSelector
  , startingItemNumberSelector
  , setStartingItemNumberSelector
  , orderedSelector
  , includesTextListMarkersSelector

  -- * Enum types
  , NSTextListOptions(NSTextListOptions)
  , pattern NSTextListPrependEnclosingMarker

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithMarkerFormat:options:startingItemNumber:@
initWithMarkerFormat_options_startingItemNumber :: (IsNSTextList nsTextList, IsNSString markerFormat) => nsTextList -> markerFormat -> NSTextListOptions -> CLong -> IO (Id NSTextList)
initWithMarkerFormat_options_startingItemNumber nsTextList  markerFormat options startingItemNumber =
withObjCPtr markerFormat $ \raw_markerFormat ->
    sendMsg nsTextList (mkSelector "initWithMarkerFormat:options:startingItemNumber:") (retPtr retVoid) [argPtr (castPtr raw_markerFormat :: Ptr ()), argCULong (coerce options), argCLong (fromIntegral startingItemNumber)] >>= ownedObject . castPtr

-- | @- initWithMarkerFormat:options:@
initWithMarkerFormat_options :: (IsNSTextList nsTextList, IsNSString markerFormat) => nsTextList -> markerFormat -> CULong -> IO (Id NSTextList)
initWithMarkerFormat_options nsTextList  markerFormat options =
withObjCPtr markerFormat $ \raw_markerFormat ->
    sendMsg nsTextList (mkSelector "initWithMarkerFormat:options:") (retPtr retVoid) [argPtr (castPtr raw_markerFormat :: Ptr ()), argCULong (fromIntegral options)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextList nsTextList, IsNSCoder coder) => nsTextList -> coder -> IO (Id NSTextList)
initWithCoder nsTextList  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsTextList (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- markerForItemNumber:@
markerForItemNumber :: IsNSTextList nsTextList => nsTextList -> CLong -> IO (Id NSString)
markerForItemNumber nsTextList  itemNumber =
  sendMsg nsTextList (mkSelector "markerForItemNumber:") (retPtr retVoid) [argCLong (fromIntegral itemNumber)] >>= retainedObject . castPtr

-- | @- markerFormat@
markerFormat :: IsNSTextList nsTextList => nsTextList -> IO (Id NSString)
markerFormat nsTextList  =
  sendMsg nsTextList (mkSelector "markerFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- listOptions@
listOptions :: IsNSTextList nsTextList => nsTextList -> IO NSTextListOptions
listOptions nsTextList  =
  fmap (coerce :: CULong -> NSTextListOptions) $ sendMsg nsTextList (mkSelector "listOptions") retCULong []

-- | @- startingItemNumber@
startingItemNumber :: IsNSTextList nsTextList => nsTextList -> IO CLong
startingItemNumber nsTextList  =
  sendMsg nsTextList (mkSelector "startingItemNumber") retCLong []

-- | @- setStartingItemNumber:@
setStartingItemNumber :: IsNSTextList nsTextList => nsTextList -> CLong -> IO ()
setStartingItemNumber nsTextList  value =
  sendMsg nsTextList (mkSelector "setStartingItemNumber:") retVoid [argCLong (fromIntegral value)]

-- | @- ordered@
ordered :: IsNSTextList nsTextList => nsTextList -> IO Bool
ordered nsTextList  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextList (mkSelector "ordered") retCULong []

-- | @+ includesTextListMarkers@
includesTextListMarkers :: IO Bool
includesTextListMarkers  =
  do
    cls' <- getRequiredClass "NSTextList"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "includesTextListMarkers") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMarkerFormat:options:startingItemNumber:@
initWithMarkerFormat_options_startingItemNumberSelector :: Selector
initWithMarkerFormat_options_startingItemNumberSelector = mkSelector "initWithMarkerFormat:options:startingItemNumber:"

-- | @Selector@ for @initWithMarkerFormat:options:@
initWithMarkerFormat_optionsSelector :: Selector
initWithMarkerFormat_optionsSelector = mkSelector "initWithMarkerFormat:options:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @markerForItemNumber:@
markerForItemNumberSelector :: Selector
markerForItemNumberSelector = mkSelector "markerForItemNumber:"

-- | @Selector@ for @markerFormat@
markerFormatSelector :: Selector
markerFormatSelector = mkSelector "markerFormat"

-- | @Selector@ for @listOptions@
listOptionsSelector :: Selector
listOptionsSelector = mkSelector "listOptions"

-- | @Selector@ for @startingItemNumber@
startingItemNumberSelector :: Selector
startingItemNumberSelector = mkSelector "startingItemNumber"

-- | @Selector@ for @setStartingItemNumber:@
setStartingItemNumberSelector :: Selector
setStartingItemNumberSelector = mkSelector "setStartingItemNumber:"

-- | @Selector@ for @ordered@
orderedSelector :: Selector
orderedSelector = mkSelector "ordered"

-- | @Selector@ for @includesTextListMarkers@
includesTextListMarkersSelector :: Selector
includesTextListMarkersSelector = mkSelector "includesTextListMarkers"

