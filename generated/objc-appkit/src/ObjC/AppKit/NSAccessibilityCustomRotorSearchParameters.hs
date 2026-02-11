{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSAccessibilityCustomRotorSearchParameters is a container for  search parameters. It should be examined to determine the next matching NSAccessibilityCustomRotorItemResult.
--
-- Generated bindings for @NSAccessibilityCustomRotorSearchParameters@.
module ObjC.AppKit.NSAccessibilityCustomRotorSearchParameters
  ( NSAccessibilityCustomRotorSearchParameters
  , IsNSAccessibilityCustomRotorSearchParameters(..)
  , currentItem
  , setCurrentItem
  , searchDirection
  , setSearchDirection
  , filterString
  , setFilterString
  , currentItemSelector
  , setCurrentItemSelector
  , searchDirectionSelector
  , setSearchDirectionSelector
  , filterStringSelector
  , setFilterStringSelector

  -- * Enum types
  , NSAccessibilityCustomRotorSearchDirection(NSAccessibilityCustomRotorSearchDirection)
  , pattern NSAccessibilityCustomRotorSearchDirectionPrevious
  , pattern NSAccessibilityCustomRotorSearchDirectionNext

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

-- | The currentItem determines where the search will start from. If  it is nil, the search should begin from, and include, the first or last item, depending on which search direction is used (e.g. search direction next will return the first item and previous will return the last item).
--
-- ObjC selector: @- currentItem@
currentItem :: IsNSAccessibilityCustomRotorSearchParameters nsAccessibilityCustomRotorSearchParameters => nsAccessibilityCustomRotorSearchParameters -> IO (Id NSAccessibilityCustomRotorItemResult)
currentItem nsAccessibilityCustomRotorSearchParameters  =
  sendMsg nsAccessibilityCustomRotorSearchParameters (mkSelector "currentItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The currentItem determines where the search will start from. If  it is nil, the search should begin from, and include, the first or last item, depending on which search direction is used (e.g. search direction next will return the first item and previous will return the last item).
--
-- ObjC selector: @- setCurrentItem:@
setCurrentItem :: (IsNSAccessibilityCustomRotorSearchParameters nsAccessibilityCustomRotorSearchParameters, IsNSAccessibilityCustomRotorItemResult value) => nsAccessibilityCustomRotorSearchParameters -> value -> IO ()
setCurrentItem nsAccessibilityCustomRotorSearchParameters  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsAccessibilityCustomRotorSearchParameters (mkSelector "setCurrentItem:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Either NSAccessibilityCustomRotorSearchDirectionPrevious or NSAccessibilityCustomRotorSearchDirectionNext.
--
-- ObjC selector: @- searchDirection@
searchDirection :: IsNSAccessibilityCustomRotorSearchParameters nsAccessibilityCustomRotorSearchParameters => nsAccessibilityCustomRotorSearchParameters -> IO NSAccessibilityCustomRotorSearchDirection
searchDirection nsAccessibilityCustomRotorSearchParameters  =
  fmap (coerce :: CLong -> NSAccessibilityCustomRotorSearchDirection) $ sendMsg nsAccessibilityCustomRotorSearchParameters (mkSelector "searchDirection") retCLong []

-- | Either NSAccessibilityCustomRotorSearchDirectionPrevious or NSAccessibilityCustomRotorSearchDirectionNext.
--
-- ObjC selector: @- setSearchDirection:@
setSearchDirection :: IsNSAccessibilityCustomRotorSearchParameters nsAccessibilityCustomRotorSearchParameters => nsAccessibilityCustomRotorSearchParameters -> NSAccessibilityCustomRotorSearchDirection -> IO ()
setSearchDirection nsAccessibilityCustomRotorSearchParameters  value =
  sendMsg nsAccessibilityCustomRotorSearchParameters (mkSelector "setSearchDirection:") retVoid [argCLong (coerce value)]

-- | A string of text to filter the results against. This is used to get type-ahead results. For example, given a list of primary colors and filter text "Re", color item "Red" would be returned as a result.
--
-- ObjC selector: @- filterString@
filterString :: IsNSAccessibilityCustomRotorSearchParameters nsAccessibilityCustomRotorSearchParameters => nsAccessibilityCustomRotorSearchParameters -> IO (Id NSString)
filterString nsAccessibilityCustomRotorSearchParameters  =
  sendMsg nsAccessibilityCustomRotorSearchParameters (mkSelector "filterString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A string of text to filter the results against. This is used to get type-ahead results. For example, given a list of primary colors and filter text "Re", color item "Red" would be returned as a result.
--
-- ObjC selector: @- setFilterString:@
setFilterString :: (IsNSAccessibilityCustomRotorSearchParameters nsAccessibilityCustomRotorSearchParameters, IsNSString value) => nsAccessibilityCustomRotorSearchParameters -> value -> IO ()
setFilterString nsAccessibilityCustomRotorSearchParameters  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsAccessibilityCustomRotorSearchParameters (mkSelector "setFilterString:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentItem@
currentItemSelector :: Selector
currentItemSelector = mkSelector "currentItem"

-- | @Selector@ for @setCurrentItem:@
setCurrentItemSelector :: Selector
setCurrentItemSelector = mkSelector "setCurrentItem:"

-- | @Selector@ for @searchDirection@
searchDirectionSelector :: Selector
searchDirectionSelector = mkSelector "searchDirection"

-- | @Selector@ for @setSearchDirection:@
setSearchDirectionSelector :: Selector
setSearchDirectionSelector = mkSelector "setSearchDirection:"

-- | @Selector@ for @filterString@
filterStringSelector :: Selector
filterStringSelector = mkSelector "filterString"

-- | @Selector@ for @setFilterString:@
setFilterStringSelector :: Selector
setFilterStringSelector = mkSelector "setFilterString:"

