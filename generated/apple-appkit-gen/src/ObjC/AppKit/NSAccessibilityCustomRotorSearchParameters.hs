{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , filterStringSelector
  , searchDirectionSelector
  , setCurrentItemSelector
  , setFilterStringSelector
  , setSearchDirectionSelector

  -- * Enum types
  , NSAccessibilityCustomRotorSearchDirection(NSAccessibilityCustomRotorSearchDirection)
  , pattern NSAccessibilityCustomRotorSearchDirectionPrevious
  , pattern NSAccessibilityCustomRotorSearchDirectionNext

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

-- | The currentItem determines where the search will start from. If  it is nil, the search should begin from, and include, the first or last item, depending on which search direction is used (e.g. search direction next will return the first item and previous will return the last item).
--
-- ObjC selector: @- currentItem@
currentItem :: IsNSAccessibilityCustomRotorSearchParameters nsAccessibilityCustomRotorSearchParameters => nsAccessibilityCustomRotorSearchParameters -> IO (Id NSAccessibilityCustomRotorItemResult)
currentItem nsAccessibilityCustomRotorSearchParameters =
  sendMessage nsAccessibilityCustomRotorSearchParameters currentItemSelector

-- | The currentItem determines where the search will start from. If  it is nil, the search should begin from, and include, the first or last item, depending on which search direction is used (e.g. search direction next will return the first item and previous will return the last item).
--
-- ObjC selector: @- setCurrentItem:@
setCurrentItem :: (IsNSAccessibilityCustomRotorSearchParameters nsAccessibilityCustomRotorSearchParameters, IsNSAccessibilityCustomRotorItemResult value) => nsAccessibilityCustomRotorSearchParameters -> value -> IO ()
setCurrentItem nsAccessibilityCustomRotorSearchParameters value =
  sendMessage nsAccessibilityCustomRotorSearchParameters setCurrentItemSelector (toNSAccessibilityCustomRotorItemResult value)

-- | Either NSAccessibilityCustomRotorSearchDirectionPrevious or NSAccessibilityCustomRotorSearchDirectionNext.
--
-- ObjC selector: @- searchDirection@
searchDirection :: IsNSAccessibilityCustomRotorSearchParameters nsAccessibilityCustomRotorSearchParameters => nsAccessibilityCustomRotorSearchParameters -> IO NSAccessibilityCustomRotorSearchDirection
searchDirection nsAccessibilityCustomRotorSearchParameters =
  sendMessage nsAccessibilityCustomRotorSearchParameters searchDirectionSelector

-- | Either NSAccessibilityCustomRotorSearchDirectionPrevious or NSAccessibilityCustomRotorSearchDirectionNext.
--
-- ObjC selector: @- setSearchDirection:@
setSearchDirection :: IsNSAccessibilityCustomRotorSearchParameters nsAccessibilityCustomRotorSearchParameters => nsAccessibilityCustomRotorSearchParameters -> NSAccessibilityCustomRotorSearchDirection -> IO ()
setSearchDirection nsAccessibilityCustomRotorSearchParameters value =
  sendMessage nsAccessibilityCustomRotorSearchParameters setSearchDirectionSelector value

-- | A string of text to filter the results against. This is used to get type-ahead results. For example, given a list of primary colors and filter text "Re", color item "Red" would be returned as a result.
--
-- ObjC selector: @- filterString@
filterString :: IsNSAccessibilityCustomRotorSearchParameters nsAccessibilityCustomRotorSearchParameters => nsAccessibilityCustomRotorSearchParameters -> IO (Id NSString)
filterString nsAccessibilityCustomRotorSearchParameters =
  sendMessage nsAccessibilityCustomRotorSearchParameters filterStringSelector

-- | A string of text to filter the results against. This is used to get type-ahead results. For example, given a list of primary colors and filter text "Re", color item "Red" would be returned as a result.
--
-- ObjC selector: @- setFilterString:@
setFilterString :: (IsNSAccessibilityCustomRotorSearchParameters nsAccessibilityCustomRotorSearchParameters, IsNSString value) => nsAccessibilityCustomRotorSearchParameters -> value -> IO ()
setFilterString nsAccessibilityCustomRotorSearchParameters value =
  sendMessage nsAccessibilityCustomRotorSearchParameters setFilterStringSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentItem@
currentItemSelector :: Selector '[] (Id NSAccessibilityCustomRotorItemResult)
currentItemSelector = mkSelector "currentItem"

-- | @Selector@ for @setCurrentItem:@
setCurrentItemSelector :: Selector '[Id NSAccessibilityCustomRotorItemResult] ()
setCurrentItemSelector = mkSelector "setCurrentItem:"

-- | @Selector@ for @searchDirection@
searchDirectionSelector :: Selector '[] NSAccessibilityCustomRotorSearchDirection
searchDirectionSelector = mkSelector "searchDirection"

-- | @Selector@ for @setSearchDirection:@
setSearchDirectionSelector :: Selector '[NSAccessibilityCustomRotorSearchDirection] ()
setSearchDirectionSelector = mkSelector "setSearchDirection:"

-- | @Selector@ for @filterString@
filterStringSelector :: Selector '[] (Id NSString)
filterStringSelector = mkSelector "filterString"

-- | @Selector@ for @setFilterString:@
setFilterStringSelector :: Selector '[Id NSString] ()
setFilterStringSelector = mkSelector "setFilterString:"

