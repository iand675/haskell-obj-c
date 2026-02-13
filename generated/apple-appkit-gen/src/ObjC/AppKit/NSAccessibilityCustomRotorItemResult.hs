{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSAccessibilityCustomRotorItemResults are the objects returned to assistive technologies that match a search parameter criteria.
--
-- Generated bindings for @NSAccessibilityCustomRotorItemResult@.
module ObjC.AppKit.NSAccessibilityCustomRotorItemResult
  ( NSAccessibilityCustomRotorItemResult
  , IsNSAccessibilityCustomRotorItemResult(..)
  , new
  , init_
  , initWithTargetElement
  , targetElement
  , targetRange
  , setTargetRange
  , customLabel
  , setCustomLabel
  , customLabelSelector
  , initSelector
  , initWithTargetElementSelector
  , newSelector
  , setCustomLabelSelector
  , setTargetRangeSelector
  , targetElementSelector
  , targetRangeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id NSAccessibilityCustomRotorItemResult)
new  =
  do
    cls' <- getRequiredClass "NSAccessibilityCustomRotorItemResult"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult => nsAccessibilityCustomRotorItemResult -> IO (Id NSAccessibilityCustomRotorItemResult)
init_ nsAccessibilityCustomRotorItemResult =
  sendOwnedMessage nsAccessibilityCustomRotorItemResult initSelector

-- | Creates an item result with a given target element. Assistive technologies may try to set accessibility focus on the element.
--
-- ObjC selector: @- initWithTargetElement:@
initWithTargetElement :: IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult => nsAccessibilityCustomRotorItemResult -> RawId -> IO (Id NSAccessibilityCustomRotorItemResult)
initWithTargetElement nsAccessibilityCustomRotorItemResult targetElement =
  sendOwnedMessage nsAccessibilityCustomRotorItemResult initWithTargetElementSelector targetElement

-- | A target element references an element that will be messaged for other accessibility properties. If it is not nil, assistive technologies may try to set accessibility focus on it.
--
-- ObjC selector: @- targetElement@
targetElement :: IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult => nsAccessibilityCustomRotorItemResult -> IO RawId
targetElement nsAccessibilityCustomRotorItemResult =
  sendMessage nsAccessibilityCustomRotorItemResult targetElementSelector

-- | For text-based elements such as an NSTextView, this is an NSRange that specifies the area of interest. If the target range has NSNotFound for the location, the search should begin from the first or last character of the text element, depending on the search direction.
--
-- ObjC selector: @- targetRange@
targetRange :: IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult => nsAccessibilityCustomRotorItemResult -> IO NSRange
targetRange nsAccessibilityCustomRotorItemResult =
  sendMessage nsAccessibilityCustomRotorItemResult targetRangeSelector

-- | For text-based elements such as an NSTextView, this is an NSRange that specifies the area of interest. If the target range has NSNotFound for the location, the search should begin from the first or last character of the text element, depending on the search direction.
--
-- ObjC selector: @- setTargetRange:@
setTargetRange :: IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult => nsAccessibilityCustomRotorItemResult -> NSRange -> IO ()
setTargetRange nsAccessibilityCustomRotorItemResult value =
  sendMessage nsAccessibilityCustomRotorItemResult setTargetRangeSelector value

-- | A localized label that can be used instead of the default item  label to describe the item result.
--
-- Required if using the loader-based initializer. Optional otherwise.
--
-- ObjC selector: @- customLabel@
customLabel :: IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult => nsAccessibilityCustomRotorItemResult -> IO (Id NSString)
customLabel nsAccessibilityCustomRotorItemResult =
  sendMessage nsAccessibilityCustomRotorItemResult customLabelSelector

-- | A localized label that can be used instead of the default item  label to describe the item result.
--
-- Required if using the loader-based initializer. Optional otherwise.
--
-- ObjC selector: @- setCustomLabel:@
setCustomLabel :: (IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult, IsNSString value) => nsAccessibilityCustomRotorItemResult -> value -> IO ()
setCustomLabel nsAccessibilityCustomRotorItemResult value =
  sendMessage nsAccessibilityCustomRotorItemResult setCustomLabelSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSAccessibilityCustomRotorItemResult)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSAccessibilityCustomRotorItemResult)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTargetElement:@
initWithTargetElementSelector :: Selector '[RawId] (Id NSAccessibilityCustomRotorItemResult)
initWithTargetElementSelector = mkSelector "initWithTargetElement:"

-- | @Selector@ for @targetElement@
targetElementSelector :: Selector '[] RawId
targetElementSelector = mkSelector "targetElement"

-- | @Selector@ for @targetRange@
targetRangeSelector :: Selector '[] NSRange
targetRangeSelector = mkSelector "targetRange"

-- | @Selector@ for @setTargetRange:@
setTargetRangeSelector :: Selector '[NSRange] ()
setTargetRangeSelector = mkSelector "setTargetRange:"

-- | @Selector@ for @customLabel@
customLabelSelector :: Selector '[] (Id NSString)
customLabelSelector = mkSelector "customLabel"

-- | @Selector@ for @setCustomLabel:@
setCustomLabelSelector :: Selector '[Id NSString] ()
setCustomLabelSelector = mkSelector "setCustomLabel:"

