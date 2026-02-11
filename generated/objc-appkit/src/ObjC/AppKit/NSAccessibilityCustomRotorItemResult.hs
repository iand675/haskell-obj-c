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
  , targetRange
  , setTargetRange
  , customLabel
  , setCustomLabel
  , newSelector
  , initSelector
  , initWithTargetElementSelector
  , targetRangeSelector
  , setTargetRangeSelector
  , customLabelSelector
  , setCustomLabelSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult => nsAccessibilityCustomRotorItemResult -> IO (Id NSAccessibilityCustomRotorItemResult)
init_ nsAccessibilityCustomRotorItemResult  =
  sendMsg nsAccessibilityCustomRotorItemResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Creates an item result with a given target element. Assistive technologies may try to set accessibility focus on the element.
--
-- ObjC selector: @- initWithTargetElement:@
initWithTargetElement :: IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult => nsAccessibilityCustomRotorItemResult -> RawId -> IO (Id NSAccessibilityCustomRotorItemResult)
initWithTargetElement nsAccessibilityCustomRotorItemResult  targetElement =
  sendMsg nsAccessibilityCustomRotorItemResult (mkSelector "initWithTargetElement:") (retPtr retVoid) [argPtr (castPtr (unRawId targetElement) :: Ptr ())] >>= ownedObject . castPtr

-- | For text-based elements such as an NSTextView, this is an NSRange that specifies the area of interest. If the target range has NSNotFound for the location, the search should begin from the first or last character of the text element, depending on the search direction.
--
-- ObjC selector: @- targetRange@
targetRange :: IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult => nsAccessibilityCustomRotorItemResult -> IO NSRange
targetRange nsAccessibilityCustomRotorItemResult  =
  sendMsgStret nsAccessibilityCustomRotorItemResult (mkSelector "targetRange") retNSRange []

-- | For text-based elements such as an NSTextView, this is an NSRange that specifies the area of interest. If the target range has NSNotFound for the location, the search should begin from the first or last character of the text element, depending on the search direction.
--
-- ObjC selector: @- setTargetRange:@
setTargetRange :: IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult => nsAccessibilityCustomRotorItemResult -> NSRange -> IO ()
setTargetRange nsAccessibilityCustomRotorItemResult  value =
  sendMsg nsAccessibilityCustomRotorItemResult (mkSelector "setTargetRange:") retVoid [argNSRange value]

-- | A localized label that can be used instead of the default item  label to describe the item result.
--
-- Required if using the loader-based initializer. Optional otherwise.
--
-- ObjC selector: @- customLabel@
customLabel :: IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult => nsAccessibilityCustomRotorItemResult -> IO (Id NSString)
customLabel nsAccessibilityCustomRotorItemResult  =
  sendMsg nsAccessibilityCustomRotorItemResult (mkSelector "customLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A localized label that can be used instead of the default item  label to describe the item result.
--
-- Required if using the loader-based initializer. Optional otherwise.
--
-- ObjC selector: @- setCustomLabel:@
setCustomLabel :: (IsNSAccessibilityCustomRotorItemResult nsAccessibilityCustomRotorItemResult, IsNSString value) => nsAccessibilityCustomRotorItemResult -> value -> IO ()
setCustomLabel nsAccessibilityCustomRotorItemResult  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsAccessibilityCustomRotorItemResult (mkSelector "setCustomLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTargetElement:@
initWithTargetElementSelector :: Selector
initWithTargetElementSelector = mkSelector "initWithTargetElement:"

-- | @Selector@ for @targetRange@
targetRangeSelector :: Selector
targetRangeSelector = mkSelector "targetRange"

-- | @Selector@ for @setTargetRange:@
setTargetRangeSelector :: Selector
setTargetRangeSelector = mkSelector "setTargetRange:"

-- | @Selector@ for @customLabel@
customLabelSelector :: Selector
customLabelSelector = mkSelector "customLabel"

-- | @Selector@ for @setCustomLabel:@
setCustomLabelSelector :: Selector
setCustomLabelSelector = mkSelector "setCustomLabel:"

