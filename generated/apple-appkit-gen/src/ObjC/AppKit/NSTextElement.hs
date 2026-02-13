{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextElement@.
module ObjC.AppKit.NSTextElement
  ( NSTextElement
  , IsNSTextElement(..)
  , initWithTextContentManager
  , textContentManager
  , setTextContentManager
  , elementRange
  , setElementRange
  , childElements
  , parentElement
  , isRepresentedElement
  , childElementsSelector
  , elementRangeSelector
  , initWithTextContentManagerSelector
  , isRepresentedElementSelector
  , parentElementSelector
  , setElementRangeSelector
  , setTextContentManagerSelector
  , textContentManagerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTextContentManager:@
initWithTextContentManager :: (IsNSTextElement nsTextElement, IsNSTextContentManager textContentManager) => nsTextElement -> textContentManager -> IO (Id NSTextElement)
initWithTextContentManager nsTextElement textContentManager =
  sendOwnedMessage nsTextElement initWithTextContentManagerSelector (toNSTextContentManager textContentManager)

-- | @- textContentManager@
textContentManager :: IsNSTextElement nsTextElement => nsTextElement -> IO (Id NSTextContentManager)
textContentManager nsTextElement =
  sendMessage nsTextElement textContentManagerSelector

-- | @- setTextContentManager:@
setTextContentManager :: (IsNSTextElement nsTextElement, IsNSTextContentManager value) => nsTextElement -> value -> IO ()
setTextContentManager nsTextElement value =
  sendMessage nsTextElement setTextContentManagerSelector (toNSTextContentManager value)

-- | @- elementRange@
elementRange :: IsNSTextElement nsTextElement => nsTextElement -> IO (Id NSTextRange)
elementRange nsTextElement =
  sendMessage nsTextElement elementRangeSelector

-- | @- setElementRange:@
setElementRange :: (IsNSTextElement nsTextElement, IsNSTextRange value) => nsTextElement -> value -> IO ()
setElementRange nsTextElement value =
  sendMessage nsTextElement setElementRangeSelector (toNSTextRange value)

-- | @- childElements@
childElements :: IsNSTextElement nsTextElement => nsTextElement -> IO (Id NSArray)
childElements nsTextElement =
  sendMessage nsTextElement childElementsSelector

-- | @- parentElement@
parentElement :: IsNSTextElement nsTextElement => nsTextElement -> IO (Id NSTextElement)
parentElement nsTextElement =
  sendMessage nsTextElement parentElementSelector

-- | @- isRepresentedElement@
isRepresentedElement :: IsNSTextElement nsTextElement => nsTextElement -> IO Bool
isRepresentedElement nsTextElement =
  sendMessage nsTextElement isRepresentedElementSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTextContentManager:@
initWithTextContentManagerSelector :: Selector '[Id NSTextContentManager] (Id NSTextElement)
initWithTextContentManagerSelector = mkSelector "initWithTextContentManager:"

-- | @Selector@ for @textContentManager@
textContentManagerSelector :: Selector '[] (Id NSTextContentManager)
textContentManagerSelector = mkSelector "textContentManager"

-- | @Selector@ for @setTextContentManager:@
setTextContentManagerSelector :: Selector '[Id NSTextContentManager] ()
setTextContentManagerSelector = mkSelector "setTextContentManager:"

-- | @Selector@ for @elementRange@
elementRangeSelector :: Selector '[] (Id NSTextRange)
elementRangeSelector = mkSelector "elementRange"

-- | @Selector@ for @setElementRange:@
setElementRangeSelector :: Selector '[Id NSTextRange] ()
setElementRangeSelector = mkSelector "setElementRange:"

-- | @Selector@ for @childElements@
childElementsSelector :: Selector '[] (Id NSArray)
childElementsSelector = mkSelector "childElements"

-- | @Selector@ for @parentElement@
parentElementSelector :: Selector '[] (Id NSTextElement)
parentElementSelector = mkSelector "parentElement"

-- | @Selector@ for @isRepresentedElement@
isRepresentedElementSelector :: Selector '[] Bool
isRepresentedElementSelector = mkSelector "isRepresentedElement"

