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
  , isRepresentedElement
  , initWithTextContentManagerSelector
  , textContentManagerSelector
  , setTextContentManagerSelector
  , elementRangeSelector
  , setElementRangeSelector
  , isRepresentedElementSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithTextContentManager:@
initWithTextContentManager :: (IsNSTextElement nsTextElement, IsNSTextContentManager textContentManager) => nsTextElement -> textContentManager -> IO (Id NSTextElement)
initWithTextContentManager nsTextElement  textContentManager =
withObjCPtr textContentManager $ \raw_textContentManager ->
    sendMsg nsTextElement (mkSelector "initWithTextContentManager:") (retPtr retVoid) [argPtr (castPtr raw_textContentManager :: Ptr ())] >>= ownedObject . castPtr

-- | @- textContentManager@
textContentManager :: IsNSTextElement nsTextElement => nsTextElement -> IO (Id NSTextContentManager)
textContentManager nsTextElement  =
  sendMsg nsTextElement (mkSelector "textContentManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextContentManager:@
setTextContentManager :: (IsNSTextElement nsTextElement, IsNSTextContentManager value) => nsTextElement -> value -> IO ()
setTextContentManager nsTextElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextElement (mkSelector "setTextContentManager:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- elementRange@
elementRange :: IsNSTextElement nsTextElement => nsTextElement -> IO (Id NSTextRange)
elementRange nsTextElement  =
  sendMsg nsTextElement (mkSelector "elementRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setElementRange:@
setElementRange :: (IsNSTextElement nsTextElement, IsNSTextRange value) => nsTextElement -> value -> IO ()
setElementRange nsTextElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTextElement (mkSelector "setElementRange:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- isRepresentedElement@
isRepresentedElement :: IsNSTextElement nsTextElement => nsTextElement -> IO Bool
isRepresentedElement nsTextElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTextElement (mkSelector "isRepresentedElement") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTextContentManager:@
initWithTextContentManagerSelector :: Selector
initWithTextContentManagerSelector = mkSelector "initWithTextContentManager:"

-- | @Selector@ for @textContentManager@
textContentManagerSelector :: Selector
textContentManagerSelector = mkSelector "textContentManager"

-- | @Selector@ for @setTextContentManager:@
setTextContentManagerSelector :: Selector
setTextContentManagerSelector = mkSelector "setTextContentManager:"

-- | @Selector@ for @elementRange@
elementRangeSelector :: Selector
elementRangeSelector = mkSelector "elementRange"

-- | @Selector@ for @setElementRange:@
setElementRangeSelector :: Selector
setElementRangeSelector = mkSelector "setElementRange:"

-- | @Selector@ for @isRepresentedElement@
isRepresentedElementSelector :: Selector
isRepresentedElementSelector = mkSelector "isRepresentedElement"

