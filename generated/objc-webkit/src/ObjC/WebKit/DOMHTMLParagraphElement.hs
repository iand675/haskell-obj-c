{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLParagraphElement@.
module ObjC.WebKit.DOMHTMLParagraphElement
  ( DOMHTMLParagraphElement
  , IsDOMHTMLParagraphElement(..)
  , align
  , setAlign
  , alignSelector
  , setAlignSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- align@
align :: IsDOMHTMLParagraphElement domhtmlParagraphElement => domhtmlParagraphElement -> IO (Id NSString)
align domhtmlParagraphElement  =
  sendMsg domhtmlParagraphElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLParagraphElement domhtmlParagraphElement, IsNSString value) => domhtmlParagraphElement -> value -> IO ()
setAlign domhtmlParagraphElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlParagraphElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

