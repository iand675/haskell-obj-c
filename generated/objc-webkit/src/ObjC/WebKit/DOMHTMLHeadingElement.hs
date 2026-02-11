{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLHeadingElement@.
module ObjC.WebKit.DOMHTMLHeadingElement
  ( DOMHTMLHeadingElement
  , IsDOMHTMLHeadingElement(..)
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
align :: IsDOMHTMLHeadingElement domhtmlHeadingElement => domhtmlHeadingElement -> IO (Id NSString)
align domhtmlHeadingElement  =
  sendMsg domhtmlHeadingElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLHeadingElement domhtmlHeadingElement, IsNSString value) => domhtmlHeadingElement -> value -> IO ()
setAlign domhtmlHeadingElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlHeadingElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

