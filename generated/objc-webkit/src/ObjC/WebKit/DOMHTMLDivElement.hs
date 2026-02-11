{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLDivElement@.
module ObjC.WebKit.DOMHTMLDivElement
  ( DOMHTMLDivElement
  , IsDOMHTMLDivElement(..)
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
align :: IsDOMHTMLDivElement domhtmlDivElement => domhtmlDivElement -> IO (Id NSString)
align domhtmlDivElement  =
  sendMsg domhtmlDivElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLDivElement domhtmlDivElement, IsNSString value) => domhtmlDivElement -> value -> IO ()
setAlign domhtmlDivElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlDivElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

