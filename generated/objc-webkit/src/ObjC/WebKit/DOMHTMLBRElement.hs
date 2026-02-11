{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLBRElement@.
module ObjC.WebKit.DOMHTMLBRElement
  ( DOMHTMLBRElement
  , IsDOMHTMLBRElement(..)
  , clear
  , setClear
  , clearSelector
  , setClearSelector


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

-- | @- clear@
clear :: IsDOMHTMLBRElement domhtmlbrElement => domhtmlbrElement -> IO (Id NSString)
clear domhtmlbrElement  =
  sendMsg domhtmlbrElement (mkSelector "clear") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClear:@
setClear :: (IsDOMHTMLBRElement domhtmlbrElement, IsNSString value) => domhtmlbrElement -> value -> IO ()
setClear domhtmlbrElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlbrElement (mkSelector "setClear:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clear@
clearSelector :: Selector
clearSelector = mkSelector "clear"

-- | @Selector@ for @setClear:@
setClearSelector :: Selector
setClearSelector = mkSelector "setClear:"

