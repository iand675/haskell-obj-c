{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLModElement@.
module ObjC.WebKit.DOMHTMLModElement
  ( DOMHTMLModElement
  , IsDOMHTMLModElement(..)
  , cite
  , setCite
  , dateTime
  , setDateTime
  , citeSelector
  , setCiteSelector
  , dateTimeSelector
  , setDateTimeSelector


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

-- | @- cite@
cite :: IsDOMHTMLModElement domhtmlModElement => domhtmlModElement -> IO (Id NSString)
cite domhtmlModElement  =
  sendMsg domhtmlModElement (mkSelector "cite") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCite:@
setCite :: (IsDOMHTMLModElement domhtmlModElement, IsNSString value) => domhtmlModElement -> value -> IO ()
setCite domhtmlModElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlModElement (mkSelector "setCite:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dateTime@
dateTime :: IsDOMHTMLModElement domhtmlModElement => domhtmlModElement -> IO (Id NSString)
dateTime domhtmlModElement  =
  sendMsg domhtmlModElement (mkSelector "dateTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDateTime:@
setDateTime :: (IsDOMHTMLModElement domhtmlModElement, IsNSString value) => domhtmlModElement -> value -> IO ()
setDateTime domhtmlModElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlModElement (mkSelector "setDateTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cite@
citeSelector :: Selector
citeSelector = mkSelector "cite"

-- | @Selector@ for @setCite:@
setCiteSelector :: Selector
setCiteSelector = mkSelector "setCite:"

-- | @Selector@ for @dateTime@
dateTimeSelector :: Selector
dateTimeSelector = mkSelector "dateTime"

-- | @Selector@ for @setDateTime:@
setDateTimeSelector :: Selector
setDateTimeSelector = mkSelector "setDateTime:"

