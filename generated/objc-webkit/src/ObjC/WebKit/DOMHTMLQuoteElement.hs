{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLQuoteElement@.
module ObjC.WebKit.DOMHTMLQuoteElement
  ( DOMHTMLQuoteElement
  , IsDOMHTMLQuoteElement(..)
  , cite
  , setCite
  , citeSelector
  , setCiteSelector


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
cite :: IsDOMHTMLQuoteElement domhtmlQuoteElement => domhtmlQuoteElement -> IO (Id NSString)
cite domhtmlQuoteElement  =
  sendMsg domhtmlQuoteElement (mkSelector "cite") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCite:@
setCite :: (IsDOMHTMLQuoteElement domhtmlQuoteElement, IsNSString value) => domhtmlQuoteElement -> value -> IO ()
setCite domhtmlQuoteElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlQuoteElement (mkSelector "setCite:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cite@
citeSelector :: Selector
citeSelector = mkSelector "cite"

-- | @Selector@ for @setCite:@
setCiteSelector :: Selector
setCiteSelector = mkSelector "setCite:"

