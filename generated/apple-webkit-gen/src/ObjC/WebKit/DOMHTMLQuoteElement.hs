{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cite@
cite :: IsDOMHTMLQuoteElement domhtmlQuoteElement => domhtmlQuoteElement -> IO (Id NSString)
cite domhtmlQuoteElement =
  sendMessage domhtmlQuoteElement citeSelector

-- | @- setCite:@
setCite :: (IsDOMHTMLQuoteElement domhtmlQuoteElement, IsNSString value) => domhtmlQuoteElement -> value -> IO ()
setCite domhtmlQuoteElement value =
  sendMessage domhtmlQuoteElement setCiteSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cite@
citeSelector :: Selector '[] (Id NSString)
citeSelector = mkSelector "cite"

-- | @Selector@ for @setCite:@
setCiteSelector :: Selector '[Id NSString] ()
setCiteSelector = mkSelector "setCite:"

