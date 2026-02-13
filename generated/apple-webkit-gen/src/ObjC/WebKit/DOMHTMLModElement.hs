{-# LANGUAGE DataKinds #-}
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
  , dateTimeSelector
  , setCiteSelector
  , setDateTimeSelector


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
cite :: IsDOMHTMLModElement domhtmlModElement => domhtmlModElement -> IO (Id NSString)
cite domhtmlModElement =
  sendMessage domhtmlModElement citeSelector

-- | @- setCite:@
setCite :: (IsDOMHTMLModElement domhtmlModElement, IsNSString value) => domhtmlModElement -> value -> IO ()
setCite domhtmlModElement value =
  sendMessage domhtmlModElement setCiteSelector (toNSString value)

-- | @- dateTime@
dateTime :: IsDOMHTMLModElement domhtmlModElement => domhtmlModElement -> IO (Id NSString)
dateTime domhtmlModElement =
  sendMessage domhtmlModElement dateTimeSelector

-- | @- setDateTime:@
setDateTime :: (IsDOMHTMLModElement domhtmlModElement, IsNSString value) => domhtmlModElement -> value -> IO ()
setDateTime domhtmlModElement value =
  sendMessage domhtmlModElement setDateTimeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cite@
citeSelector :: Selector '[] (Id NSString)
citeSelector = mkSelector "cite"

-- | @Selector@ for @setCite:@
setCiteSelector :: Selector '[Id NSString] ()
setCiteSelector = mkSelector "setCite:"

-- | @Selector@ for @dateTime@
dateTimeSelector :: Selector '[] (Id NSString)
dateTimeSelector = mkSelector "dateTime"

-- | @Selector@ for @setDateTime:@
setDateTimeSelector :: Selector '[Id NSString] ()
setDateTimeSelector = mkSelector "setDateTime:"

