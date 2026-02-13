{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLLegendElement@.
module ObjC.WebKit.DOMHTMLLegendElement
  ( DOMHTMLLegendElement
  , IsDOMHTMLLegendElement(..)
  , form
  , align
  , setAlign
  , accessKey
  , setAccessKey
  , accessKeySelector
  , alignSelector
  , formSelector
  , setAccessKeySelector
  , setAlignSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- form@
form :: IsDOMHTMLLegendElement domhtmlLegendElement => domhtmlLegendElement -> IO (Id DOMHTMLFormElement)
form domhtmlLegendElement =
  sendMessage domhtmlLegendElement formSelector

-- | @- align@
align :: IsDOMHTMLLegendElement domhtmlLegendElement => domhtmlLegendElement -> IO (Id NSString)
align domhtmlLegendElement =
  sendMessage domhtmlLegendElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLLegendElement domhtmlLegendElement, IsNSString value) => domhtmlLegendElement -> value -> IO ()
setAlign domhtmlLegendElement value =
  sendMessage domhtmlLegendElement setAlignSelector (toNSString value)

-- | @- accessKey@
accessKey :: IsDOMHTMLLegendElement domhtmlLegendElement => domhtmlLegendElement -> IO (Id NSString)
accessKey domhtmlLegendElement =
  sendMessage domhtmlLegendElement accessKeySelector

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLLegendElement domhtmlLegendElement, IsNSString value) => domhtmlLegendElement -> value -> IO ()
setAccessKey domhtmlLegendElement value =
  sendMessage domhtmlLegendElement setAccessKeySelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @form@
formSelector :: Selector '[] (Id DOMHTMLFormElement)
formSelector = mkSelector "form"

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector '[] (Id NSString)
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector '[Id NSString] ()
setAccessKeySelector = mkSelector "setAccessKey:"

