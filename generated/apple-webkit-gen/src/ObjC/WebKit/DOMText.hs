{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMText@.
module ObjC.WebKit.DOMText
  ( DOMText
  , IsDOMText(..)
  , splitText
  , replaceWholeText
  , wholeText
  , replaceWholeTextSelector
  , splitTextSelector
  , wholeTextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- splitText:@
splitText :: IsDOMText domText => domText -> CUInt -> IO (Id DOMText)
splitText domText offset =
  sendMessage domText splitTextSelector offset

-- | @- replaceWholeText:@
replaceWholeText :: (IsDOMText domText, IsNSString content) => domText -> content -> IO (Id DOMText)
replaceWholeText domText content =
  sendMessage domText replaceWholeTextSelector (toNSString content)

-- | @- wholeText@
wholeText :: IsDOMText domText => domText -> IO (Id NSString)
wholeText domText =
  sendMessage domText wholeTextSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @splitText:@
splitTextSelector :: Selector '[CUInt] (Id DOMText)
splitTextSelector = mkSelector "splitText:"

-- | @Selector@ for @replaceWholeText:@
replaceWholeTextSelector :: Selector '[Id NSString] (Id DOMText)
replaceWholeTextSelector = mkSelector "replaceWholeText:"

-- | @Selector@ for @wholeText@
wholeTextSelector :: Selector '[] (Id NSString)
wholeTextSelector = mkSelector "wholeText"

