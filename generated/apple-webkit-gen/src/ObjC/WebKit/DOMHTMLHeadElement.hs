{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLHeadElement@.
module ObjC.WebKit.DOMHTMLHeadElement
  ( DOMHTMLHeadElement
  , IsDOMHTMLHeadElement(..)
  , profile
  , setProfile
  , profileSelector
  , setProfileSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- profile@
profile :: IsDOMHTMLHeadElement domhtmlHeadElement => domhtmlHeadElement -> IO (Id NSString)
profile domhtmlHeadElement =
  sendMessage domhtmlHeadElement profileSelector

-- | @- setProfile:@
setProfile :: (IsDOMHTMLHeadElement domhtmlHeadElement, IsNSString value) => domhtmlHeadElement -> value -> IO ()
setProfile domhtmlHeadElement value =
  sendMessage domhtmlHeadElement setProfileSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @profile@
profileSelector :: Selector '[] (Id NSString)
profileSelector = mkSelector "profile"

-- | @Selector@ for @setProfile:@
setProfileSelector :: Selector '[Id NSString] ()
setProfileSelector = mkSelector "setProfile:"

