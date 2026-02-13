{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserActivity@.
module ObjC.CoreSpotlight.NSUserActivity
  ( NSUserActivity
  , IsNSUserActivity(..)
  , contentAttributeSet
  , setContentAttributeSet
  , contentAttributeSetSelector
  , setContentAttributeSetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- contentAttributeSet@
contentAttributeSet :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id CSSearchableItemAttributeSet)
contentAttributeSet nsUserActivity =
  sendMessage nsUserActivity contentAttributeSetSelector

-- | @- setContentAttributeSet:@
setContentAttributeSet :: (IsNSUserActivity nsUserActivity, IsCSSearchableItemAttributeSet value) => nsUserActivity -> value -> IO ()
setContentAttributeSet nsUserActivity value =
  sendMessage nsUserActivity setContentAttributeSetSelector (toCSSearchableItemAttributeSet value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentAttributeSet@
contentAttributeSetSelector :: Selector '[] (Id CSSearchableItemAttributeSet)
contentAttributeSetSelector = mkSelector "contentAttributeSet"

-- | @Selector@ for @setContentAttributeSet:@
setContentAttributeSetSelector :: Selector '[Id CSSearchableItemAttributeSet] ()
setContentAttributeSetSelector = mkSelector "setContentAttributeSet:"

