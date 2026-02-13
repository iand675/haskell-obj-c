{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INShareFocusStatusIntent@.
module ObjC.Intents.INShareFocusStatusIntent
  ( INShareFocusStatusIntent
  , IsINShareFocusStatusIntent(..)
  , initWithFocusStatus
  , focusStatus
  , focusStatusSelector
  , initWithFocusStatusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithFocusStatus:@
initWithFocusStatus :: (IsINShareFocusStatusIntent inShareFocusStatusIntent, IsINFocusStatus focusStatus) => inShareFocusStatusIntent -> focusStatus -> IO (Id INShareFocusStatusIntent)
initWithFocusStatus inShareFocusStatusIntent focusStatus =
  sendOwnedMessage inShareFocusStatusIntent initWithFocusStatusSelector (toINFocusStatus focusStatus)

-- | @- focusStatus@
focusStatus :: IsINShareFocusStatusIntent inShareFocusStatusIntent => inShareFocusStatusIntent -> IO (Id INFocusStatus)
focusStatus inShareFocusStatusIntent =
  sendMessage inShareFocusStatusIntent focusStatusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFocusStatus:@
initWithFocusStatusSelector :: Selector '[Id INFocusStatus] (Id INShareFocusStatusIntent)
initWithFocusStatusSelector = mkSelector "initWithFocusStatus:"

-- | @Selector@ for @focusStatus@
focusStatusSelector :: Selector '[] (Id INFocusStatus)
focusStatusSelector = mkSelector "focusStatus"

