{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INFocusStatus@.
module ObjC.Intents.INFocusStatus
  ( INFocusStatus
  , IsINFocusStatus(..)
  , init_
  , initWithIsFocused
  , isFocused
  , initSelector
  , initWithIsFocusedSelector
  , isFocusedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINFocusStatus inFocusStatus => inFocusStatus -> IO (Id INFocusStatus)
init_ inFocusStatus =
  sendOwnedMessage inFocusStatus initSelector

-- | @- initWithIsFocused:@
initWithIsFocused :: (IsINFocusStatus inFocusStatus, IsNSNumber isFocused) => inFocusStatus -> isFocused -> IO (Id INFocusStatus)
initWithIsFocused inFocusStatus isFocused =
  sendOwnedMessage inFocusStatus initWithIsFocusedSelector (toNSNumber isFocused)

-- | @- isFocused@
isFocused :: IsINFocusStatus inFocusStatus => inFocusStatus -> IO (Id NSNumber)
isFocused inFocusStatus =
  sendMessage inFocusStatus isFocusedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INFocusStatus)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIsFocused:@
initWithIsFocusedSelector :: Selector '[Id NSNumber] (Id INFocusStatus)
initWithIsFocusedSelector = mkSelector "initWithIsFocused:"

-- | @Selector@ for @isFocused@
isFocusedSelector :: Selector '[] (Id NSNumber)
isFocusedSelector = mkSelector "isFocused"

