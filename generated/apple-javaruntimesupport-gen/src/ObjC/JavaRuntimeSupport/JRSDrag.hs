{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @JRSDrag@.
module ObjC.JavaRuntimeSupport.JRSDrag
  ( JRSDrag
  , IsJRSDrag(..)
  , currentAllowableActions
  , currentModifiers
  , currentAllowableActionsSelector
  , currentModifiersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ currentAllowableActions@
currentAllowableActions :: IO CInt
currentAllowableActions  =
  do
    cls' <- getRequiredClass "JRSDrag"
    sendClassMessage cls' currentAllowableActionsSelector

-- | @+ currentModifiers@
currentModifiers :: IO CULong
currentModifiers  =
  do
    cls' <- getRequiredClass "JRSDrag"
    sendClassMessage cls' currentModifiersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentAllowableActions@
currentAllowableActionsSelector :: Selector '[] CInt
currentAllowableActionsSelector = mkSelector "currentAllowableActions"

-- | @Selector@ for @currentModifiers@
currentModifiersSelector :: Selector '[] CULong
currentModifiersSelector = mkSelector "currentModifiers"

