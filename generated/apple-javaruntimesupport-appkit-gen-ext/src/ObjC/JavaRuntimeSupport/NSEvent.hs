{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSEvent@.
module ObjC.JavaRuntimeSupport.NSEvent
  ( NSEvent
  , IsNSEvent(..)
  , deadKeyCharacter
  , willBeHandledByComplexInputMethod
  , deadKeyCharacterSelector
  , willBeHandledByComplexInputMethodSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- deadKeyCharacter@
deadKeyCharacter :: IsNSEvent nsEvent => nsEvent -> IO CUShort
deadKeyCharacter nsEvent =
  sendMessage nsEvent deadKeyCharacterSelector

-- | @- willBeHandledByComplexInputMethod@
willBeHandledByComplexInputMethod :: IsNSEvent nsEvent => nsEvent -> IO Bool
willBeHandledByComplexInputMethod nsEvent =
  sendMessage nsEvent willBeHandledByComplexInputMethodSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deadKeyCharacter@
deadKeyCharacterSelector :: Selector '[] CUShort
deadKeyCharacterSelector = mkSelector "deadKeyCharacter"

-- | @Selector@ for @willBeHandledByComplexInputMethod@
willBeHandledByComplexInputMethodSelector :: Selector '[] Bool
willBeHandledByComplexInputMethodSelector = mkSelector "willBeHandledByComplexInputMethod"

