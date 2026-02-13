{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCreateCommand@.
module ObjC.Foundation.NSCreateCommand
  ( NSCreateCommand
  , IsNSCreateCommand(..)
  , createClassDescription
  , resolvedKeyDictionary
  , createClassDescriptionSelector
  , resolvedKeyDictionarySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- createClassDescription@
createClassDescription :: IsNSCreateCommand nsCreateCommand => nsCreateCommand -> IO (Id NSScriptClassDescription)
createClassDescription nsCreateCommand =
  sendMessage nsCreateCommand createClassDescriptionSelector

-- | @- resolvedKeyDictionary@
resolvedKeyDictionary :: IsNSCreateCommand nsCreateCommand => nsCreateCommand -> IO (Id NSDictionary)
resolvedKeyDictionary nsCreateCommand =
  sendMessage nsCreateCommand resolvedKeyDictionarySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createClassDescription@
createClassDescriptionSelector :: Selector '[] (Id NSScriptClassDescription)
createClassDescriptionSelector = mkSelector "createClassDescription"

-- | @Selector@ for @resolvedKeyDictionary@
resolvedKeyDictionarySelector :: Selector '[] (Id NSDictionary)
resolvedKeyDictionarySelector = mkSelector "resolvedKeyDictionary"

