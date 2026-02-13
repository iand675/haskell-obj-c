{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INGetVisualCodeIntent@.
module ObjC.Intents.INGetVisualCodeIntent
  ( INGetVisualCodeIntent
  , IsINGetVisualCodeIntent(..)
  , initWithVisualCodeType
  , visualCodeType
  , initWithVisualCodeTypeSelector
  , visualCodeTypeSelector

  -- * Enum types
  , INVisualCodeType(INVisualCodeType)
  , pattern INVisualCodeTypeUnknown
  , pattern INVisualCodeTypeContact
  , pattern INVisualCodeTypeRequestPayment
  , pattern INVisualCodeTypeSendPayment
  , pattern INVisualCodeTypeTransit
  , pattern INVisualCodeTypeBus
  , pattern INVisualCodeTypeSubway

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithVisualCodeType:@
initWithVisualCodeType :: IsINGetVisualCodeIntent inGetVisualCodeIntent => inGetVisualCodeIntent -> INVisualCodeType -> IO (Id INGetVisualCodeIntent)
initWithVisualCodeType inGetVisualCodeIntent visualCodeType =
  sendOwnedMessage inGetVisualCodeIntent initWithVisualCodeTypeSelector visualCodeType

-- | @- visualCodeType@
visualCodeType :: IsINGetVisualCodeIntent inGetVisualCodeIntent => inGetVisualCodeIntent -> IO INVisualCodeType
visualCodeType inGetVisualCodeIntent =
  sendMessage inGetVisualCodeIntent visualCodeTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithVisualCodeType:@
initWithVisualCodeTypeSelector :: Selector '[INVisualCodeType] (Id INGetVisualCodeIntent)
initWithVisualCodeTypeSelector = mkSelector "initWithVisualCodeType:"

-- | @Selector@ for @visualCodeType@
visualCodeTypeSelector :: Selector '[] INVisualCodeType
visualCodeTypeSelector = mkSelector "visualCodeType"

