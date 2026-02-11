{-# LANGUAGE PatternSynonyms #-}
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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithVisualCodeType:@
initWithVisualCodeType :: IsINGetVisualCodeIntent inGetVisualCodeIntent => inGetVisualCodeIntent -> INVisualCodeType -> IO (Id INGetVisualCodeIntent)
initWithVisualCodeType inGetVisualCodeIntent  visualCodeType =
  sendMsg inGetVisualCodeIntent (mkSelector "initWithVisualCodeType:") (retPtr retVoid) [argCLong (coerce visualCodeType)] >>= ownedObject . castPtr

-- | @- visualCodeType@
visualCodeType :: IsINGetVisualCodeIntent inGetVisualCodeIntent => inGetVisualCodeIntent -> IO INVisualCodeType
visualCodeType inGetVisualCodeIntent  =
  fmap (coerce :: CLong -> INVisualCodeType) $ sendMsg inGetVisualCodeIntent (mkSelector "visualCodeType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithVisualCodeType:@
initWithVisualCodeTypeSelector :: Selector
initWithVisualCodeTypeSelector = mkSelector "initWithVisualCodeType:"

-- | @Selector@ for @visualCodeType@
visualCodeTypeSelector :: Selector
visualCodeTypeSelector = mkSelector "visualCodeType"

