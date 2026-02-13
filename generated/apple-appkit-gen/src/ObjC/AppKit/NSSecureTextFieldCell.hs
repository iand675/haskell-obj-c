{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSecureTextFieldCell@.
module ObjC.AppKit.NSSecureTextFieldCell
  ( NSSecureTextFieldCell
  , IsNSSecureTextFieldCell(..)
  , echosBullets
  , setEchosBullets
  , echosBulletsSelector
  , setEchosBulletsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- echosBullets@
echosBullets :: IsNSSecureTextFieldCell nsSecureTextFieldCell => nsSecureTextFieldCell -> IO Bool
echosBullets nsSecureTextFieldCell =
  sendMessage nsSecureTextFieldCell echosBulletsSelector

-- | @- setEchosBullets:@
setEchosBullets :: IsNSSecureTextFieldCell nsSecureTextFieldCell => nsSecureTextFieldCell -> Bool -> IO ()
setEchosBullets nsSecureTextFieldCell value =
  sendMessage nsSecureTextFieldCell setEchosBulletsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @echosBullets@
echosBulletsSelector :: Selector '[] Bool
echosBulletsSelector = mkSelector "echosBullets"

-- | @Selector@ for @setEchosBullets:@
setEchosBulletsSelector :: Selector '[Bool] ()
setEchosBulletsSelector = mkSelector "setEchosBullets:"

