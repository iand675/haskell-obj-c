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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- echosBullets@
echosBullets :: IsNSSecureTextFieldCell nsSecureTextFieldCell => nsSecureTextFieldCell -> IO Bool
echosBullets nsSecureTextFieldCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSecureTextFieldCell (mkSelector "echosBullets") retCULong []

-- | @- setEchosBullets:@
setEchosBullets :: IsNSSecureTextFieldCell nsSecureTextFieldCell => nsSecureTextFieldCell -> Bool -> IO ()
setEchosBullets nsSecureTextFieldCell  value =
  sendMsg nsSecureTextFieldCell (mkSelector "setEchosBullets:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @echosBullets@
echosBulletsSelector :: Selector
echosBulletsSelector = mkSelector "echosBullets"

-- | @Selector@ for @setEchosBullets:@
setEchosBulletsSelector :: Selector
setEchosBulletsSelector = mkSelector "setEchosBullets:"

