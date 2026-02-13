{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSQuitCommand@.
module ObjC.Foundation.NSQuitCommand
  ( NSQuitCommand
  , IsNSQuitCommand(..)
  , saveOptions
  , saveOptionsSelector

  -- * Enum types
  , NSSaveOptions(NSSaveOptions)
  , pattern NSSaveOptionsYes
  , pattern NSSaveOptionsNo
  , pattern NSSaveOptionsAsk

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- saveOptions@
saveOptions :: IsNSQuitCommand nsQuitCommand => nsQuitCommand -> IO NSSaveOptions
saveOptions nsQuitCommand =
  sendMessage nsQuitCommand saveOptionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @saveOptions@
saveOptionsSelector :: Selector '[] NSSaveOptions
saveOptionsSelector = mkSelector "saveOptions"

