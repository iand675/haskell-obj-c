{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCloseCommand@.
module ObjC.Foundation.NSCloseCommand
  ( NSCloseCommand
  , IsNSCloseCommand(..)
  , saveOptions
  , saveOptionsSelector

  -- * Enum types
  , NSSaveOptions(NSSaveOptions)
  , pattern NSSaveOptionsYes
  , pattern NSSaveOptionsNo
  , pattern NSSaveOptionsAsk

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- saveOptions@
saveOptions :: IsNSCloseCommand nsCloseCommand => nsCloseCommand -> IO NSSaveOptions
saveOptions nsCloseCommand  =
  fmap (coerce :: CULong -> NSSaveOptions) $ sendMsg nsCloseCommand (mkSelector "saveOptions") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @saveOptions@
saveOptionsSelector :: Selector
saveOptionsSelector = mkSelector "saveOptions"

