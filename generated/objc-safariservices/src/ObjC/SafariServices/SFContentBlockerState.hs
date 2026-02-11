{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SFContentBlockerState@.
module ObjC.SafariServices.SFContentBlockerState
  ( SFContentBlockerState
  , IsSFContentBlockerState(..)
  , enabled
  , enabledSelector


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

import ObjC.SafariServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- enabled@
enabled :: IsSFContentBlockerState sfContentBlockerState => sfContentBlockerState -> IO Bool
enabled sfContentBlockerState  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfContentBlockerState (mkSelector "enabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

