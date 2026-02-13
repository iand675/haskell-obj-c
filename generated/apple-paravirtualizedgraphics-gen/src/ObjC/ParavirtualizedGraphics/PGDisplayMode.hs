{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | PGDisplayMode:
--
-- Description of supported display mode.
--
-- Client of PGDisplay can dynamically supply NSArray of PGDisplayMode objects to convey supported modes.  The first mode in array is preferred.
--
-- Generated bindings for @PGDisplayMode@.
module ObjC.ParavirtualizedGraphics.PGDisplayMode
  ( PGDisplayMode
  , IsPGDisplayMode(..)
  , refreshRate
  , refreshRateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ParavirtualizedGraphics.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | refreshRateInHz
--
-- refreshRate of supported display mode.  Consider only supplying modes using a refreshRate equal to that of host OS's physical display where representation is ultimately shown.
--
-- ObjC selector: @- refreshRate@
refreshRate :: IsPGDisplayMode pgDisplayMode => pgDisplayMode -> IO CDouble
refreshRate pgDisplayMode =
  sendMessage pgDisplayMode refreshRateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @refreshRate@
refreshRateSelector :: Selector '[] CDouble
refreshRateSelector = mkSelector "refreshRate"

