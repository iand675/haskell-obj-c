{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A cursor is a Direction Pad that has its axis extended from [-1; 1] to [width; height] range Up, down, left, right allows to use mouse to simulate DirectionaPad or Thumbstick since values are normalized for these elements
--
-- Generated bindings for @GCDeviceCursor@.
module ObjC.GameController.GCDeviceCursor
  ( GCDeviceCursor
  , IsGCDeviceCursor(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

