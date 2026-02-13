{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | OSLogPosition
--
-- An opaque abstraction representing a point in a sequence of entries in the unified logging system.
--
-- Generate positions with OSLogStore instance methods and use them to start viewing entries from a particular starting point.
--
-- Generated bindings for @OSLogPosition@.
module ObjC.OSLog.OSLogPosition
  ( OSLogPosition
  , IsOSLogPosition(..)
  , init_
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OSLog.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsOSLogPosition osLogPosition => osLogPosition -> IO (Id OSLogPosition)
init_ osLogPosition =
  sendOwnedMessage osLogPosition initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id OSLogPosition)
initSelector = mkSelector "init"

