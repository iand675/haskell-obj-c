{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAttributeValueWaiter@.
module ObjC.Matter.MTRAttributeValueWaiter
  ( MTRAttributeValueWaiter
  , IsMTRAttributeValueWaiter(..)
  , init_
  , new
  , cancel
  , uuid
  , cancelSelector
  , initSelector
  , newSelector
  , uuidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRAttributeValueWaiter mtrAttributeValueWaiter => mtrAttributeValueWaiter -> IO (Id MTRAttributeValueWaiter)
init_ mtrAttributeValueWaiter =
  sendOwnedMessage mtrAttributeValueWaiter initSelector

-- | @+ new@
new :: IO (Id MTRAttributeValueWaiter)
new  =
  do
    cls' <- getRequiredClass "MTRAttributeValueWaiter"
    sendOwnedClassMessage cls' newSelector

-- | Cancel the wait for the set of attribute path/value pairs represented by this MTRAttributeValueWaiter.  If the completion has not been called yet, it will becalled with MTRErrorCodeCancelled.
--
-- ObjC selector: @- cancel@
cancel :: IsMTRAttributeValueWaiter mtrAttributeValueWaiter => mtrAttributeValueWaiter -> IO ()
cancel mtrAttributeValueWaiter =
  sendMessage mtrAttributeValueWaiter cancelSelector

-- | @- UUID@
uuid :: IsMTRAttributeValueWaiter mtrAttributeValueWaiter => mtrAttributeValueWaiter -> IO (Id NSUUID)
uuid mtrAttributeValueWaiter =
  sendMessage mtrAttributeValueWaiter uuidSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRAttributeValueWaiter)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRAttributeValueWaiter)
newSelector = mkSelector "new"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @UUID@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "UUID"

