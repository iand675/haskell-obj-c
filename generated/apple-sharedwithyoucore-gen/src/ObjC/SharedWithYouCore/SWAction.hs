{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SWAction@.
module ObjC.SharedWithYouCore.SWAction
  ( SWAction
  , IsSWAction(..)
  , fulfill
  , fail_
  , uuid
  , complete
  , completeSelector
  , failSelector
  , fulfillSelector
  , uuidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- fulfill@
fulfill :: IsSWAction swAction => swAction -> IO ()
fulfill swAction =
  sendMessage swAction fulfillSelector

-- | @- fail@
fail_ :: IsSWAction swAction => swAction -> IO ()
fail_ swAction =
  sendMessage swAction failSelector

-- | @- uuid@
uuid :: IsSWAction swAction => swAction -> IO (Id NSUUID)
uuid swAction =
  sendMessage swAction uuidSelector

-- | @- complete@
complete :: IsSWAction swAction => swAction -> IO Bool
complete swAction =
  sendMessage swAction completeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fulfill@
fulfillSelector :: Selector '[] ()
fulfillSelector = mkSelector "fulfill"

-- | @Selector@ for @fail@
failSelector :: Selector '[] ()
failSelector = mkSelector "fail"

-- | @Selector@ for @uuid@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "uuid"

-- | @Selector@ for @complete@
completeSelector :: Selector '[] Bool
completeSelector = mkSelector "complete"

