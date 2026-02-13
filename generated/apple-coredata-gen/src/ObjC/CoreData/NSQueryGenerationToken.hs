{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSQueryGenerationToken@.
module ObjC.CoreData.NSQueryGenerationToken
  ( NSQueryGenerationToken
  , IsNSQueryGenerationToken(..)
  , currentQueryGenerationToken
  , currentQueryGenerationTokenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ currentQueryGenerationToken@
currentQueryGenerationToken :: IO (Id NSQueryGenerationToken)
currentQueryGenerationToken  =
  do
    cls' <- getRequiredClass "NSQueryGenerationToken"
    sendClassMessage cls' currentQueryGenerationTokenSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentQueryGenerationToken@
currentQueryGenerationTokenSelector :: Selector '[] (Id NSQueryGenerationToken)
currentQueryGenerationTokenSelector = mkSelector "currentQueryGenerationToken"

