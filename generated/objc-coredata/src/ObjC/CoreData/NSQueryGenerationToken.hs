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

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ currentQueryGenerationToken@
currentQueryGenerationToken :: IO (Id NSQueryGenerationToken)
currentQueryGenerationToken  =
  do
    cls' <- getRequiredClass "NSQueryGenerationToken"
    sendClassMsg cls' (mkSelector "currentQueryGenerationToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currentQueryGenerationToken@
currentQueryGenerationTokenSelector :: Selector
currentQueryGenerationTokenSelector = mkSelector "currentQueryGenerationToken"

