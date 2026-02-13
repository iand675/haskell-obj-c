{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains the cloud identifier result from looking up a local identifier via @cloudIdentifierMappingsForLocalIdentifiers,@ or an @error@ indicating why the lookup failed
--
-- Generated bindings for @PHCloudIdentifierMapping@.
module ObjC.Photos.PHCloudIdentifierMapping
  ( PHCloudIdentifierMapping
  , IsPHCloudIdentifierMapping(..)
  , cloudIdentifier
  , error_
  , cloudIdentifierSelector
  , errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cloudIdentifier@
cloudIdentifier :: IsPHCloudIdentifierMapping phCloudIdentifierMapping => phCloudIdentifierMapping -> IO (Id PHCloudIdentifier)
cloudIdentifier phCloudIdentifierMapping =
  sendMessage phCloudIdentifierMapping cloudIdentifierSelector

-- | The cloud identifier of the resource found for this local identifier
--
-- ObjC selector: @- error@
error_ :: IsPHCloudIdentifierMapping phCloudIdentifierMapping => phCloudIdentifierMapping -> IO (Id NSError)
error_ phCloudIdentifierMapping =
  sendMessage phCloudIdentifierMapping errorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cloudIdentifier@
cloudIdentifierSelector :: Selector '[] (Id PHCloudIdentifier)
cloudIdentifierSelector = mkSelector "cloudIdentifier"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

