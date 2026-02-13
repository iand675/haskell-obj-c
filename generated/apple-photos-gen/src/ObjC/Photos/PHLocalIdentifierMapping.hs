{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains the local identifier result from looking up a cloud identifier via @localIdentifierMappingsForCloudIdentifiers,@ or an @error@ indicating why the lookup failed
--
-- Generated bindings for @PHLocalIdentifierMapping@.
module ObjC.Photos.PHLocalIdentifierMapping
  ( PHLocalIdentifierMapping
  , IsPHLocalIdentifierMapping(..)
  , localIdentifier
  , error_
  , errorSelector
  , localIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- localIdentifier@
localIdentifier :: IsPHLocalIdentifierMapping phLocalIdentifierMapping => phLocalIdentifierMapping -> IO (Id NSString)
localIdentifier phLocalIdentifierMapping =
  sendMessage phLocalIdentifierMapping localIdentifierSelector

-- | The @NSString@ representing the local identifier of the resource found for this cloud identifier, or nil if the match was not found.
--
-- ObjC selector: @- error@
error_ :: IsPHLocalIdentifierMapping phLocalIdentifierMapping => phLocalIdentifierMapping -> IO (Id NSError)
error_ phLocalIdentifierMapping =
  sendMessage phLocalIdentifierMapping errorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localIdentifier@
localIdentifierSelector :: Selector '[] (Id NSString)
localIdentifierSelector = mkSelector "localIdentifier"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

