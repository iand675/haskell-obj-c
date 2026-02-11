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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- cloudIdentifier@
cloudIdentifier :: IsPHCloudIdentifierMapping phCloudIdentifierMapping => phCloudIdentifierMapping -> IO (Id PHCloudIdentifier)
cloudIdentifier phCloudIdentifierMapping  =
  sendMsg phCloudIdentifierMapping (mkSelector "cloudIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The cloud identifier of the resource found for this local identifier
--
-- ObjC selector: @- error@
error_ :: IsPHCloudIdentifierMapping phCloudIdentifierMapping => phCloudIdentifierMapping -> IO (Id NSError)
error_ phCloudIdentifierMapping  =
  sendMsg phCloudIdentifierMapping (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cloudIdentifier@
cloudIdentifierSelector :: Selector
cloudIdentifierSelector = mkSelector "cloudIdentifier"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

