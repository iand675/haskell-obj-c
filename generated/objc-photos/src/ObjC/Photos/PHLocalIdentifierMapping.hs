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
  , localIdentifierSelector
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

-- | @- localIdentifier@
localIdentifier :: IsPHLocalIdentifierMapping phLocalIdentifierMapping => phLocalIdentifierMapping -> IO (Id NSString)
localIdentifier phLocalIdentifierMapping  =
  sendMsg phLocalIdentifierMapping (mkSelector "localIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The @NSString@ representing the local identifier of the resource found for this cloud identifier, or nil if the match was not found.
--
-- ObjC selector: @- error@
error_ :: IsPHLocalIdentifierMapping phLocalIdentifierMapping => phLocalIdentifierMapping -> IO (Id NSError)
error_ phLocalIdentifierMapping  =
  sendMsg phLocalIdentifierMapping (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @localIdentifier@
localIdentifierSelector :: Selector
localIdentifierSelector = mkSelector "localIdentifier"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

