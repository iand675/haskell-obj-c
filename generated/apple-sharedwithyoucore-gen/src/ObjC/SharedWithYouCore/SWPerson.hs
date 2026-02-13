{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SWPerson@.
module ObjC.SharedWithYouCore.SWPerson
  ( SWPerson
  , IsSWPerson(..)
  , initWithHandle_identity_displayName_thumbnailImageData
  , init_
  , new
  , initSelector
  , initWithHandle_identity_displayName_thumbnailImageDataSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYouCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An initializer
--
-- @handle@ — The phone number or email address for this person.
--
-- @identity@ — The identity of this person.
--
-- @displayName@ — The name of this person.
--
-- @thumbnailImageData@ — Optional thumbnail image data for this person. If nil, this will be inferred by the system.
--
-- ObjC selector: @- initWithHandle:identity:displayName:thumbnailImageData:@
initWithHandle_identity_displayName_thumbnailImageData :: (IsSWPerson swPerson, IsNSString handle, IsSWPersonIdentity identity, IsNSString displayName, IsNSData thumbnailImageData) => swPerson -> handle -> identity -> displayName -> thumbnailImageData -> IO (Id SWPerson)
initWithHandle_identity_displayName_thumbnailImageData swPerson handle identity displayName thumbnailImageData =
  sendOwnedMessage swPerson initWithHandle_identity_displayName_thumbnailImageDataSelector (toNSString handle) (toSWPersonIdentity identity) (toNSString displayName) (toNSData thumbnailImageData)

-- | @- init@
init_ :: IsSWPerson swPerson => swPerson -> IO (Id SWPerson)
init_ swPerson =
  sendOwnedMessage swPerson initSelector

-- | @+ new@
new :: IO (Id SWPerson)
new  =
  do
    cls' <- getRequiredClass "SWPerson"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHandle:identity:displayName:thumbnailImageData:@
initWithHandle_identity_displayName_thumbnailImageDataSelector :: Selector '[Id NSString, Id SWPersonIdentity, Id NSString, Id NSData] (Id SWPerson)
initWithHandle_identity_displayName_thumbnailImageDataSelector = mkSelector "initWithHandle:identity:displayName:thumbnailImageData:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id SWPerson)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id SWPerson)
newSelector = mkSelector "new"

