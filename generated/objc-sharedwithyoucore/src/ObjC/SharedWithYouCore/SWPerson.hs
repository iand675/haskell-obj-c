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
  , initWithHandle_identity_displayName_thumbnailImageDataSelector
  , initSelector
  , newSelector


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
initWithHandle_identity_displayName_thumbnailImageData swPerson  handle identity displayName thumbnailImageData =
withObjCPtr handle $ \raw_handle ->
  withObjCPtr identity $ \raw_identity ->
    withObjCPtr displayName $ \raw_displayName ->
      withObjCPtr thumbnailImageData $ \raw_thumbnailImageData ->
          sendMsg swPerson (mkSelector "initWithHandle:identity:displayName:thumbnailImageData:") (retPtr retVoid) [argPtr (castPtr raw_handle :: Ptr ()), argPtr (castPtr raw_identity :: Ptr ()), argPtr (castPtr raw_displayName :: Ptr ()), argPtr (castPtr raw_thumbnailImageData :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsSWPerson swPerson => swPerson -> IO (Id SWPerson)
init_ swPerson  =
  sendMsg swPerson (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id SWPerson)
new  =
  do
    cls' <- getRequiredClass "SWPerson"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithHandle:identity:displayName:thumbnailImageData:@
initWithHandle_identity_displayName_thumbnailImageDataSelector :: Selector
initWithHandle_identity_displayName_thumbnailImageDataSelector = mkSelector "initWithHandle:identity:displayName:thumbnailImageData:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

