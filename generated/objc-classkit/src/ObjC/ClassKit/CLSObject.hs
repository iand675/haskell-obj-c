{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object managed by ClassKit.
--
-- See @CLSContext@ for more details.
--
-- Generated bindings for @CLSObject@.
module ObjC.ClassKit.CLSObject
  ( CLSObject
  , IsCLSObject(..)
  , new
  , init_
  , dateCreated
  , dateLastModified
  , newSelector
  , initSelector
  , dateCreatedSelector
  , dateLastModifiedSelector


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

import ObjC.ClassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id CLSObject)
new  =
  do
    cls' <- getRequiredClass "CLSObject"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsCLSObject clsObject => clsObject -> IO (Id CLSObject)
init_ clsObject  =
  sendMsg clsObject (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The date this object was created.
--
-- ObjC selector: @- dateCreated@
dateCreated :: IsCLSObject clsObject => clsObject -> IO (Id NSDate)
dateCreated clsObject  =
  sendMsg clsObject (mkSelector "dateCreated") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The date this object was last modified.
--
-- ObjC selector: @- dateLastModified@
dateLastModified :: IsCLSObject clsObject => clsObject -> IO (Id NSDate)
dateLastModified clsObject  =
  sendMsg clsObject (mkSelector "dateLastModified") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @dateCreated@
dateCreatedSelector :: Selector
dateCreatedSelector = mkSelector "dateCreated"

-- | @Selector@ for @dateLastModified@
dateLastModifiedSelector :: Selector
dateLastModifiedSelector = mkSelector "dateLastModified"

