{-# LANGUAGE DataKinds #-}
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
  , dateCreatedSelector
  , dateLastModifiedSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ClassKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id CLSObject)
new  =
  do
    cls' <- getRequiredClass "CLSObject"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsCLSObject clsObject => clsObject -> IO (Id CLSObject)
init_ clsObject =
  sendOwnedMessage clsObject initSelector

-- | The date this object was created.
--
-- ObjC selector: @- dateCreated@
dateCreated :: IsCLSObject clsObject => clsObject -> IO (Id NSDate)
dateCreated clsObject =
  sendMessage clsObject dateCreatedSelector

-- | The date this object was last modified.
--
-- ObjC selector: @- dateLastModified@
dateLastModified :: IsCLSObject clsObject => clsObject -> IO (Id NSDate)
dateLastModified clsObject =
  sendMessage clsObject dateLastModifiedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CLSObject)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CLSObject)
initSelector = mkSelector "init"

-- | @Selector@ for @dateCreated@
dateCreatedSelector :: Selector '[] (Id NSDate)
dateCreatedSelector = mkSelector "dateCreated"

-- | @Selector@ for @dateLastModified@
dateLastModifiedSelector :: Selector '[] (Id NSDate)
dateLastModifiedSelector = mkSelector "dateLastModified"

