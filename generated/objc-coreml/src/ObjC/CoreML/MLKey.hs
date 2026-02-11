{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing key used to store any value against
--
-- Generated bindings for @MLKey@.
module ObjC.CoreML.MLKey
  ( MLKey
  , IsMLKey(..)
  , init_
  , new
  , name
  , scope
  , initSelector
  , newSelector
  , nameSelector
  , scopeSelector


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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLKey mlKey => mlKey -> IO (Id MLKey)
init_ mlKey  =
  sendMsg mlKey (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLKey"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "new") (retPtr retVoid) []

-- | @- name@
name :: IsMLKey mlKey => mlKey -> IO (Id NSString)
name mlKey  =
  sendMsg mlKey (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- scope@
scope :: IsMLKey mlKey => mlKey -> IO (Id NSString)
scope mlKey  =
  sendMsg mlKey (mkSelector "scope") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @scope@
scopeSelector :: Selector
scopeSelector = mkSelector "scope"

