{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHPersistentChange@.
module ObjC.Photos.PHPersistentChange
  ( PHPersistentChange
  , IsPHPersistentChange(..)
  , new
  , init_
  , changeDetailsForObjectType_error
  , changeToken
  , newSelector
  , initSelector
  , changeDetailsForObjectType_errorSelector
  , changeTokenSelector

  -- * Enum types
  , PHObjectType(PHObjectType)
  , pattern PHObjectTypeAsset
  , pattern PHObjectTypeAssetCollection
  , pattern PHObjectTypeCollectionList

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
import ObjC.Photos.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id PHPersistentChange)
new  =
  do
    cls' <- getRequiredClass "PHPersistentChange"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsPHPersistentChange phPersistentChange => phPersistentChange -> IO (Id PHPersistentChange)
init_ phPersistentChange  =
  sendMsg phPersistentChange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- changeDetailsForObjectType:error:@
changeDetailsForObjectType_error :: (IsPHPersistentChange phPersistentChange, IsNSError error_) => phPersistentChange -> PHObjectType -> error_ -> IO (Id PHPersistentObjectChangeDetails)
changeDetailsForObjectType_error phPersistentChange  objectType error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg phPersistentChange (mkSelector "changeDetailsForObjectType:error:") (retPtr retVoid) [argCLong (coerce objectType), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- changeToken@
changeToken :: IsPHPersistentChange phPersistentChange => phPersistentChange -> IO (Id PHPersistentChangeToken)
changeToken phPersistentChange  =
  sendMsg phPersistentChange (mkSelector "changeToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @changeDetailsForObjectType:error:@
changeDetailsForObjectType_errorSelector :: Selector
changeDetailsForObjectType_errorSelector = mkSelector "changeDetailsForObjectType:error:"

-- | @Selector@ for @changeToken@
changeTokenSelector :: Selector
changeTokenSelector = mkSelector "changeToken"

