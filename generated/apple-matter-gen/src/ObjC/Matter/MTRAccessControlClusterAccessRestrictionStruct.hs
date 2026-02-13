{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAccessControlClusterAccessRestrictionStruct@.
module ObjC.Matter.MTRAccessControlClusterAccessRestrictionStruct
  ( MTRAccessControlClusterAccessRestrictionStruct
  , IsMTRAccessControlClusterAccessRestrictionStruct(..)
  , type_
  , setType
  , id_
  , setId
  , idSelector
  , setIdSelector
  , setTypeSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- type@
type_ :: IsMTRAccessControlClusterAccessRestrictionStruct mtrAccessControlClusterAccessRestrictionStruct => mtrAccessControlClusterAccessRestrictionStruct -> IO (Id NSNumber)
type_ mtrAccessControlClusterAccessRestrictionStruct =
  sendMessage mtrAccessControlClusterAccessRestrictionStruct typeSelector

-- | @- setType:@
setType :: (IsMTRAccessControlClusterAccessRestrictionStruct mtrAccessControlClusterAccessRestrictionStruct, IsNSNumber value) => mtrAccessControlClusterAccessRestrictionStruct -> value -> IO ()
setType mtrAccessControlClusterAccessRestrictionStruct value =
  sendMessage mtrAccessControlClusterAccessRestrictionStruct setTypeSelector (toNSNumber value)

-- | @- id@
id_ :: IsMTRAccessControlClusterAccessRestrictionStruct mtrAccessControlClusterAccessRestrictionStruct => mtrAccessControlClusterAccessRestrictionStruct -> IO (Id NSNumber)
id_ mtrAccessControlClusterAccessRestrictionStruct =
  sendMessage mtrAccessControlClusterAccessRestrictionStruct idSelector

-- | @- setId:@
setId :: (IsMTRAccessControlClusterAccessRestrictionStruct mtrAccessControlClusterAccessRestrictionStruct, IsNSNumber value) => mtrAccessControlClusterAccessRestrictionStruct -> value -> IO ()
setId mtrAccessControlClusterAccessRestrictionStruct value =
  sendMessage mtrAccessControlClusterAccessRestrictionStruct setIdSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSNumber)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSNumber] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @id@
idSelector :: Selector '[] (Id NSNumber)
idSelector = mkSelector "id"

-- | @Selector@ for @setId:@
setIdSelector :: Selector '[Id NSNumber] ()
setIdSelector = mkSelector "setId:"

