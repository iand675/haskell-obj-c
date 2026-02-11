{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSIncrementalStoreNode@.
module ObjC.CoreData.NSIncrementalStoreNode
  ( NSIncrementalStoreNode
  , IsNSIncrementalStoreNode(..)
  , initWithObjectID_withValues_version
  , updateWithValues_version
  , valueForPropertyDescription
  , objectID
  , version
  , initWithObjectID_withValues_versionSelector
  , updateWithValues_versionSelector
  , valueForPropertyDescriptionSelector
  , objectIDSelector
  , versionSelector


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

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithObjectID:withValues:version:@
initWithObjectID_withValues_version :: (IsNSIncrementalStoreNode nsIncrementalStoreNode, IsNSManagedObjectID objectID, IsNSDictionary values) => nsIncrementalStoreNode -> objectID -> values -> CULong -> IO (Id NSIncrementalStoreNode)
initWithObjectID_withValues_version nsIncrementalStoreNode  objectID values version =
withObjCPtr objectID $ \raw_objectID ->
  withObjCPtr values $ \raw_values ->
      sendMsg nsIncrementalStoreNode (mkSelector "initWithObjectID:withValues:version:") (retPtr retVoid) [argPtr (castPtr raw_objectID :: Ptr ()), argPtr (castPtr raw_values :: Ptr ()), argCULong (fromIntegral version)] >>= ownedObject . castPtr

-- | @- updateWithValues:version:@
updateWithValues_version :: (IsNSIncrementalStoreNode nsIncrementalStoreNode, IsNSDictionary values) => nsIncrementalStoreNode -> values -> CULong -> IO ()
updateWithValues_version nsIncrementalStoreNode  values version =
withObjCPtr values $ \raw_values ->
    sendMsg nsIncrementalStoreNode (mkSelector "updateWithValues:version:") retVoid [argPtr (castPtr raw_values :: Ptr ()), argCULong (fromIntegral version)]

-- | @- valueForPropertyDescription:@
valueForPropertyDescription :: (IsNSIncrementalStoreNode nsIncrementalStoreNode, IsNSPropertyDescription prop) => nsIncrementalStoreNode -> prop -> IO RawId
valueForPropertyDescription nsIncrementalStoreNode  prop =
withObjCPtr prop $ \raw_prop ->
    fmap (RawId . castPtr) $ sendMsg nsIncrementalStoreNode (mkSelector "valueForPropertyDescription:") (retPtr retVoid) [argPtr (castPtr raw_prop :: Ptr ())]

-- | @- objectID@
objectID :: IsNSIncrementalStoreNode nsIncrementalStoreNode => nsIncrementalStoreNode -> IO (Id NSManagedObjectID)
objectID nsIncrementalStoreNode  =
  sendMsg nsIncrementalStoreNode (mkSelector "objectID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- version@
version :: IsNSIncrementalStoreNode nsIncrementalStoreNode => nsIncrementalStoreNode -> IO CULong
version nsIncrementalStoreNode  =
  sendMsg nsIncrementalStoreNode (mkSelector "version") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithObjectID:withValues:version:@
initWithObjectID_withValues_versionSelector :: Selector
initWithObjectID_withValues_versionSelector = mkSelector "initWithObjectID:withValues:version:"

-- | @Selector@ for @updateWithValues:version:@
updateWithValues_versionSelector :: Selector
updateWithValues_versionSelector = mkSelector "updateWithValues:version:"

-- | @Selector@ for @valueForPropertyDescription:@
valueForPropertyDescriptionSelector :: Selector
valueForPropertyDescriptionSelector = mkSelector "valueForPropertyDescription:"

-- | @Selector@ for @objectID@
objectIDSelector :: Selector
objectIDSelector = mkSelector "objectID"

-- | @Selector@ for @version@
versionSelector :: Selector
versionSelector = mkSelector "version"

