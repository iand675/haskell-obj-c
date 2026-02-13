{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMediaEntity@.
module ObjC.MediaPlayer.MPMediaEntity
  ( MPMediaEntity
  , IsMPMediaEntity(..)
  , canFilterByProperty
  , enumerateValuesForProperties_usingBlock
  , objectForKeyedSubscript
  , valueForProperty
  , persistentID
  , canFilterByPropertySelector
  , enumerateValuesForProperties_usingBlockSelector
  , objectForKeyedSubscriptSelector
  , persistentIDSelector
  , valueForPropertySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ canFilterByProperty:@
canFilterByProperty :: IsNSString property => property -> IO Bool
canFilterByProperty property =
  do
    cls' <- getRequiredClass "MPMediaEntity"
    sendClassMessage cls' canFilterByPropertySelector (toNSString property)

-- | @- enumerateValuesForProperties:usingBlock:@
enumerateValuesForProperties_usingBlock :: (IsMPMediaEntity mpMediaEntity, IsNSSet properties) => mpMediaEntity -> properties -> Ptr () -> IO ()
enumerateValuesForProperties_usingBlock mpMediaEntity properties block =
  sendMessage mpMediaEntity enumerateValuesForProperties_usingBlockSelector (toNSSet properties) block

-- | @- objectForKeyedSubscript:@
objectForKeyedSubscript :: IsMPMediaEntity mpMediaEntity => mpMediaEntity -> RawId -> IO RawId
objectForKeyedSubscript mpMediaEntity key =
  sendMessage mpMediaEntity objectForKeyedSubscriptSelector key

-- | @- valueForProperty:@
valueForProperty :: (IsMPMediaEntity mpMediaEntity, IsNSString property) => mpMediaEntity -> property -> IO RawId
valueForProperty mpMediaEntity property =
  sendMessage mpMediaEntity valueForPropertySelector (toNSString property)

-- | @- persistentID@
persistentID :: IsMPMediaEntity mpMediaEntity => mpMediaEntity -> IO CULong
persistentID mpMediaEntity =
  sendMessage mpMediaEntity persistentIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canFilterByProperty:@
canFilterByPropertySelector :: Selector '[Id NSString] Bool
canFilterByPropertySelector = mkSelector "canFilterByProperty:"

-- | @Selector@ for @enumerateValuesForProperties:usingBlock:@
enumerateValuesForProperties_usingBlockSelector :: Selector '[Id NSSet, Ptr ()] ()
enumerateValuesForProperties_usingBlockSelector = mkSelector "enumerateValuesForProperties:usingBlock:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector '[RawId] RawId
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @valueForProperty:@
valueForPropertySelector :: Selector '[Id NSString] RawId
valueForPropertySelector = mkSelector "valueForProperty:"

-- | @Selector@ for @persistentID@
persistentIDSelector :: Selector '[] CULong
persistentIDSelector = mkSelector "persistentID"

