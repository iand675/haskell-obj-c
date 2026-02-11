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
  , valueForPropertySelector
  , persistentIDSelector


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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ canFilterByProperty:@
canFilterByProperty :: IsNSString property => property -> IO Bool
canFilterByProperty property =
  do
    cls' <- getRequiredClass "MPMediaEntity"
    withObjCPtr property $ \raw_property ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canFilterByProperty:") retCULong [argPtr (castPtr raw_property :: Ptr ())]

-- | @- enumerateValuesForProperties:usingBlock:@
enumerateValuesForProperties_usingBlock :: (IsMPMediaEntity mpMediaEntity, IsNSSet properties) => mpMediaEntity -> properties -> Ptr () -> IO ()
enumerateValuesForProperties_usingBlock mpMediaEntity  properties block =
withObjCPtr properties $ \raw_properties ->
    sendMsg mpMediaEntity (mkSelector "enumerateValuesForProperties:usingBlock:") retVoid [argPtr (castPtr raw_properties :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- objectForKeyedSubscript:@
objectForKeyedSubscript :: IsMPMediaEntity mpMediaEntity => mpMediaEntity -> RawId -> IO RawId
objectForKeyedSubscript mpMediaEntity  key =
  fmap (RawId . castPtr) $ sendMsg mpMediaEntity (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr (unRawId key) :: Ptr ())]

-- | @- valueForProperty:@
valueForProperty :: (IsMPMediaEntity mpMediaEntity, IsNSString property) => mpMediaEntity -> property -> IO RawId
valueForProperty mpMediaEntity  property =
withObjCPtr property $ \raw_property ->
    fmap (RawId . castPtr) $ sendMsg mpMediaEntity (mkSelector "valueForProperty:") (retPtr retVoid) [argPtr (castPtr raw_property :: Ptr ())]

-- | @- persistentID@
persistentID :: IsMPMediaEntity mpMediaEntity => mpMediaEntity -> IO CULong
persistentID mpMediaEntity  =
  sendMsg mpMediaEntity (mkSelector "persistentID") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @canFilterByProperty:@
canFilterByPropertySelector :: Selector
canFilterByPropertySelector = mkSelector "canFilterByProperty:"

-- | @Selector@ for @enumerateValuesForProperties:usingBlock:@
enumerateValuesForProperties_usingBlockSelector :: Selector
enumerateValuesForProperties_usingBlockSelector = mkSelector "enumerateValuesForProperties:usingBlock:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @valueForProperty:@
valueForPropertySelector :: Selector
valueForPropertySelector = mkSelector "valueForProperty:"

-- | @Selector@ for @persistentID@
persistentIDSelector :: Selector
persistentIDSelector = mkSelector "persistentID"

