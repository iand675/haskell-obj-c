{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ABPerson@.
module ObjC.AddressBook.ABPerson
  ( ABPerson
  , IsABPerson(..)
  , parentGroups
  , linkedPeople
  , setImageData
  , imageData
  , beginLoadingImageDataForClient
  , cancelLoadingImageDataForTag
  , initWithVCardRepresentation
  , vCardRepresentation
  , searchElementForProperty_label_key_value_comparison
  , addPropertiesAndTypes
  , removeProperties
  , properties
  , typeOfProperty
  , parentGroupsSelector
  , linkedPeopleSelector
  , setImageDataSelector
  , imageDataSelector
  , beginLoadingImageDataForClientSelector
  , cancelLoadingImageDataForTagSelector
  , initWithVCardRepresentationSelector
  , vCardRepresentationSelector
  , searchElementForProperty_label_key_value_comparisonSelector
  , addPropertiesAndTypesSelector
  , removePropertiesSelector
  , propertiesSelector
  , typeOfPropertySelector


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

import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- parentGroups@
parentGroups :: IsABPerson abPerson => abPerson -> IO (Id NSArray)
parentGroups abPerson  =
  sendMsg abPerson (mkSelector "parentGroups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- linkedPeople@
linkedPeople :: IsABPerson abPerson => abPerson -> IO (Id NSArray)
linkedPeople abPerson  =
  sendMsg abPerson (mkSelector "linkedPeople") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageData:@
setImageData :: (IsABPerson abPerson, IsNSData data_) => abPerson -> data_ -> IO Bool
setImageData abPerson  data_ =
withObjCPtr data_ $ \raw_data_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg abPerson (mkSelector "setImageData:") retCULong [argPtr (castPtr raw_data_ :: Ptr ())]

-- | @- imageData@
imageData :: IsABPerson abPerson => abPerson -> IO (Id NSData)
imageData abPerson  =
  sendMsg abPerson (mkSelector "imageData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- beginLoadingImageDataForClient:@
beginLoadingImageDataForClient :: IsABPerson abPerson => abPerson -> RawId -> IO CLong
beginLoadingImageDataForClient abPerson  client =
  sendMsg abPerson (mkSelector "beginLoadingImageDataForClient:") retCLong [argPtr (castPtr (unRawId client) :: Ptr ())]

-- | @+ cancelLoadingImageDataForTag:@
cancelLoadingImageDataForTag :: CLong -> IO ()
cancelLoadingImageDataForTag tag =
  do
    cls' <- getRequiredClass "ABPerson"
    sendClassMsg cls' (mkSelector "cancelLoadingImageDataForTag:") retVoid [argCLong (fromIntegral tag)]

-- | @- initWithVCardRepresentation:@
initWithVCardRepresentation :: (IsABPerson abPerson, IsNSData vCardData) => abPerson -> vCardData -> IO RawId
initWithVCardRepresentation abPerson  vCardData =
withObjCPtr vCardData $ \raw_vCardData ->
    fmap (RawId . castPtr) $ sendMsg abPerson (mkSelector "initWithVCardRepresentation:") (retPtr retVoid) [argPtr (castPtr raw_vCardData :: Ptr ())]

-- | @- vCardRepresentation@
vCardRepresentation :: IsABPerson abPerson => abPerson -> IO (Id NSData)
vCardRepresentation abPerson  =
  sendMsg abPerson (mkSelector "vCardRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ searchElementForProperty:label:key:value:comparison:@
searchElementForProperty_label_key_value_comparison :: (IsNSString property, IsNSString label, IsNSString key) => property -> label -> key -> RawId -> CLong -> IO (Id ABSearchElement)
searchElementForProperty_label_key_value_comparison property label key value comparison =
  do
    cls' <- getRequiredClass "ABPerson"
    withObjCPtr property $ \raw_property ->
      withObjCPtr label $ \raw_label ->
        withObjCPtr key $ \raw_key ->
          sendClassMsg cls' (mkSelector "searchElementForProperty:label:key:value:comparison:") (retPtr retVoid) [argPtr (castPtr raw_property :: Ptr ()), argPtr (castPtr raw_label :: Ptr ()), argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr (unRawId value) :: Ptr ()), argCLong (fromIntegral comparison)] >>= retainedObject . castPtr

-- | @+ addPropertiesAndTypes:@
addPropertiesAndTypes :: IsNSDictionary properties => properties -> IO CLong
addPropertiesAndTypes properties =
  do
    cls' <- getRequiredClass "ABPerson"
    withObjCPtr properties $ \raw_properties ->
      sendClassMsg cls' (mkSelector "addPropertiesAndTypes:") retCLong [argPtr (castPtr raw_properties :: Ptr ())]

-- | @+ removeProperties:@
removeProperties :: IsNSArray properties => properties -> IO CLong
removeProperties properties =
  do
    cls' <- getRequiredClass "ABPerson"
    withObjCPtr properties $ \raw_properties ->
      sendClassMsg cls' (mkSelector "removeProperties:") retCLong [argPtr (castPtr raw_properties :: Ptr ())]

-- | @+ properties@
properties :: IO (Id NSArray)
properties  =
  do
    cls' <- getRequiredClass "ABPerson"
    sendClassMsg cls' (mkSelector "properties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ typeOfProperty:@
typeOfProperty :: IsNSString property => property -> IO CLong
typeOfProperty property =
  do
    cls' <- getRequiredClass "ABPerson"
    withObjCPtr property $ \raw_property ->
      sendClassMsg cls' (mkSelector "typeOfProperty:") retCLong [argPtr (castPtr raw_property :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @parentGroups@
parentGroupsSelector :: Selector
parentGroupsSelector = mkSelector "parentGroups"

-- | @Selector@ for @linkedPeople@
linkedPeopleSelector :: Selector
linkedPeopleSelector = mkSelector "linkedPeople"

-- | @Selector@ for @setImageData:@
setImageDataSelector :: Selector
setImageDataSelector = mkSelector "setImageData:"

-- | @Selector@ for @imageData@
imageDataSelector :: Selector
imageDataSelector = mkSelector "imageData"

-- | @Selector@ for @beginLoadingImageDataForClient:@
beginLoadingImageDataForClientSelector :: Selector
beginLoadingImageDataForClientSelector = mkSelector "beginLoadingImageDataForClient:"

-- | @Selector@ for @cancelLoadingImageDataForTag:@
cancelLoadingImageDataForTagSelector :: Selector
cancelLoadingImageDataForTagSelector = mkSelector "cancelLoadingImageDataForTag:"

-- | @Selector@ for @initWithVCardRepresentation:@
initWithVCardRepresentationSelector :: Selector
initWithVCardRepresentationSelector = mkSelector "initWithVCardRepresentation:"

-- | @Selector@ for @vCardRepresentation@
vCardRepresentationSelector :: Selector
vCardRepresentationSelector = mkSelector "vCardRepresentation"

-- | @Selector@ for @searchElementForProperty:label:key:value:comparison:@
searchElementForProperty_label_key_value_comparisonSelector :: Selector
searchElementForProperty_label_key_value_comparisonSelector = mkSelector "searchElementForProperty:label:key:value:comparison:"

-- | @Selector@ for @addPropertiesAndTypes:@
addPropertiesAndTypesSelector :: Selector
addPropertiesAndTypesSelector = mkSelector "addPropertiesAndTypes:"

-- | @Selector@ for @removeProperties:@
removePropertiesSelector :: Selector
removePropertiesSelector = mkSelector "removeProperties:"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @typeOfProperty:@
typeOfPropertySelector :: Selector
typeOfPropertySelector = mkSelector "typeOfProperty:"

