{-# LANGUAGE DataKinds #-}
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
  , addPropertiesAndTypesSelector
  , beginLoadingImageDataForClientSelector
  , cancelLoadingImageDataForTagSelector
  , imageDataSelector
  , initWithVCardRepresentationSelector
  , linkedPeopleSelector
  , parentGroupsSelector
  , propertiesSelector
  , removePropertiesSelector
  , searchElementForProperty_label_key_value_comparisonSelector
  , setImageDataSelector
  , typeOfPropertySelector
  , vCardRepresentationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AddressBook.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- parentGroups@
parentGroups :: IsABPerson abPerson => abPerson -> IO (Id NSArray)
parentGroups abPerson =
  sendMessage abPerson parentGroupsSelector

-- | @- linkedPeople@
linkedPeople :: IsABPerson abPerson => abPerson -> IO (Id NSArray)
linkedPeople abPerson =
  sendMessage abPerson linkedPeopleSelector

-- | @- setImageData:@
setImageData :: (IsABPerson abPerson, IsNSData data_) => abPerson -> data_ -> IO Bool
setImageData abPerson data_ =
  sendMessage abPerson setImageDataSelector (toNSData data_)

-- | @- imageData@
imageData :: IsABPerson abPerson => abPerson -> IO (Id NSData)
imageData abPerson =
  sendMessage abPerson imageDataSelector

-- | @- beginLoadingImageDataForClient:@
beginLoadingImageDataForClient :: IsABPerson abPerson => abPerson -> RawId -> IO CLong
beginLoadingImageDataForClient abPerson client =
  sendMessage abPerson beginLoadingImageDataForClientSelector client

-- | @+ cancelLoadingImageDataForTag:@
cancelLoadingImageDataForTag :: CLong -> IO ()
cancelLoadingImageDataForTag tag =
  do
    cls' <- getRequiredClass "ABPerson"
    sendClassMessage cls' cancelLoadingImageDataForTagSelector tag

-- | @- initWithVCardRepresentation:@
initWithVCardRepresentation :: (IsABPerson abPerson, IsNSData vCardData) => abPerson -> vCardData -> IO RawId
initWithVCardRepresentation abPerson vCardData =
  sendOwnedMessage abPerson initWithVCardRepresentationSelector (toNSData vCardData)

-- | @- vCardRepresentation@
vCardRepresentation :: IsABPerson abPerson => abPerson -> IO (Id NSData)
vCardRepresentation abPerson =
  sendMessage abPerson vCardRepresentationSelector

-- | @+ searchElementForProperty:label:key:value:comparison:@
searchElementForProperty_label_key_value_comparison :: (IsNSString property, IsNSString label, IsNSString key) => property -> label -> key -> RawId -> CLong -> IO (Id ABSearchElement)
searchElementForProperty_label_key_value_comparison property label key value comparison =
  do
    cls' <- getRequiredClass "ABPerson"
    sendClassMessage cls' searchElementForProperty_label_key_value_comparisonSelector (toNSString property) (toNSString label) (toNSString key) value comparison

-- | @+ addPropertiesAndTypes:@
addPropertiesAndTypes :: IsNSDictionary properties => properties -> IO CLong
addPropertiesAndTypes properties =
  do
    cls' <- getRequiredClass "ABPerson"
    sendClassMessage cls' addPropertiesAndTypesSelector (toNSDictionary properties)

-- | @+ removeProperties:@
removeProperties :: IsNSArray properties => properties -> IO CLong
removeProperties properties =
  do
    cls' <- getRequiredClass "ABPerson"
    sendClassMessage cls' removePropertiesSelector (toNSArray properties)

-- | @+ properties@
properties :: IO (Id NSArray)
properties  =
  do
    cls' <- getRequiredClass "ABPerson"
    sendClassMessage cls' propertiesSelector

-- | @+ typeOfProperty:@
typeOfProperty :: IsNSString property => property -> IO CLong
typeOfProperty property =
  do
    cls' <- getRequiredClass "ABPerson"
    sendClassMessage cls' typeOfPropertySelector (toNSString property)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @parentGroups@
parentGroupsSelector :: Selector '[] (Id NSArray)
parentGroupsSelector = mkSelector "parentGroups"

-- | @Selector@ for @linkedPeople@
linkedPeopleSelector :: Selector '[] (Id NSArray)
linkedPeopleSelector = mkSelector "linkedPeople"

-- | @Selector@ for @setImageData:@
setImageDataSelector :: Selector '[Id NSData] Bool
setImageDataSelector = mkSelector "setImageData:"

-- | @Selector@ for @imageData@
imageDataSelector :: Selector '[] (Id NSData)
imageDataSelector = mkSelector "imageData"

-- | @Selector@ for @beginLoadingImageDataForClient:@
beginLoadingImageDataForClientSelector :: Selector '[RawId] CLong
beginLoadingImageDataForClientSelector = mkSelector "beginLoadingImageDataForClient:"

-- | @Selector@ for @cancelLoadingImageDataForTag:@
cancelLoadingImageDataForTagSelector :: Selector '[CLong] ()
cancelLoadingImageDataForTagSelector = mkSelector "cancelLoadingImageDataForTag:"

-- | @Selector@ for @initWithVCardRepresentation:@
initWithVCardRepresentationSelector :: Selector '[Id NSData] RawId
initWithVCardRepresentationSelector = mkSelector "initWithVCardRepresentation:"

-- | @Selector@ for @vCardRepresentation@
vCardRepresentationSelector :: Selector '[] (Id NSData)
vCardRepresentationSelector = mkSelector "vCardRepresentation"

-- | @Selector@ for @searchElementForProperty:label:key:value:comparison:@
searchElementForProperty_label_key_value_comparisonSelector :: Selector '[Id NSString, Id NSString, Id NSString, RawId, CLong] (Id ABSearchElement)
searchElementForProperty_label_key_value_comparisonSelector = mkSelector "searchElementForProperty:label:key:value:comparison:"

-- | @Selector@ for @addPropertiesAndTypes:@
addPropertiesAndTypesSelector :: Selector '[Id NSDictionary] CLong
addPropertiesAndTypesSelector = mkSelector "addPropertiesAndTypes:"

-- | @Selector@ for @removeProperties:@
removePropertiesSelector :: Selector '[Id NSArray] CLong
removePropertiesSelector = mkSelector "removeProperties:"

-- | @Selector@ for @properties@
propertiesSelector :: Selector '[] (Id NSArray)
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @typeOfProperty:@
typeOfPropertySelector :: Selector '[Id NSString] CLong
typeOfPropertySelector = mkSelector "typeOfProperty:"

