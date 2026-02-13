{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MERAWProcessingListParameter@.
module ObjC.MediaExtension.MERAWProcessingListParameter
  ( MERAWProcessingListParameter
  , IsMERAWProcessingListParameter(..)
  , initWithName_key_description_list_initialValue
  , initWithName_key_description_list_initialValue_neutralValue
  , initWithName_key_description_list_initialValue_cameraValue
  , initWithName_key_description_list_initialValue_neutralValue_cameraValue
  , hasNeutralValue
  , hasCameraValue
  , listElements
  , initialValue
  , currentValue
  , setCurrentValue
  , currentValueSelector
  , hasCameraValueSelector
  , hasNeutralValueSelector
  , initWithName_key_description_list_initialValueSelector
  , initWithName_key_description_list_initialValue_cameraValueSelector
  , initWithName_key_description_list_initialValue_neutralValueSelector
  , initWithName_key_description_list_initialValue_neutralValue_cameraValueSelector
  , initialValueSelector
  , listElementsSelector
  , setCurrentValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:key:description:list:initialValue:@
initWithName_key_description_list_initialValue :: (IsMERAWProcessingListParameter merawProcessingListParameter, IsNSString name, IsNSString key, IsNSString description, IsNSArray listElements) => merawProcessingListParameter -> name -> key -> description -> listElements -> CLong -> IO (Id MERAWProcessingListParameter)
initWithName_key_description_list_initialValue merawProcessingListParameter name key description listElements initialValue =
  sendOwnedMessage merawProcessingListParameter initWithName_key_description_list_initialValueSelector (toNSString name) (toNSString key) (toNSString description) (toNSArray listElements) initialValue

-- | @- initWithName:key:description:list:initialValue:neutralValue:@
initWithName_key_description_list_initialValue_neutralValue :: (IsMERAWProcessingListParameter merawProcessingListParameter, IsNSString name, IsNSString key, IsNSString description, IsNSArray listElements) => merawProcessingListParameter -> name -> key -> description -> listElements -> CLong -> CLong -> IO (Id MERAWProcessingListParameter)
initWithName_key_description_list_initialValue_neutralValue merawProcessingListParameter name key description listElements initialValue neutralValue =
  sendOwnedMessage merawProcessingListParameter initWithName_key_description_list_initialValue_neutralValueSelector (toNSString name) (toNSString key) (toNSString description) (toNSArray listElements) initialValue neutralValue

-- | @- initWithName:key:description:list:initialValue:cameraValue:@
initWithName_key_description_list_initialValue_cameraValue :: (IsMERAWProcessingListParameter merawProcessingListParameter, IsNSString name, IsNSString key, IsNSString description, IsNSArray listElements) => merawProcessingListParameter -> name -> key -> description -> listElements -> CLong -> CLong -> IO (Id MERAWProcessingListParameter)
initWithName_key_description_list_initialValue_cameraValue merawProcessingListParameter name key description listElements initialValue cameraValue =
  sendOwnedMessage merawProcessingListParameter initWithName_key_description_list_initialValue_cameraValueSelector (toNSString name) (toNSString key) (toNSString description) (toNSArray listElements) initialValue cameraValue

-- | @- initWithName:key:description:list:initialValue:neutralValue:cameraValue:@
initWithName_key_description_list_initialValue_neutralValue_cameraValue :: (IsMERAWProcessingListParameter merawProcessingListParameter, IsNSString name, IsNSString key, IsNSString description, IsNSArray listElements) => merawProcessingListParameter -> name -> key -> description -> listElements -> CLong -> CLong -> CLong -> IO (Id MERAWProcessingListParameter)
initWithName_key_description_list_initialValue_neutralValue_cameraValue merawProcessingListParameter name key description listElements initialValue neutralValue cameraValue =
  sendOwnedMessage merawProcessingListParameter initWithName_key_description_list_initialValue_neutralValue_cameraValueSelector (toNSString name) (toNSString key) (toNSString description) (toNSArray listElements) initialValue neutralValue cameraValue

-- | hasNeutralValue
--
-- Return value indicates whether the MERAWProcessingListParameter has an optional declared Neutral value.
--
-- If the return value is YES and outNeutralValue is not nil, the value held by outNeutralValue will be set to the neutral value.				If the return value is NO and outNeutralValue is not nil, the value held by outNeutralValue will be set to 0.
--
-- ObjC selector: @- hasNeutralValue:@
hasNeutralValue :: IsMERAWProcessingListParameter merawProcessingListParameter => merawProcessingListParameter -> Ptr CLong -> IO Bool
hasNeutralValue merawProcessingListParameter outNeutralValue =
  sendMessage merawProcessingListParameter hasNeutralValueSelector outNeutralValue

-- | hasCameraValue
--
-- Return value indicates whether the MERAWProcessingListParameter has an optional declared Camera value.
--
-- If the return value is YES and outCameraValue is not nil, the value held by outCameraValue will be set to the camera value.				If the return value is NO and outCameraValue is not nil, the value held by outCameraValue will be set to 0.
--
-- ObjC selector: @- hasCameraValue:@
hasCameraValue :: IsMERAWProcessingListParameter merawProcessingListParameter => merawProcessingListParameter -> Ptr CLong -> IO Bool
hasCameraValue merawProcessingListParameter outCameraValue =
  sendMessage merawProcessingListParameter hasCameraValueSelector outCameraValue

-- | listElements
--
-- The ordered array of MERAWProcessingListElementParameter which make up this list.
--
-- ObjC selector: @- listElements@
listElements :: IsMERAWProcessingListParameter merawProcessingListParameter => merawProcessingListParameter -> IO (Id NSArray)
listElements merawProcessingListParameter =
  sendMessage merawProcessingListParameter listElementsSelector

-- | initialValue
--
-- The initial value for this parameter as defined in the sequence metadata.  The value is the listElementID value of the MERAWProcessingListElementParameter for initial settings.
--
-- ObjC selector: @- initialValue@
initialValue :: IsMERAWProcessingListParameter merawProcessingListParameter => merawProcessingListParameter -> IO CLong
initialValue merawProcessingListParameter =
  sendOwnedMessage merawProcessingListParameter initialValueSelector

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- The value is the listElementID value of the selected MERAWProcessingListElementParameter.   This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- currentValue@
currentValue :: IsMERAWProcessingListParameter merawProcessingListParameter => merawProcessingListParameter -> IO CLong
currentValue merawProcessingListParameter =
  sendMessage merawProcessingListParameter currentValueSelector

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- The value is the listElementID value of the selected MERAWProcessingListElementParameter.   This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- setCurrentValue:@
setCurrentValue :: IsMERAWProcessingListParameter merawProcessingListParameter => merawProcessingListParameter -> CLong -> IO ()
setCurrentValue merawProcessingListParameter value =
  sendMessage merawProcessingListParameter setCurrentValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:key:description:list:initialValue:@
initWithName_key_description_list_initialValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSArray, CLong] (Id MERAWProcessingListParameter)
initWithName_key_description_list_initialValueSelector = mkSelector "initWithName:key:description:list:initialValue:"

-- | @Selector@ for @initWithName:key:description:list:initialValue:neutralValue:@
initWithName_key_description_list_initialValue_neutralValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSArray, CLong, CLong] (Id MERAWProcessingListParameter)
initWithName_key_description_list_initialValue_neutralValueSelector = mkSelector "initWithName:key:description:list:initialValue:neutralValue:"

-- | @Selector@ for @initWithName:key:description:list:initialValue:cameraValue:@
initWithName_key_description_list_initialValue_cameraValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSArray, CLong, CLong] (Id MERAWProcessingListParameter)
initWithName_key_description_list_initialValue_cameraValueSelector = mkSelector "initWithName:key:description:list:initialValue:cameraValue:"

-- | @Selector@ for @initWithName:key:description:list:initialValue:neutralValue:cameraValue:@
initWithName_key_description_list_initialValue_neutralValue_cameraValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSArray, CLong, CLong, CLong] (Id MERAWProcessingListParameter)
initWithName_key_description_list_initialValue_neutralValue_cameraValueSelector = mkSelector "initWithName:key:description:list:initialValue:neutralValue:cameraValue:"

-- | @Selector@ for @hasNeutralValue:@
hasNeutralValueSelector :: Selector '[Ptr CLong] Bool
hasNeutralValueSelector = mkSelector "hasNeutralValue:"

-- | @Selector@ for @hasCameraValue:@
hasCameraValueSelector :: Selector '[Ptr CLong] Bool
hasCameraValueSelector = mkSelector "hasCameraValue:"

-- | @Selector@ for @listElements@
listElementsSelector :: Selector '[] (Id NSArray)
listElementsSelector = mkSelector "listElements"

-- | @Selector@ for @initialValue@
initialValueSelector :: Selector '[] CLong
initialValueSelector = mkSelector "initialValue"

-- | @Selector@ for @currentValue@
currentValueSelector :: Selector '[] CLong
currentValueSelector = mkSelector "currentValue"

-- | @Selector@ for @setCurrentValue:@
setCurrentValueSelector :: Selector '[CLong] ()
setCurrentValueSelector = mkSelector "setCurrentValue:"

