{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MERAWProcessingIntegerParameter@.
module ObjC.MediaExtension.MERAWProcessingIntegerParameter
  ( MERAWProcessingIntegerParameter
  , IsMERAWProcessingIntegerParameter(..)
  , initWithName_key_description_initialValue_maximum_minimum
  , initWithName_key_description_initialValue_maximum_minimum_neutralValue
  , initWithName_key_description_initialValue_maximum_minimum_cameraValue
  , initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValue
  , hasNeutralValue
  , hasCameraValue
  , maximumValue
  , minimumValue
  , initialValue
  , currentValue
  , setCurrentValue
  , currentValueSelector
  , hasCameraValueSelector
  , hasNeutralValueSelector
  , initWithName_key_description_initialValue_maximum_minimumSelector
  , initWithName_key_description_initialValue_maximum_minimum_cameraValueSelector
  , initWithName_key_description_initialValue_maximum_minimum_neutralValueSelector
  , initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValueSelector
  , initialValueSelector
  , maximumValueSelector
  , minimumValueSelector
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

-- | @- initWithName:key:description:initialValue:maximum:minimum:@
initWithName_key_description_initialValue_maximum_minimum :: (IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingIntegerParameter -> name -> key -> description -> CLong -> CLong -> CLong -> IO (Id MERAWProcessingIntegerParameter)
initWithName_key_description_initialValue_maximum_minimum merawProcessingIntegerParameter name key description initialValue maximum_ minimum_ =
  sendOwnedMessage merawProcessingIntegerParameter initWithName_key_description_initialValue_maximum_minimumSelector (toNSString name) (toNSString key) (toNSString description) initialValue maximum_ minimum_

-- | @- initWithName:key:description:initialValue:maximum:minimum:neutralValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValue :: (IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingIntegerParameter -> name -> key -> description -> CLong -> CLong -> CLong -> CLong -> IO (Id MERAWProcessingIntegerParameter)
initWithName_key_description_initialValue_maximum_minimum_neutralValue merawProcessingIntegerParameter name key description initialValue maximum_ minimum_ neutralValue =
  sendOwnedMessage merawProcessingIntegerParameter initWithName_key_description_initialValue_maximum_minimum_neutralValueSelector (toNSString name) (toNSString key) (toNSString description) initialValue maximum_ minimum_ neutralValue

-- | @- initWithName:key:description:initialValue:maximum:minimum:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_cameraValue :: (IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingIntegerParameter -> name -> key -> description -> CLong -> CLong -> CLong -> CLong -> IO (Id MERAWProcessingIntegerParameter)
initWithName_key_description_initialValue_maximum_minimum_cameraValue merawProcessingIntegerParameter name key description initialValue maximum_ minimum_ cameraValue =
  sendOwnedMessage merawProcessingIntegerParameter initWithName_key_description_initialValue_maximum_minimum_cameraValueSelector (toNSString name) (toNSString key) (toNSString description) initialValue maximum_ minimum_ cameraValue

-- | @- initWithName:key:description:initialValue:maximum:minimum:neutralValue:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValue :: (IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingIntegerParameter -> name -> key -> description -> CLong -> CLong -> CLong -> CLong -> CLong -> IO (Id MERAWProcessingIntegerParameter)
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValue merawProcessingIntegerParameter name key description initialValue maximum_ minimum_ neutralValue cameraValue =
  sendOwnedMessage merawProcessingIntegerParameter initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValueSelector (toNSString name) (toNSString key) (toNSString description) initialValue maximum_ minimum_ neutralValue cameraValue

-- | hasNeutralValue
--
-- Return value indicates whether the MERAWProcessingIntegerParameter has an optional declared Neutral value.
--
-- If the return value is YES and outNeutralValue is not nil, the value held by outNeutralValue will be set to the neutral value.				If the return value is NO and outNeutralValue is not nil, the value held by outNeutralValue will be set to 0.
--
-- ObjC selector: @- hasNeutralValue:@
hasNeutralValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> Ptr CLong -> IO Bool
hasNeutralValue merawProcessingIntegerParameter outNeutralValue =
  sendMessage merawProcessingIntegerParameter hasNeutralValueSelector outNeutralValue

-- | hasCameraValue
--
-- Return value indicates whether the MERAWProcessingIntegerParameter has an optional declared Camera value.				If the return value is YES and outCameraValue is not nil, the value held by outCameraValue will be set to the camera value.				If the return value is NO and outCameraValue is not nil, the value held by outCameraValue will be set to 0.
--
-- ObjC selector: @- hasCameraValue:@
hasCameraValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> Ptr CLong -> IO Bool
hasCameraValue merawProcessingIntegerParameter outCameraValue =
  sendMessage merawProcessingIntegerParameter hasCameraValueSelector outCameraValue

-- | maximumValue
--
-- The maximum value for this parameter.
--
-- ObjC selector: @- maximumValue@
maximumValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> IO CLong
maximumValue merawProcessingIntegerParameter =
  sendMessage merawProcessingIntegerParameter maximumValueSelector

-- | minimumValue
--
-- The minimum value for this parameter.
--
-- ObjC selector: @- minimumValue@
minimumValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> IO CLong
minimumValue merawProcessingIntegerParameter =
  sendMessage merawProcessingIntegerParameter minimumValueSelector

-- | initialValue
--
-- The initial value for this parameter as defined in the sequence metadata.
--
-- ObjC selector: @- initialValue@
initialValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> IO CLong
initialValue merawProcessingIntegerParameter =
  sendOwnedMessage merawProcessingIntegerParameter initialValueSelector

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- currentValue@
currentValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> IO CLong
currentValue merawProcessingIntegerParameter =
  sendMessage merawProcessingIntegerParameter currentValueSelector

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- setCurrentValue:@
setCurrentValue :: IsMERAWProcessingIntegerParameter merawProcessingIntegerParameter => merawProcessingIntegerParameter -> CLong -> IO ()
setCurrentValue merawProcessingIntegerParameter value =
  sendMessage merawProcessingIntegerParameter setCurrentValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:key:description:initialValue:maximum:minimum:@
initWithName_key_description_initialValue_maximum_minimumSelector :: Selector '[Id NSString, Id NSString, Id NSString, CLong, CLong, CLong] (Id MERAWProcessingIntegerParameter)
initWithName_key_description_initialValue_maximum_minimumSelector = mkSelector "initWithName:key:description:initialValue:maximum:minimum:"

-- | @Selector@ for @initWithName:key:description:initialValue:maximum:minimum:neutralValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, CLong, CLong, CLong, CLong] (Id MERAWProcessingIntegerParameter)
initWithName_key_description_initialValue_maximum_minimum_neutralValueSelector = mkSelector "initWithName:key:description:initialValue:maximum:minimum:neutralValue:"

-- | @Selector@ for @initWithName:key:description:initialValue:maximum:minimum:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_cameraValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, CLong, CLong, CLong, CLong] (Id MERAWProcessingIntegerParameter)
initWithName_key_description_initialValue_maximum_minimum_cameraValueSelector = mkSelector "initWithName:key:description:initialValue:maximum:minimum:cameraValue:"

-- | @Selector@ for @initWithName:key:description:initialValue:maximum:minimum:neutralValue:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, CLong, CLong, CLong, CLong, CLong] (Id MERAWProcessingIntegerParameter)
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValueSelector = mkSelector "initWithName:key:description:initialValue:maximum:minimum:neutralValue:cameraValue:"

-- | @Selector@ for @hasNeutralValue:@
hasNeutralValueSelector :: Selector '[Ptr CLong] Bool
hasNeutralValueSelector = mkSelector "hasNeutralValue:"

-- | @Selector@ for @hasCameraValue:@
hasCameraValueSelector :: Selector '[Ptr CLong] Bool
hasCameraValueSelector = mkSelector "hasCameraValue:"

-- | @Selector@ for @maximumValue@
maximumValueSelector :: Selector '[] CLong
maximumValueSelector = mkSelector "maximumValue"

-- | @Selector@ for @minimumValue@
minimumValueSelector :: Selector '[] CLong
minimumValueSelector = mkSelector "minimumValue"

-- | @Selector@ for @initialValue@
initialValueSelector :: Selector '[] CLong
initialValueSelector = mkSelector "initialValue"

-- | @Selector@ for @currentValue@
currentValueSelector :: Selector '[] CLong
currentValueSelector = mkSelector "currentValue"

-- | @Selector@ for @setCurrentValue:@
setCurrentValueSelector :: Selector '[CLong] ()
setCurrentValueSelector = mkSelector "setCurrentValue:"

