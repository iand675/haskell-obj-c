{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MERAWProcessingFloatParameter@.
module ObjC.MediaExtension.MERAWProcessingFloatParameter
  ( MERAWProcessingFloatParameter
  , IsMERAWProcessingFloatParameter(..)
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
initWithName_key_description_initialValue_maximum_minimum :: (IsMERAWProcessingFloatParameter merawProcessingFloatParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingFloatParameter -> name -> key -> description -> CFloat -> CFloat -> CFloat -> IO (Id MERAWProcessingFloatParameter)
initWithName_key_description_initialValue_maximum_minimum merawProcessingFloatParameter name key description initialValue maximum_ minimum_ =
  sendOwnedMessage merawProcessingFloatParameter initWithName_key_description_initialValue_maximum_minimumSelector (toNSString name) (toNSString key) (toNSString description) initialValue maximum_ minimum_

-- | @- initWithName:key:description:initialValue:maximum:minimum:neutralValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValue :: (IsMERAWProcessingFloatParameter merawProcessingFloatParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingFloatParameter -> name -> key -> description -> CFloat -> CFloat -> CFloat -> CFloat -> IO (Id MERAWProcessingFloatParameter)
initWithName_key_description_initialValue_maximum_minimum_neutralValue merawProcessingFloatParameter name key description initialValue maximum_ minimum_ neutralValue =
  sendOwnedMessage merawProcessingFloatParameter initWithName_key_description_initialValue_maximum_minimum_neutralValueSelector (toNSString name) (toNSString key) (toNSString description) initialValue maximum_ minimum_ neutralValue

-- | @- initWithName:key:description:initialValue:maximum:minimum:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_cameraValue :: (IsMERAWProcessingFloatParameter merawProcessingFloatParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingFloatParameter -> name -> key -> description -> CFloat -> CFloat -> CFloat -> CFloat -> IO (Id MERAWProcessingFloatParameter)
initWithName_key_description_initialValue_maximum_minimum_cameraValue merawProcessingFloatParameter name key description initialValue maximum_ minimum_ cameraValue =
  sendOwnedMessage merawProcessingFloatParameter initWithName_key_description_initialValue_maximum_minimum_cameraValueSelector (toNSString name) (toNSString key) (toNSString description) initialValue maximum_ minimum_ cameraValue

-- | @- initWithName:key:description:initialValue:maximum:minimum:neutralValue:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValue :: (IsMERAWProcessingFloatParameter merawProcessingFloatParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingFloatParameter -> name -> key -> description -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO (Id MERAWProcessingFloatParameter)
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValue merawProcessingFloatParameter name key description initialValue maximum_ minimum_ neutralValue cameraValue =
  sendOwnedMessage merawProcessingFloatParameter initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValueSelector (toNSString name) (toNSString key) (toNSString description) initialValue maximum_ minimum_ neutralValue cameraValue

-- | hasNeutralValue
--
-- Return value indicates whether the MERAWProcessingFloatParameter has an optional declared Neutral value.
--
-- If the return value is YES and outNeutralValue is not nil, the value held by outNeutralValue will be set to the neutral value.				If the return value is NO and outNeutralValue is not nil, the value held by outNeutralValue will be set to 0.
--
-- ObjC selector: @- hasNeutralValue:@
hasNeutralValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> Ptr CFloat -> IO Bool
hasNeutralValue merawProcessingFloatParameter outNeutralValue =
  sendMessage merawProcessingFloatParameter hasNeutralValueSelector outNeutralValue

-- | hasCameraValue
--
-- Return value indicates whether the MERAWProcessingFloatParameter has an optional declared Camera value.
--
-- If the return value is YES and outCameraValue is not nil, the value held by outCameraValue will be set to the camera value.				If the return value is NO and outCameraValue is not nil, the value held by outCameraValue will be set to 0.
--
-- ObjC selector: @- hasCameraValue:@
hasCameraValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> Ptr CFloat -> IO Bool
hasCameraValue merawProcessingFloatParameter outCameraValue =
  sendMessage merawProcessingFloatParameter hasCameraValueSelector outCameraValue

-- | maximumValue
--
-- The maximum value for this parameter.
--
-- ObjC selector: @- maximumValue@
maximumValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> IO CFloat
maximumValue merawProcessingFloatParameter =
  sendMessage merawProcessingFloatParameter maximumValueSelector

-- | minimumValue
--
-- The minimum value for this parameter.
--
-- ObjC selector: @- minimumValue@
minimumValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> IO CFloat
minimumValue merawProcessingFloatParameter =
  sendMessage merawProcessingFloatParameter minimumValueSelector

-- | initialValue
--
-- The initial value for this parameter as defined in the sequence metadata.
--
-- ObjC selector: @- initialValue@
initialValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> IO CFloat
initialValue merawProcessingFloatParameter =
  sendOwnedMessage merawProcessingFloatParameter initialValueSelector

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- currentValue@
currentValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> IO CFloat
currentValue merawProcessingFloatParameter =
  sendMessage merawProcessingFloatParameter currentValueSelector

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- setCurrentValue:@
setCurrentValue :: IsMERAWProcessingFloatParameter merawProcessingFloatParameter => merawProcessingFloatParameter -> CFloat -> IO ()
setCurrentValue merawProcessingFloatParameter value =
  sendMessage merawProcessingFloatParameter setCurrentValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:key:description:initialValue:maximum:minimum:@
initWithName_key_description_initialValue_maximum_minimumSelector :: Selector '[Id NSString, Id NSString, Id NSString, CFloat, CFloat, CFloat] (Id MERAWProcessingFloatParameter)
initWithName_key_description_initialValue_maximum_minimumSelector = mkSelector "initWithName:key:description:initialValue:maximum:minimum:"

-- | @Selector@ for @initWithName:key:description:initialValue:maximum:minimum:neutralValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, CFloat, CFloat, CFloat, CFloat] (Id MERAWProcessingFloatParameter)
initWithName_key_description_initialValue_maximum_minimum_neutralValueSelector = mkSelector "initWithName:key:description:initialValue:maximum:minimum:neutralValue:"

-- | @Selector@ for @initWithName:key:description:initialValue:maximum:minimum:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_cameraValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, CFloat, CFloat, CFloat, CFloat] (Id MERAWProcessingFloatParameter)
initWithName_key_description_initialValue_maximum_minimum_cameraValueSelector = mkSelector "initWithName:key:description:initialValue:maximum:minimum:cameraValue:"

-- | @Selector@ for @initWithName:key:description:initialValue:maximum:minimum:neutralValue:cameraValue:@
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, CFloat, CFloat, CFloat, CFloat, CFloat] (Id MERAWProcessingFloatParameter)
initWithName_key_description_initialValue_maximum_minimum_neutralValue_cameraValueSelector = mkSelector "initWithName:key:description:initialValue:maximum:minimum:neutralValue:cameraValue:"

-- | @Selector@ for @hasNeutralValue:@
hasNeutralValueSelector :: Selector '[Ptr CFloat] Bool
hasNeutralValueSelector = mkSelector "hasNeutralValue:"

-- | @Selector@ for @hasCameraValue:@
hasCameraValueSelector :: Selector '[Ptr CFloat] Bool
hasCameraValueSelector = mkSelector "hasCameraValue:"

-- | @Selector@ for @maximumValue@
maximumValueSelector :: Selector '[] CFloat
maximumValueSelector = mkSelector "maximumValue"

-- | @Selector@ for @minimumValue@
minimumValueSelector :: Selector '[] CFloat
minimumValueSelector = mkSelector "minimumValue"

-- | @Selector@ for @initialValue@
initialValueSelector :: Selector '[] CFloat
initialValueSelector = mkSelector "initialValue"

-- | @Selector@ for @currentValue@
currentValueSelector :: Selector '[] CFloat
currentValueSelector = mkSelector "currentValue"

-- | @Selector@ for @setCurrentValue:@
setCurrentValueSelector :: Selector '[CFloat] ()
setCurrentValueSelector = mkSelector "setCurrentValue:"

