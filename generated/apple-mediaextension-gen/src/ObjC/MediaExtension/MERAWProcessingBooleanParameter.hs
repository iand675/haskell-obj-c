{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MERAWProcessingBooleanParameter@.
module ObjC.MediaExtension.MERAWProcessingBooleanParameter
  ( MERAWProcessingBooleanParameter
  , IsMERAWProcessingBooleanParameter(..)
  , initWithName_key_description_initialValue
  , initWithName_key_description_initialValue_neutralValue
  , initWithName_key_description_initialValue_cameraValue
  , initWithName_key_description_initialValue_neutralValue_cameraValue
  , hasNeutralValue
  , hasCameraValue
  , initialValue
  , currentValue
  , setCurrentValue
  , currentValueSelector
  , hasCameraValueSelector
  , hasNeutralValueSelector
  , initWithName_key_description_initialValueSelector
  , initWithName_key_description_initialValue_cameraValueSelector
  , initWithName_key_description_initialValue_neutralValueSelector
  , initWithName_key_description_initialValue_neutralValue_cameraValueSelector
  , initialValueSelector
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

-- | @- initWithName:key:description:initialValue:@
initWithName_key_description_initialValue :: (IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingBooleanParameter -> name -> key -> description -> Bool -> IO (Id MERAWProcessingBooleanParameter)
initWithName_key_description_initialValue merawProcessingBooleanParameter name key description initialValue =
  sendOwnedMessage merawProcessingBooleanParameter initWithName_key_description_initialValueSelector (toNSString name) (toNSString key) (toNSString description) initialValue

-- | @- initWithName:key:description:initialValue:neutralValue:@
initWithName_key_description_initialValue_neutralValue :: (IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingBooleanParameter -> name -> key -> description -> Bool -> Bool -> IO (Id MERAWProcessingBooleanParameter)
initWithName_key_description_initialValue_neutralValue merawProcessingBooleanParameter name key description initialValue neutralValue =
  sendOwnedMessage merawProcessingBooleanParameter initWithName_key_description_initialValue_neutralValueSelector (toNSString name) (toNSString key) (toNSString description) initialValue neutralValue

-- | @- initWithName:key:description:initialValue:cameraValue:@
initWithName_key_description_initialValue_cameraValue :: (IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingBooleanParameter -> name -> key -> description -> Bool -> Bool -> IO (Id MERAWProcessingBooleanParameter)
initWithName_key_description_initialValue_cameraValue merawProcessingBooleanParameter name key description initialValue cameraValue =
  sendOwnedMessage merawProcessingBooleanParameter initWithName_key_description_initialValue_cameraValueSelector (toNSString name) (toNSString key) (toNSString description) initialValue cameraValue

-- | @- initWithName:key:description:initialValue:neutralValue:cameraValue:@
initWithName_key_description_initialValue_neutralValue_cameraValue :: (IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter, IsNSString name, IsNSString key, IsNSString description) => merawProcessingBooleanParameter -> name -> key -> description -> Bool -> Bool -> Bool -> IO (Id MERAWProcessingBooleanParameter)
initWithName_key_description_initialValue_neutralValue_cameraValue merawProcessingBooleanParameter name key description initialValue neutralValue cameraValue =
  sendOwnedMessage merawProcessingBooleanParameter initWithName_key_description_initialValue_neutralValue_cameraValueSelector (toNSString name) (toNSString key) (toNSString description) initialValue neutralValue cameraValue

-- | hasNeutralValue
--
-- Return value indicates whether the MERAWProcessingBooleanParameter has an optional declared Neutral value.
--
-- If the return value is YES and outNeutralValue is not nil, the value held by outNeutralValue will be set to the neutral value.				If the return value is NO and outNeutralValue is not nil, the value held by outNeutralValue will be set to NO.
--
-- ObjC selector: @- hasNeutralValue:@
hasNeutralValue :: IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter => merawProcessingBooleanParameter -> Ptr Bool -> IO Bool
hasNeutralValue merawProcessingBooleanParameter outNeutralValue =
  sendMessage merawProcessingBooleanParameter hasNeutralValueSelector outNeutralValue

-- | hasCameraValue
--
-- Return value indicates whether the MERAWProcessingBooleanParameter has an optional declared Camera value.
--
-- If the return value is YES and outCameraValue is not nil, the value held by outCameraValue will be set to the camera value.				If the return value is NO and outCameraValue is not nil, the value held by outCameraValue will be set to NO.
--
-- ObjC selector: @- hasCameraValue:@
hasCameraValue :: IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter => merawProcessingBooleanParameter -> Ptr Bool -> IO Bool
hasCameraValue merawProcessingBooleanParameter outCameraValue =
  sendMessage merawProcessingBooleanParameter hasCameraValueSelector outCameraValue

-- | initialValue
--
-- The initial value for this parameter as defined in the sequence metadata.
--
-- ObjC selector: @- initialValue@
initialValue :: IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter => merawProcessingBooleanParameter -> IO Bool
initialValue merawProcessingBooleanParameter =
  sendOwnedMessage merawProcessingBooleanParameter initialValueSelector

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- currentValue@
currentValue :: IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter => merawProcessingBooleanParameter -> IO Bool
currentValue merawProcessingBooleanParameter =
  sendMessage merawProcessingBooleanParameter currentValueSelector

-- | currentValue
--
-- Get or set the current value for this parameter.
--
-- This property can be observed if appropriate in order to react to changes which would result in changes to the set of MERAWProcessingParameters vended by the extension.
--
-- ObjC selector: @- setCurrentValue:@
setCurrentValue :: IsMERAWProcessingBooleanParameter merawProcessingBooleanParameter => merawProcessingBooleanParameter -> Bool -> IO ()
setCurrentValue merawProcessingBooleanParameter value =
  sendMessage merawProcessingBooleanParameter setCurrentValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:key:description:initialValue:@
initWithName_key_description_initialValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, Bool] (Id MERAWProcessingBooleanParameter)
initWithName_key_description_initialValueSelector = mkSelector "initWithName:key:description:initialValue:"

-- | @Selector@ for @initWithName:key:description:initialValue:neutralValue:@
initWithName_key_description_initialValue_neutralValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, Bool, Bool] (Id MERAWProcessingBooleanParameter)
initWithName_key_description_initialValue_neutralValueSelector = mkSelector "initWithName:key:description:initialValue:neutralValue:"

-- | @Selector@ for @initWithName:key:description:initialValue:cameraValue:@
initWithName_key_description_initialValue_cameraValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, Bool, Bool] (Id MERAWProcessingBooleanParameter)
initWithName_key_description_initialValue_cameraValueSelector = mkSelector "initWithName:key:description:initialValue:cameraValue:"

-- | @Selector@ for @initWithName:key:description:initialValue:neutralValue:cameraValue:@
initWithName_key_description_initialValue_neutralValue_cameraValueSelector :: Selector '[Id NSString, Id NSString, Id NSString, Bool, Bool, Bool] (Id MERAWProcessingBooleanParameter)
initWithName_key_description_initialValue_neutralValue_cameraValueSelector = mkSelector "initWithName:key:description:initialValue:neutralValue:cameraValue:"

-- | @Selector@ for @hasNeutralValue:@
hasNeutralValueSelector :: Selector '[Ptr Bool] Bool
hasNeutralValueSelector = mkSelector "hasNeutralValue:"

-- | @Selector@ for @hasCameraValue:@
hasCameraValueSelector :: Selector '[Ptr Bool] Bool
hasCameraValueSelector = mkSelector "hasCameraValue:"

-- | @Selector@ for @initialValue@
initialValueSelector :: Selector '[] Bool
initialValueSelector = mkSelector "initialValue"

-- | @Selector@ for @currentValue@
currentValueSelector :: Selector '[] Bool
currentValueSelector = mkSelector "currentValue"

-- | @Selector@ for @setCurrentValue:@
setCurrentValueSelector :: Selector '[Bool] ()
setCurrentValueSelector = mkSelector "setCurrentValue:"

