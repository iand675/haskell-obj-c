{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRentalCar@.
module ObjC.Intents.INRentalCar
  ( INRentalCar
  , IsINRentalCar(..)
  , init_
  , initWithRentalCompanyName_type_make_model_rentalCarDescription
  , rentalCompanyName
  , type_
  , make
  , model
  , rentalCarDescription
  , initSelector
  , initWithRentalCompanyName_type_make_model_rentalCarDescriptionSelector
  , makeSelector
  , modelSelector
  , rentalCarDescriptionSelector
  , rentalCompanyNameSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINRentalCar inRentalCar => inRentalCar -> IO (Id INRentalCar)
init_ inRentalCar =
  sendOwnedMessage inRentalCar initSelector

-- | @- initWithRentalCompanyName:type:make:model:rentalCarDescription:@
initWithRentalCompanyName_type_make_model_rentalCarDescription :: (IsINRentalCar inRentalCar, IsNSString rentalCompanyName, IsNSString type_, IsNSString make, IsNSString model, IsNSString rentalCarDescription) => inRentalCar -> rentalCompanyName -> type_ -> make -> model -> rentalCarDescription -> IO (Id INRentalCar)
initWithRentalCompanyName_type_make_model_rentalCarDescription inRentalCar rentalCompanyName type_ make model rentalCarDescription =
  sendOwnedMessage inRentalCar initWithRentalCompanyName_type_make_model_rentalCarDescriptionSelector (toNSString rentalCompanyName) (toNSString type_) (toNSString make) (toNSString model) (toNSString rentalCarDescription)

-- | @- rentalCompanyName@
rentalCompanyName :: IsINRentalCar inRentalCar => inRentalCar -> IO (Id NSString)
rentalCompanyName inRentalCar =
  sendMessage inRentalCar rentalCompanyNameSelector

-- | @- type@
type_ :: IsINRentalCar inRentalCar => inRentalCar -> IO (Id NSString)
type_ inRentalCar =
  sendMessage inRentalCar typeSelector

-- | @- make@
make :: IsINRentalCar inRentalCar => inRentalCar -> IO (Id NSString)
make inRentalCar =
  sendMessage inRentalCar makeSelector

-- | @- model@
model :: IsINRentalCar inRentalCar => inRentalCar -> IO (Id NSString)
model inRentalCar =
  sendMessage inRentalCar modelSelector

-- | @- rentalCarDescription@
rentalCarDescription :: IsINRentalCar inRentalCar => inRentalCar -> IO (Id NSString)
rentalCarDescription inRentalCar =
  sendMessage inRentalCar rentalCarDescriptionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INRentalCar)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRentalCompanyName:type:make:model:rentalCarDescription:@
initWithRentalCompanyName_type_make_model_rentalCarDescriptionSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSString, Id NSString] (Id INRentalCar)
initWithRentalCompanyName_type_make_model_rentalCarDescriptionSelector = mkSelector "initWithRentalCompanyName:type:make:model:rentalCarDescription:"

-- | @Selector@ for @rentalCompanyName@
rentalCompanyNameSelector :: Selector '[] (Id NSString)
rentalCompanyNameSelector = mkSelector "rentalCompanyName"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @make@
makeSelector :: Selector '[] (Id NSString)
makeSelector = mkSelector "make"

-- | @Selector@ for @model@
modelSelector :: Selector '[] (Id NSString)
modelSelector = mkSelector "model"

-- | @Selector@ for @rentalCarDescription@
rentalCarDescriptionSelector :: Selector '[] (Id NSString)
rentalCarDescriptionSelector = mkSelector "rentalCarDescription"

