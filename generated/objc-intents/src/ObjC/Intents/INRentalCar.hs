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
  , rentalCompanyNameSelector
  , typeSelector
  , makeSelector
  , modelSelector
  , rentalCarDescriptionSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINRentalCar inRentalCar => inRentalCar -> IO (Id INRentalCar)
init_ inRentalCar  =
  sendMsg inRentalCar (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithRentalCompanyName:type:make:model:rentalCarDescription:@
initWithRentalCompanyName_type_make_model_rentalCarDescription :: (IsINRentalCar inRentalCar, IsNSString rentalCompanyName, IsNSString type_, IsNSString make, IsNSString model, IsNSString rentalCarDescription) => inRentalCar -> rentalCompanyName -> type_ -> make -> model -> rentalCarDescription -> IO (Id INRentalCar)
initWithRentalCompanyName_type_make_model_rentalCarDescription inRentalCar  rentalCompanyName type_ make model rentalCarDescription =
withObjCPtr rentalCompanyName $ \raw_rentalCompanyName ->
  withObjCPtr type_ $ \raw_type_ ->
    withObjCPtr make $ \raw_make ->
      withObjCPtr model $ \raw_model ->
        withObjCPtr rentalCarDescription $ \raw_rentalCarDescription ->
            sendMsg inRentalCar (mkSelector "initWithRentalCompanyName:type:make:model:rentalCarDescription:") (retPtr retVoid) [argPtr (castPtr raw_rentalCompanyName :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_make :: Ptr ()), argPtr (castPtr raw_model :: Ptr ()), argPtr (castPtr raw_rentalCarDescription :: Ptr ())] >>= ownedObject . castPtr

-- | @- rentalCompanyName@
rentalCompanyName :: IsINRentalCar inRentalCar => inRentalCar -> IO (Id NSString)
rentalCompanyName inRentalCar  =
  sendMsg inRentalCar (mkSelector "rentalCompanyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsINRentalCar inRentalCar => inRentalCar -> IO (Id NSString)
type_ inRentalCar  =
  sendMsg inRentalCar (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- make@
make :: IsINRentalCar inRentalCar => inRentalCar -> IO (Id NSString)
make inRentalCar  =
  sendMsg inRentalCar (mkSelector "make") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- model@
model :: IsINRentalCar inRentalCar => inRentalCar -> IO (Id NSString)
model inRentalCar  =
  sendMsg inRentalCar (mkSelector "model") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rentalCarDescription@
rentalCarDescription :: IsINRentalCar inRentalCar => inRentalCar -> IO (Id NSString)
rentalCarDescription inRentalCar  =
  sendMsg inRentalCar (mkSelector "rentalCarDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRentalCompanyName:type:make:model:rentalCarDescription:@
initWithRentalCompanyName_type_make_model_rentalCarDescriptionSelector :: Selector
initWithRentalCompanyName_type_make_model_rentalCarDescriptionSelector = mkSelector "initWithRentalCompanyName:type:make:model:rentalCarDescription:"

-- | @Selector@ for @rentalCompanyName@
rentalCompanyNameSelector :: Selector
rentalCompanyNameSelector = mkSelector "rentalCompanyName"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @make@
makeSelector :: Selector
makeSelector = mkSelector "make"

-- | @Selector@ for @model@
modelSelector :: Selector
modelSelector = mkSelector "model"

-- | @Selector@ for @rentalCarDescription@
rentalCarDescriptionSelector :: Selector
rentalCarDescriptionSelector = mkSelector "rentalCarDescription"

