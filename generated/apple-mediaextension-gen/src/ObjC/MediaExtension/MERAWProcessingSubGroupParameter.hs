{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MERAWProcessingSubGroupParameter@.
module ObjC.MediaExtension.MERAWProcessingSubGroupParameter
  ( MERAWProcessingSubGroupParameter
  , IsMERAWProcessingSubGroupParameter(..)
  , initWithName_description_parameters
  , subGroupParameters
  , initWithName_description_parametersSelector
  , subGroupParametersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithName:description:parameters:@
initWithName_description_parameters :: (IsMERAWProcessingSubGroupParameter merawProcessingSubGroupParameter, IsNSString name, IsNSString description, IsNSArray parameters) => merawProcessingSubGroupParameter -> name -> description -> parameters -> IO (Id MERAWProcessingSubGroupParameter)
initWithName_description_parameters merawProcessingSubGroupParameter name description parameters =
  sendOwnedMessage merawProcessingSubGroupParameter initWithName_description_parametersSelector (toNSString name) (toNSString description) (toNSArray parameters)

-- | @- subGroupParameters@
subGroupParameters :: IsMERAWProcessingSubGroupParameter merawProcessingSubGroupParameter => merawProcessingSubGroupParameter -> IO (Id NSArray)
subGroupParameters merawProcessingSubGroupParameter =
  sendMessage merawProcessingSubGroupParameter subGroupParametersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithName:description:parameters:@
initWithName_description_parametersSelector :: Selector '[Id NSString, Id NSString, Id NSArray] (Id MERAWProcessingSubGroupParameter)
initWithName_description_parametersSelector = mkSelector "initWithName:description:parameters:"

-- | @Selector@ for @subGroupParameters@
subGroupParametersSelector :: Selector '[] (Id NSArray)
subGroupParametersSelector = mkSelector "subGroupParameters"

