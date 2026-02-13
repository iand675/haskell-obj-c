{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CISampler@.
module ObjC.CoreImage.CISampler
  ( CISampler
  , IsCISampler(..)
  , samplerWithImage
  , samplerWithImage_keysAndValues
  , samplerWithImage_options
  , initWithImage
  , initWithImage_keysAndValues
  , initWithImage_options
  , definition
  , definitionSelector
  , initWithImageSelector
  , initWithImage_keysAndValuesSelector
  , initWithImage_optionsSelector
  , samplerWithImageSelector
  , samplerWithImage_keysAndValuesSelector
  , samplerWithImage_optionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ samplerWithImage:@
samplerWithImage :: IsCIImage im => im -> IO (Id CISampler)
samplerWithImage im =
  do
    cls' <- getRequiredClass "CISampler"
    sendClassMessage cls' samplerWithImageSelector (toCIImage im)

-- | @+ samplerWithImage:keysAndValues:@
samplerWithImage_keysAndValues :: IsCIImage im => im -> RawId -> IO (Id CISampler)
samplerWithImage_keysAndValues im key0 =
  do
    cls' <- getRequiredClass "CISampler"
    sendClassMessage cls' samplerWithImage_keysAndValuesSelector (toCIImage im) key0

-- | @+ samplerWithImage:options:@
samplerWithImage_options :: (IsCIImage im, IsNSDictionary dict) => im -> dict -> IO (Id CISampler)
samplerWithImage_options im dict =
  do
    cls' <- getRequiredClass "CISampler"
    sendClassMessage cls' samplerWithImage_optionsSelector (toCIImage im) (toNSDictionary dict)

-- | @- initWithImage:@
initWithImage :: (IsCISampler ciSampler, IsCIImage im) => ciSampler -> im -> IO (Id CISampler)
initWithImage ciSampler im =
  sendOwnedMessage ciSampler initWithImageSelector (toCIImage im)

-- | @- initWithImage:keysAndValues:@
initWithImage_keysAndValues :: (IsCISampler ciSampler, IsCIImage im) => ciSampler -> im -> RawId -> IO (Id CISampler)
initWithImage_keysAndValues ciSampler im key0 =
  sendOwnedMessage ciSampler initWithImage_keysAndValuesSelector (toCIImage im) key0

-- | @- initWithImage:options:@
initWithImage_options :: (IsCISampler ciSampler, IsCIImage im, IsNSDictionary dict) => ciSampler -> im -> dict -> IO (Id CISampler)
initWithImage_options ciSampler im dict =
  sendOwnedMessage ciSampler initWithImage_optionsSelector (toCIImage im) (toNSDictionary dict)

-- | @- definition@
definition :: IsCISampler ciSampler => ciSampler -> IO (Id CIFilterShape)
definition ciSampler =
  sendMessage ciSampler definitionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @samplerWithImage:@
samplerWithImageSelector :: Selector '[Id CIImage] (Id CISampler)
samplerWithImageSelector = mkSelector "samplerWithImage:"

-- | @Selector@ for @samplerWithImage:keysAndValues:@
samplerWithImage_keysAndValuesSelector :: Selector '[Id CIImage, RawId] (Id CISampler)
samplerWithImage_keysAndValuesSelector = mkSelector "samplerWithImage:keysAndValues:"

-- | @Selector@ for @samplerWithImage:options:@
samplerWithImage_optionsSelector :: Selector '[Id CIImage, Id NSDictionary] (Id CISampler)
samplerWithImage_optionsSelector = mkSelector "samplerWithImage:options:"

-- | @Selector@ for @initWithImage:@
initWithImageSelector :: Selector '[Id CIImage] (Id CISampler)
initWithImageSelector = mkSelector "initWithImage:"

-- | @Selector@ for @initWithImage:keysAndValues:@
initWithImage_keysAndValuesSelector :: Selector '[Id CIImage, RawId] (Id CISampler)
initWithImage_keysAndValuesSelector = mkSelector "initWithImage:keysAndValues:"

-- | @Selector@ for @initWithImage:options:@
initWithImage_optionsSelector :: Selector '[Id CIImage, Id NSDictionary] (Id CISampler)
initWithImage_optionsSelector = mkSelector "initWithImage:options:"

-- | @Selector@ for @definition@
definitionSelector :: Selector '[] (Id CIFilterShape)
definitionSelector = mkSelector "definition"

