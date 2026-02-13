{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QuartzFilter@.
module ObjC.Quartz.QuartzFilter
  ( QuartzFilter
  , IsQuartzFilter(..)
  , quartzFilterWithURL
  , quartzFilterWithProperties
  , quartzFilterWithOutputIntents
  , properties
  , url
  , localizedName
  , applyToContext
  , removeFromContext
  , applyToContextSelector
  , localizedNameSelector
  , propertiesSelector
  , quartzFilterWithOutputIntentsSelector
  , quartzFilterWithPropertiesSelector
  , quartzFilterWithURLSelector
  , removeFromContextSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ quartzFilterWithURL:@
quartzFilterWithURL :: IsNSURL aURL => aURL -> IO (Id QuartzFilter)
quartzFilterWithURL aURL =
  do
    cls' <- getRequiredClass "QuartzFilter"
    sendClassMessage cls' quartzFilterWithURLSelector (toNSURL aURL)

-- | @+ quartzFilterWithProperties:@
quartzFilterWithProperties :: IsNSDictionary properties => properties -> IO (Id QuartzFilter)
quartzFilterWithProperties properties =
  do
    cls' <- getRequiredClass "QuartzFilter"
    sendClassMessage cls' quartzFilterWithPropertiesSelector (toNSDictionary properties)

-- | @+ quartzFilterWithOutputIntents:@
quartzFilterWithOutputIntents :: IsNSArray outputIntents => outputIntents -> IO (Id QuartzFilter)
quartzFilterWithOutputIntents outputIntents =
  do
    cls' <- getRequiredClass "QuartzFilter"
    sendClassMessage cls' quartzFilterWithOutputIntentsSelector (toNSArray outputIntents)

-- | @- properties@
properties :: IsQuartzFilter quartzFilter => quartzFilter -> IO (Id NSDictionary)
properties quartzFilter =
  sendMessage quartzFilter propertiesSelector

-- | @- url@
url :: IsQuartzFilter quartzFilter => quartzFilter -> IO (Id NSURL)
url quartzFilter =
  sendMessage quartzFilter urlSelector

-- | @- localizedName@
localizedName :: IsQuartzFilter quartzFilter => quartzFilter -> IO (Id NSString)
localizedName quartzFilter =
  sendMessage quartzFilter localizedNameSelector

-- | @- applyToContext:@
applyToContext :: IsQuartzFilter quartzFilter => quartzFilter -> Ptr () -> IO Bool
applyToContext quartzFilter aContext =
  sendMessage quartzFilter applyToContextSelector aContext

-- | @- removeFromContext:@
removeFromContext :: IsQuartzFilter quartzFilter => quartzFilter -> Ptr () -> IO ()
removeFromContext quartzFilter aContext =
  sendMessage quartzFilter removeFromContextSelector aContext

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @quartzFilterWithURL:@
quartzFilterWithURLSelector :: Selector '[Id NSURL] (Id QuartzFilter)
quartzFilterWithURLSelector = mkSelector "quartzFilterWithURL:"

-- | @Selector@ for @quartzFilterWithProperties:@
quartzFilterWithPropertiesSelector :: Selector '[Id NSDictionary] (Id QuartzFilter)
quartzFilterWithPropertiesSelector = mkSelector "quartzFilterWithProperties:"

-- | @Selector@ for @quartzFilterWithOutputIntents:@
quartzFilterWithOutputIntentsSelector :: Selector '[Id NSArray] (Id QuartzFilter)
quartzFilterWithOutputIntentsSelector = mkSelector "quartzFilterWithOutputIntents:"

-- | @Selector@ for @properties@
propertiesSelector :: Selector '[] (Id NSDictionary)
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector '[] (Id NSString)
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @applyToContext:@
applyToContextSelector :: Selector '[Ptr ()] Bool
applyToContextSelector = mkSelector "applyToContext:"

-- | @Selector@ for @removeFromContext:@
removeFromContextSelector :: Selector '[Ptr ()] ()
removeFromContextSelector = mkSelector "removeFromContext:"

