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
  , quartzFilterWithURLSelector
  , quartzFilterWithPropertiesSelector
  , quartzFilterWithOutputIntentsSelector
  , propertiesSelector
  , urlSelector
  , localizedNameSelector
  , applyToContextSelector
  , removeFromContextSelector


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

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ quartzFilterWithURL:@
quartzFilterWithURL :: IsNSURL aURL => aURL -> IO (Id QuartzFilter)
quartzFilterWithURL aURL =
  do
    cls' <- getRequiredClass "QuartzFilter"
    withObjCPtr aURL $ \raw_aURL ->
      sendClassMsg cls' (mkSelector "quartzFilterWithURL:") (retPtr retVoid) [argPtr (castPtr raw_aURL :: Ptr ())] >>= retainedObject . castPtr

-- | @+ quartzFilterWithProperties:@
quartzFilterWithProperties :: IsNSDictionary properties => properties -> IO (Id QuartzFilter)
quartzFilterWithProperties properties =
  do
    cls' <- getRequiredClass "QuartzFilter"
    withObjCPtr properties $ \raw_properties ->
      sendClassMsg cls' (mkSelector "quartzFilterWithProperties:") (retPtr retVoid) [argPtr (castPtr raw_properties :: Ptr ())] >>= retainedObject . castPtr

-- | @+ quartzFilterWithOutputIntents:@
quartzFilterWithOutputIntents :: IsNSArray outputIntents => outputIntents -> IO (Id QuartzFilter)
quartzFilterWithOutputIntents outputIntents =
  do
    cls' <- getRequiredClass "QuartzFilter"
    withObjCPtr outputIntents $ \raw_outputIntents ->
      sendClassMsg cls' (mkSelector "quartzFilterWithOutputIntents:") (retPtr retVoid) [argPtr (castPtr raw_outputIntents :: Ptr ())] >>= retainedObject . castPtr

-- | @- properties@
properties :: IsQuartzFilter quartzFilter => quartzFilter -> IO (Id NSDictionary)
properties quartzFilter  =
  sendMsg quartzFilter (mkSelector "properties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- url@
url :: IsQuartzFilter quartzFilter => quartzFilter -> IO (Id NSURL)
url quartzFilter  =
  sendMsg quartzFilter (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedName@
localizedName :: IsQuartzFilter quartzFilter => quartzFilter -> IO (Id NSString)
localizedName quartzFilter  =
  sendMsg quartzFilter (mkSelector "localizedName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- applyToContext:@
applyToContext :: IsQuartzFilter quartzFilter => quartzFilter -> Ptr () -> IO Bool
applyToContext quartzFilter  aContext =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg quartzFilter (mkSelector "applyToContext:") retCULong [argPtr aContext]

-- | @- removeFromContext:@
removeFromContext :: IsQuartzFilter quartzFilter => quartzFilter -> Ptr () -> IO ()
removeFromContext quartzFilter  aContext =
  sendMsg quartzFilter (mkSelector "removeFromContext:") retVoid [argPtr aContext]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @quartzFilterWithURL:@
quartzFilterWithURLSelector :: Selector
quartzFilterWithURLSelector = mkSelector "quartzFilterWithURL:"

-- | @Selector@ for @quartzFilterWithProperties:@
quartzFilterWithPropertiesSelector :: Selector
quartzFilterWithPropertiesSelector = mkSelector "quartzFilterWithProperties:"

-- | @Selector@ for @quartzFilterWithOutputIntents:@
quartzFilterWithOutputIntentsSelector :: Selector
quartzFilterWithOutputIntentsSelector = mkSelector "quartzFilterWithOutputIntents:"

-- | @Selector@ for @properties@
propertiesSelector :: Selector
propertiesSelector = mkSelector "properties"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @localizedName@
localizedNameSelector :: Selector
localizedNameSelector = mkSelector "localizedName"

-- | @Selector@ for @applyToContext:@
applyToContextSelector :: Selector
applyToContextSelector = mkSelector "applyToContext:"

-- | @Selector@ for @removeFromContext:@
removeFromContextSelector :: Selector
removeFromContextSelector = mkSelector "removeFromContext:"

