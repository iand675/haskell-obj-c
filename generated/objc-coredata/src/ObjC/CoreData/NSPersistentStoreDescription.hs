{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentStoreDescription@.
module ObjC.CoreData.NSPersistentStoreDescription
  ( NSPersistentStoreDescription
  , IsNSPersistentStoreDescription(..)
  , persistentStoreDescriptionWithURL
  , setOption_forKey
  , setValue_forPragmaNamed
  , initWithURL
  , type_
  , setType
  , configuration
  , setConfiguration
  , url
  , setURL
  , options
  , readOnly
  , setReadOnly
  , timeout
  , setTimeout
  , sqlitePragmas
  , shouldAddStoreAsynchronously
  , setShouldAddStoreAsynchronously
  , shouldMigrateStoreAutomatically
  , setShouldMigrateStoreAutomatically
  , shouldInferMappingModelAutomatically
  , setShouldInferMappingModelAutomatically
  , cloudKitContainerOptions
  , setCloudKitContainerOptions
  , persistentStoreDescriptionWithURLSelector
  , setOption_forKeySelector
  , setValue_forPragmaNamedSelector
  , initWithURLSelector
  , typeSelector
  , setTypeSelector
  , configurationSelector
  , setConfigurationSelector
  , urlSelector
  , setURLSelector
  , optionsSelector
  , readOnlySelector
  , setReadOnlySelector
  , timeoutSelector
  , setTimeoutSelector
  , sqlitePragmasSelector
  , shouldAddStoreAsynchronouslySelector
  , setShouldAddStoreAsynchronouslySelector
  , shouldMigrateStoreAutomaticallySelector
  , setShouldMigrateStoreAutomaticallySelector
  , shouldInferMappingModelAutomaticallySelector
  , setShouldInferMappingModelAutomaticallySelector
  , cloudKitContainerOptionsSelector
  , setCloudKitContainerOptionsSelector


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

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ persistentStoreDescriptionWithURL:@
persistentStoreDescriptionWithURL :: IsNSURL url => url -> IO (Id NSPersistentStoreDescription)
persistentStoreDescriptionWithURL url =
  do
    cls' <- getRequiredClass "NSPersistentStoreDescription"
    withObjCPtr url $ \raw_url ->
      sendClassMsg cls' (mkSelector "persistentStoreDescriptionWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- setOption:forKey:@
setOption_forKey :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSObject option, IsNSString key) => nsPersistentStoreDescription -> option -> key -> IO ()
setOption_forKey nsPersistentStoreDescription  option key =
withObjCPtr option $ \raw_option ->
  withObjCPtr key $ \raw_key ->
      sendMsg nsPersistentStoreDescription (mkSelector "setOption:forKey:") retVoid [argPtr (castPtr raw_option :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- setValue:forPragmaNamed:@
setValue_forPragmaNamed :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSObject value, IsNSString name) => nsPersistentStoreDescription -> value -> name -> IO ()
setValue_forPragmaNamed nsPersistentStoreDescription  value name =
withObjCPtr value $ \raw_value ->
  withObjCPtr name $ \raw_name ->
      sendMsg nsPersistentStoreDescription (mkSelector "setValue:forPragmaNamed:") retVoid [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | @- initWithURL:@
initWithURL :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSURL url) => nsPersistentStoreDescription -> url -> IO (Id NSPersistentStoreDescription)
initWithURL nsPersistentStoreDescription  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsPersistentStoreDescription (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- type@
type_ :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO (Id NSString)
type_ nsPersistentStoreDescription  =
  sendMsg nsPersistentStoreDescription (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSString value) => nsPersistentStoreDescription -> value -> IO ()
setType nsPersistentStoreDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersistentStoreDescription (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- configuration@
configuration :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO (Id NSString)
configuration nsPersistentStoreDescription  =
  sendMsg nsPersistentStoreDescription (mkSelector "configuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setConfiguration:@
setConfiguration :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSString value) => nsPersistentStoreDescription -> value -> IO ()
setConfiguration nsPersistentStoreDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersistentStoreDescription (mkSelector "setConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- URL@
url :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO (Id NSURL)
url nsPersistentStoreDescription  =
  sendMsg nsPersistentStoreDescription (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURL:@
setURL :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSURL value) => nsPersistentStoreDescription -> value -> IO ()
setURL nsPersistentStoreDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersistentStoreDescription (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- options@
options :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO (Id NSDictionary)
options nsPersistentStoreDescription  =
  sendMsg nsPersistentStoreDescription (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- readOnly@
readOnly :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO Bool
readOnly nsPersistentStoreDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStoreDescription (mkSelector "readOnly") retCULong []

-- | @- setReadOnly:@
setReadOnly :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> Bool -> IO ()
setReadOnly nsPersistentStoreDescription  value =
  sendMsg nsPersistentStoreDescription (mkSelector "setReadOnly:") retVoid [argCULong (if value then 1 else 0)]

-- | @- timeout@
timeout :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO CDouble
timeout nsPersistentStoreDescription  =
  sendMsg nsPersistentStoreDescription (mkSelector "timeout") retCDouble []

-- | @- setTimeout:@
setTimeout :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> CDouble -> IO ()
setTimeout nsPersistentStoreDescription  value =
  sendMsg nsPersistentStoreDescription (mkSelector "setTimeout:") retVoid [argCDouble (fromIntegral value)]

-- | @- sqlitePragmas@
sqlitePragmas :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO (Id NSDictionary)
sqlitePragmas nsPersistentStoreDescription  =
  sendMsg nsPersistentStoreDescription (mkSelector "sqlitePragmas") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- shouldAddStoreAsynchronously@
shouldAddStoreAsynchronously :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO Bool
shouldAddStoreAsynchronously nsPersistentStoreDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStoreDescription (mkSelector "shouldAddStoreAsynchronously") retCULong []

-- | @- setShouldAddStoreAsynchronously:@
setShouldAddStoreAsynchronously :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> Bool -> IO ()
setShouldAddStoreAsynchronously nsPersistentStoreDescription  value =
  sendMsg nsPersistentStoreDescription (mkSelector "setShouldAddStoreAsynchronously:") retVoid [argCULong (if value then 1 else 0)]

-- | @- shouldMigrateStoreAutomatically@
shouldMigrateStoreAutomatically :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO Bool
shouldMigrateStoreAutomatically nsPersistentStoreDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStoreDescription (mkSelector "shouldMigrateStoreAutomatically") retCULong []

-- | @- setShouldMigrateStoreAutomatically:@
setShouldMigrateStoreAutomatically :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> Bool -> IO ()
setShouldMigrateStoreAutomatically nsPersistentStoreDescription  value =
  sendMsg nsPersistentStoreDescription (mkSelector "setShouldMigrateStoreAutomatically:") retVoid [argCULong (if value then 1 else 0)]

-- | @- shouldInferMappingModelAutomatically@
shouldInferMappingModelAutomatically :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO Bool
shouldInferMappingModelAutomatically nsPersistentStoreDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStoreDescription (mkSelector "shouldInferMappingModelAutomatically") retCULong []

-- | @- setShouldInferMappingModelAutomatically:@
setShouldInferMappingModelAutomatically :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> Bool -> IO ()
setShouldInferMappingModelAutomatically nsPersistentStoreDescription  value =
  sendMsg nsPersistentStoreDescription (mkSelector "setShouldInferMappingModelAutomatically:") retVoid [argCULong (if value then 1 else 0)]

-- | Use this property to apply customized instances of NSPersistentCloudKitContainerOptions to a store description you wish to use with CloudKit.
--
-- ObjC selector: @- cloudKitContainerOptions@
cloudKitContainerOptions :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO (Id NSPersistentCloudKitContainerOptions)
cloudKitContainerOptions nsPersistentStoreDescription  =
  sendMsg nsPersistentStoreDescription (mkSelector "cloudKitContainerOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Use this property to apply customized instances of NSPersistentCloudKitContainerOptions to a store description you wish to use with CloudKit.
--
-- ObjC selector: @- setCloudKitContainerOptions:@
setCloudKitContainerOptions :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSPersistentCloudKitContainerOptions value) => nsPersistentStoreDescription -> value -> IO ()
setCloudKitContainerOptions nsPersistentStoreDescription  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersistentStoreDescription (mkSelector "setCloudKitContainerOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @persistentStoreDescriptionWithURL:@
persistentStoreDescriptionWithURLSelector :: Selector
persistentStoreDescriptionWithURLSelector = mkSelector "persistentStoreDescriptionWithURL:"

-- | @Selector@ for @setOption:forKey:@
setOption_forKeySelector :: Selector
setOption_forKeySelector = mkSelector "setOption:forKey:"

-- | @Selector@ for @setValue:forPragmaNamed:@
setValue_forPragmaNamedSelector :: Selector
setValue_forPragmaNamedSelector = mkSelector "setValue:forPragmaNamed:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @setConfiguration:@
setConfigurationSelector :: Selector
setConfigurationSelector = mkSelector "setConfiguration:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @setReadOnly:@
setReadOnlySelector :: Selector
setReadOnlySelector = mkSelector "setReadOnly:"

-- | @Selector@ for @timeout@
timeoutSelector :: Selector
timeoutSelector = mkSelector "timeout"

-- | @Selector@ for @setTimeout:@
setTimeoutSelector :: Selector
setTimeoutSelector = mkSelector "setTimeout:"

-- | @Selector@ for @sqlitePragmas@
sqlitePragmasSelector :: Selector
sqlitePragmasSelector = mkSelector "sqlitePragmas"

-- | @Selector@ for @shouldAddStoreAsynchronously@
shouldAddStoreAsynchronouslySelector :: Selector
shouldAddStoreAsynchronouslySelector = mkSelector "shouldAddStoreAsynchronously"

-- | @Selector@ for @setShouldAddStoreAsynchronously:@
setShouldAddStoreAsynchronouslySelector :: Selector
setShouldAddStoreAsynchronouslySelector = mkSelector "setShouldAddStoreAsynchronously:"

-- | @Selector@ for @shouldMigrateStoreAutomatically@
shouldMigrateStoreAutomaticallySelector :: Selector
shouldMigrateStoreAutomaticallySelector = mkSelector "shouldMigrateStoreAutomatically"

-- | @Selector@ for @setShouldMigrateStoreAutomatically:@
setShouldMigrateStoreAutomaticallySelector :: Selector
setShouldMigrateStoreAutomaticallySelector = mkSelector "setShouldMigrateStoreAutomatically:"

-- | @Selector@ for @shouldInferMappingModelAutomatically@
shouldInferMappingModelAutomaticallySelector :: Selector
shouldInferMappingModelAutomaticallySelector = mkSelector "shouldInferMappingModelAutomatically"

-- | @Selector@ for @setShouldInferMappingModelAutomatically:@
setShouldInferMappingModelAutomaticallySelector :: Selector
setShouldInferMappingModelAutomaticallySelector = mkSelector "setShouldInferMappingModelAutomatically:"

-- | @Selector@ for @cloudKitContainerOptions@
cloudKitContainerOptionsSelector :: Selector
cloudKitContainerOptionsSelector = mkSelector "cloudKitContainerOptions"

-- | @Selector@ for @setCloudKitContainerOptions:@
setCloudKitContainerOptionsSelector :: Selector
setCloudKitContainerOptionsSelector = mkSelector "setCloudKitContainerOptions:"

