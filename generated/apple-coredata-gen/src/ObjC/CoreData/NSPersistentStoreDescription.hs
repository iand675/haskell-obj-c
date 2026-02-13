{-# LANGUAGE DataKinds #-}
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
  , cloudKitContainerOptionsSelector
  , configurationSelector
  , initWithURLSelector
  , optionsSelector
  , persistentStoreDescriptionWithURLSelector
  , readOnlySelector
  , setCloudKitContainerOptionsSelector
  , setConfigurationSelector
  , setOption_forKeySelector
  , setReadOnlySelector
  , setShouldAddStoreAsynchronouslySelector
  , setShouldInferMappingModelAutomaticallySelector
  , setShouldMigrateStoreAutomaticallySelector
  , setTimeoutSelector
  , setTypeSelector
  , setURLSelector
  , setValue_forPragmaNamedSelector
  , shouldAddStoreAsynchronouslySelector
  , shouldInferMappingModelAutomaticallySelector
  , shouldMigrateStoreAutomaticallySelector
  , sqlitePragmasSelector
  , timeoutSelector
  , typeSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ persistentStoreDescriptionWithURL:@
persistentStoreDescriptionWithURL :: IsNSURL url => url -> IO (Id NSPersistentStoreDescription)
persistentStoreDescriptionWithURL url =
  do
    cls' <- getRequiredClass "NSPersistentStoreDescription"
    sendClassMessage cls' persistentStoreDescriptionWithURLSelector (toNSURL url)

-- | @- setOption:forKey:@
setOption_forKey :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSObject option, IsNSString key) => nsPersistentStoreDescription -> option -> key -> IO ()
setOption_forKey nsPersistentStoreDescription option key =
  sendMessage nsPersistentStoreDescription setOption_forKeySelector (toNSObject option) (toNSString key)

-- | @- setValue:forPragmaNamed:@
setValue_forPragmaNamed :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSObject value, IsNSString name) => nsPersistentStoreDescription -> value -> name -> IO ()
setValue_forPragmaNamed nsPersistentStoreDescription value name =
  sendMessage nsPersistentStoreDescription setValue_forPragmaNamedSelector (toNSObject value) (toNSString name)

-- | @- initWithURL:@
initWithURL :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSURL url) => nsPersistentStoreDescription -> url -> IO (Id NSPersistentStoreDescription)
initWithURL nsPersistentStoreDescription url =
  sendOwnedMessage nsPersistentStoreDescription initWithURLSelector (toNSURL url)

-- | @- type@
type_ :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO (Id NSString)
type_ nsPersistentStoreDescription =
  sendMessage nsPersistentStoreDescription typeSelector

-- | @- setType:@
setType :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSString value) => nsPersistentStoreDescription -> value -> IO ()
setType nsPersistentStoreDescription value =
  sendMessage nsPersistentStoreDescription setTypeSelector (toNSString value)

-- | @- configuration@
configuration :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO (Id NSString)
configuration nsPersistentStoreDescription =
  sendMessage nsPersistentStoreDescription configurationSelector

-- | @- setConfiguration:@
setConfiguration :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSString value) => nsPersistentStoreDescription -> value -> IO ()
setConfiguration nsPersistentStoreDescription value =
  sendMessage nsPersistentStoreDescription setConfigurationSelector (toNSString value)

-- | @- URL@
url :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO (Id NSURL)
url nsPersistentStoreDescription =
  sendMessage nsPersistentStoreDescription urlSelector

-- | @- setURL:@
setURL :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSURL value) => nsPersistentStoreDescription -> value -> IO ()
setURL nsPersistentStoreDescription value =
  sendMessage nsPersistentStoreDescription setURLSelector (toNSURL value)

-- | @- options@
options :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO (Id NSDictionary)
options nsPersistentStoreDescription =
  sendMessage nsPersistentStoreDescription optionsSelector

-- | @- readOnly@
readOnly :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO Bool
readOnly nsPersistentStoreDescription =
  sendMessage nsPersistentStoreDescription readOnlySelector

-- | @- setReadOnly:@
setReadOnly :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> Bool -> IO ()
setReadOnly nsPersistentStoreDescription value =
  sendMessage nsPersistentStoreDescription setReadOnlySelector value

-- | @- timeout@
timeout :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO CDouble
timeout nsPersistentStoreDescription =
  sendMessage nsPersistentStoreDescription timeoutSelector

-- | @- setTimeout:@
setTimeout :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> CDouble -> IO ()
setTimeout nsPersistentStoreDescription value =
  sendMessage nsPersistentStoreDescription setTimeoutSelector value

-- | @- sqlitePragmas@
sqlitePragmas :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO (Id NSDictionary)
sqlitePragmas nsPersistentStoreDescription =
  sendMessage nsPersistentStoreDescription sqlitePragmasSelector

-- | @- shouldAddStoreAsynchronously@
shouldAddStoreAsynchronously :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO Bool
shouldAddStoreAsynchronously nsPersistentStoreDescription =
  sendMessage nsPersistentStoreDescription shouldAddStoreAsynchronouslySelector

-- | @- setShouldAddStoreAsynchronously:@
setShouldAddStoreAsynchronously :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> Bool -> IO ()
setShouldAddStoreAsynchronously nsPersistentStoreDescription value =
  sendMessage nsPersistentStoreDescription setShouldAddStoreAsynchronouslySelector value

-- | @- shouldMigrateStoreAutomatically@
shouldMigrateStoreAutomatically :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO Bool
shouldMigrateStoreAutomatically nsPersistentStoreDescription =
  sendMessage nsPersistentStoreDescription shouldMigrateStoreAutomaticallySelector

-- | @- setShouldMigrateStoreAutomatically:@
setShouldMigrateStoreAutomatically :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> Bool -> IO ()
setShouldMigrateStoreAutomatically nsPersistentStoreDescription value =
  sendMessage nsPersistentStoreDescription setShouldMigrateStoreAutomaticallySelector value

-- | @- shouldInferMappingModelAutomatically@
shouldInferMappingModelAutomatically :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO Bool
shouldInferMappingModelAutomatically nsPersistentStoreDescription =
  sendMessage nsPersistentStoreDescription shouldInferMappingModelAutomaticallySelector

-- | @- setShouldInferMappingModelAutomatically:@
setShouldInferMappingModelAutomatically :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> Bool -> IO ()
setShouldInferMappingModelAutomatically nsPersistentStoreDescription value =
  sendMessage nsPersistentStoreDescription setShouldInferMappingModelAutomaticallySelector value

-- | Use this property to apply customized instances of NSPersistentCloudKitContainerOptions to a store description you wish to use with CloudKit.
--
-- ObjC selector: @- cloudKitContainerOptions@
cloudKitContainerOptions :: IsNSPersistentStoreDescription nsPersistentStoreDescription => nsPersistentStoreDescription -> IO (Id NSPersistentCloudKitContainerOptions)
cloudKitContainerOptions nsPersistentStoreDescription =
  sendMessage nsPersistentStoreDescription cloudKitContainerOptionsSelector

-- | Use this property to apply customized instances of NSPersistentCloudKitContainerOptions to a store description you wish to use with CloudKit.
--
-- ObjC selector: @- setCloudKitContainerOptions:@
setCloudKitContainerOptions :: (IsNSPersistentStoreDescription nsPersistentStoreDescription, IsNSPersistentCloudKitContainerOptions value) => nsPersistentStoreDescription -> value -> IO ()
setCloudKitContainerOptions nsPersistentStoreDescription value =
  sendMessage nsPersistentStoreDescription setCloudKitContainerOptionsSelector (toNSPersistentCloudKitContainerOptions value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @persistentStoreDescriptionWithURL:@
persistentStoreDescriptionWithURLSelector :: Selector '[Id NSURL] (Id NSPersistentStoreDescription)
persistentStoreDescriptionWithURLSelector = mkSelector "persistentStoreDescriptionWithURL:"

-- | @Selector@ for @setOption:forKey:@
setOption_forKeySelector :: Selector '[Id NSObject, Id NSString] ()
setOption_forKeySelector = mkSelector "setOption:forKey:"

-- | @Selector@ for @setValue:forPragmaNamed:@
setValue_forPragmaNamedSelector :: Selector '[Id NSObject, Id NSString] ()
setValue_forPragmaNamedSelector = mkSelector "setValue:forPragmaNamed:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id NSPersistentStoreDescription)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id NSString)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @setConfiguration:@
setConfigurationSelector :: Selector '[Id NSString] ()
setConfigurationSelector = mkSelector "setConfiguration:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id NSDictionary)
optionsSelector = mkSelector "options"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector '[] Bool
readOnlySelector = mkSelector "readOnly"

-- | @Selector@ for @setReadOnly:@
setReadOnlySelector :: Selector '[Bool] ()
setReadOnlySelector = mkSelector "setReadOnly:"

-- | @Selector@ for @timeout@
timeoutSelector :: Selector '[] CDouble
timeoutSelector = mkSelector "timeout"

-- | @Selector@ for @setTimeout:@
setTimeoutSelector :: Selector '[CDouble] ()
setTimeoutSelector = mkSelector "setTimeout:"

-- | @Selector@ for @sqlitePragmas@
sqlitePragmasSelector :: Selector '[] (Id NSDictionary)
sqlitePragmasSelector = mkSelector "sqlitePragmas"

-- | @Selector@ for @shouldAddStoreAsynchronously@
shouldAddStoreAsynchronouslySelector :: Selector '[] Bool
shouldAddStoreAsynchronouslySelector = mkSelector "shouldAddStoreAsynchronously"

-- | @Selector@ for @setShouldAddStoreAsynchronously:@
setShouldAddStoreAsynchronouslySelector :: Selector '[Bool] ()
setShouldAddStoreAsynchronouslySelector = mkSelector "setShouldAddStoreAsynchronously:"

-- | @Selector@ for @shouldMigrateStoreAutomatically@
shouldMigrateStoreAutomaticallySelector :: Selector '[] Bool
shouldMigrateStoreAutomaticallySelector = mkSelector "shouldMigrateStoreAutomatically"

-- | @Selector@ for @setShouldMigrateStoreAutomatically:@
setShouldMigrateStoreAutomaticallySelector :: Selector '[Bool] ()
setShouldMigrateStoreAutomaticallySelector = mkSelector "setShouldMigrateStoreAutomatically:"

-- | @Selector@ for @shouldInferMappingModelAutomatically@
shouldInferMappingModelAutomaticallySelector :: Selector '[] Bool
shouldInferMappingModelAutomaticallySelector = mkSelector "shouldInferMappingModelAutomatically"

-- | @Selector@ for @setShouldInferMappingModelAutomatically:@
setShouldInferMappingModelAutomaticallySelector :: Selector '[Bool] ()
setShouldInferMappingModelAutomaticallySelector = mkSelector "setShouldInferMappingModelAutomatically:"

-- | @Selector@ for @cloudKitContainerOptions@
cloudKitContainerOptionsSelector :: Selector '[] (Id NSPersistentCloudKitContainerOptions)
cloudKitContainerOptionsSelector = mkSelector "cloudKitContainerOptions"

-- | @Selector@ for @setCloudKitContainerOptions:@
setCloudKitContainerOptionsSelector :: Selector '[Id NSPersistentCloudKitContainerOptions] ()
setCloudKitContainerOptionsSelector = mkSelector "setCloudKitContainerOptions:"

