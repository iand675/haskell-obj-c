{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSUserDefaults is a hierarchical persistent interprocess (optionally distributed) key-value store, optimized for storing user settings.
--
-- Hierarchical: NSUserDefaults has a list of places to look for data called the "search list". A search list is referred to by an arbitrary string called the "suite identifier" or "domain identifier". When queried, NSUserDefaults checks each entry of its search list until it finds one that contains the key in question, or has searched the whole list. The list is (note: "current host + current user" preferences are unimplemented on iOS, watchOS, and tvOS, and "any user" preferences are not generally useful for applications on those operating systems): - Managed ("forced") preferences, set by a configuration profile or via mcx from a network administrator - Commandline arguments - Preferences for the current domain, in the cloud - Preferences for the current domain, the current user, in the current host - Preferences for the current domain, the current user, in any host - Preferences added via -addSuiteNamed: - Preferences global to all apps for the current user, in the current host - Preferences global to all apps for the current user, in any host - Preferences for the current domain, for all users, in the current host - Preferences global to all apps for all users, in the current host - Preferences registered with -registerDefaults:
--
-- Persistent: Preferences stored in NSUserDefaults persist across reboots and relaunches of apps unless otherwise specified.
--
-- Interprocess: Preferences may be accessible to and modified from multiple processes simultaneously (for example between an application and an extension).
--
-- Optionally distributed (Currently only supported in Shared iPad for Students mode):  Data stored in user defaults can be made "ubiqitous", i.e. synchronized between devices via the cloud.  Ubiquitous user defaults are automatically propagated to all devices logged into the same iCloud account. When reading defaults (via -*ForKey: methods on NSUserDefaults), ubiquitous defaults are searched before local defaults. All operations on ubiquitous defaults are asynchronous, so registered defaults may be returned in place of ubiquitous defaults if downloading from iCloud hasn't finished. Ubiquitous defaults are specified in the Defaults Configuration File for an application.
--
-- Key-Value Store: NSUserDefaults stores Property List objects (NSString, NSData, NSNumber, NSDate, NSArray, and NSDictionary) identified by NSString keys, similar to an NSMutableDictionary.
--
-- Optimized for storing user settings: NSUserDefaults is intended for relatively small amounts of data, queried very frequently, and modified occasionally. Using it in other ways may be slow or use more memory than solutions more suited to those uses.
--
-- The 'App' CFPreferences functions in CoreFoundation act on the same search lists that NSUserDefaults does.
--
-- NSUserDefaults can be observed using Key-Value Observing for any key stored in it. Using NSKeyValueObservingOptionPrior to observe changes from other processes or devices will behave as though NSKeyValueObservingOptionPrior was not specified.
--
-- Generated bindings for @NSUserDefaults@.
module ObjC.Foundation.NSUserDefaults
  ( NSUserDefaults
  , IsNSUserDefaults(..)
  , resetStandardUserDefaults
  , init_
  , initWithSuiteName
  , initWithUser
  , objectForKey
  , setObject_forKey
  , removeObjectForKey
  , stringForKey
  , arrayForKey
  , dictionaryForKey
  , dataForKey
  , stringArrayForKey
  , integerForKey
  , floatForKey
  , doubleForKey
  , boolForKey
  , urlForKey
  , setInteger_forKey
  , setFloat_forKey
  , setDouble_forKey
  , setBool_forKey
  , setURL_forKey
  , registerDefaults
  , addSuiteNamed
  , removeSuiteNamed
  , dictionaryRepresentation
  , volatileDomainForName
  , setVolatileDomain_forName
  , removeVolatileDomainForName
  , persistentDomainNames
  , persistentDomainForName
  , setPersistentDomain_forName
  , removePersistentDomainForName
  , synchronize
  , objectIsForcedForKey
  , objectIsForcedForKey_inDomain
  , standardUserDefaults
  , volatileDomainNames
  , resetStandardUserDefaultsSelector
  , initSelector
  , initWithSuiteNameSelector
  , initWithUserSelector
  , objectForKeySelector
  , setObject_forKeySelector
  , removeObjectForKeySelector
  , stringForKeySelector
  , arrayForKeySelector
  , dictionaryForKeySelector
  , dataForKeySelector
  , stringArrayForKeySelector
  , integerForKeySelector
  , floatForKeySelector
  , doubleForKeySelector
  , boolForKeySelector
  , urlForKeySelector
  , setInteger_forKeySelector
  , setFloat_forKeySelector
  , setDouble_forKeySelector
  , setBool_forKeySelector
  , setURL_forKeySelector
  , registerDefaultsSelector
  , addSuiteNamedSelector
  , removeSuiteNamedSelector
  , dictionaryRepresentationSelector
  , volatileDomainForNameSelector
  , setVolatileDomain_forNameSelector
  , removeVolatileDomainForNameSelector
  , persistentDomainNamesSelector
  , persistentDomainForNameSelector
  , setPersistentDomain_forNameSelector
  , removePersistentDomainForNameSelector
  , synchronizeSelector
  , objectIsForcedForKeySelector
  , objectIsForcedForKey_inDomainSelector
  , standardUserDefaultsSelector
  , volatileDomainNamesSelector


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

import ObjC.Foundation.Internal.Classes

-- | +resetStandardUserDefaults releases the standardUserDefaults and sets it to nil. A new standardUserDefaults will be created the next time it's accessed. The only visible effect this has is that all KVO observers of the previous standardUserDefaults will no longer be observing it.
--
-- ObjC selector: @+ resetStandardUserDefaults@
resetStandardUserDefaults :: IO ()
resetStandardUserDefaults  =
  do
    cls' <- getRequiredClass "NSUserDefaults"
    sendClassMsg cls' (mkSelector "resetStandardUserDefaults") retVoid []

-- | -init is equivalent to -initWithSuiteName:nil
--
-- ObjC selector: @- init@
init_ :: IsNSUserDefaults nsUserDefaults => nsUserDefaults -> IO (Id NSUserDefaults)
init_ nsUserDefaults  =
  sendMsg nsUserDefaults (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | -initWithSuiteName: initializes an instance of NSUserDefaults that searches the shared preferences search list for the domain 'suitename'. For example, using the identifier of an application group will cause the receiver to search the preferences for that group. Passing the current application's bundle identifier, NSGlobalDomain, or the corresponding CFPreferences constants is an error. Passing nil will search the default search list.
--
-- ObjC selector: @- initWithSuiteName:@
initWithSuiteName :: (IsNSUserDefaults nsUserDefaults, IsNSString suitename) => nsUserDefaults -> suitename -> IO (Id NSUserDefaults)
initWithSuiteName nsUserDefaults  suitename =
withObjCPtr suitename $ \raw_suitename ->
    sendMsg nsUserDefaults (mkSelector "initWithSuiteName:") (retPtr retVoid) [argPtr (castPtr raw_suitename :: Ptr ())] >>= ownedObject . castPtr

-- | -initWithUser: is equivalent to -init
--
-- ObjC selector: @- initWithUser:@
initWithUser :: (IsNSUserDefaults nsUserDefaults, IsNSString username) => nsUserDefaults -> username -> IO RawId
initWithUser nsUserDefaults  username =
withObjCPtr username $ \raw_username ->
    fmap (RawId . castPtr) $ sendMsg nsUserDefaults (mkSelector "initWithUser:") (retPtr retVoid) [argPtr (castPtr raw_username :: Ptr ())]

-- | -objectForKey: will search the receiver's search list for a default with the key 'defaultName' and return it. If another process has changed defaults in the search list, NSUserDefaults will automatically update to the latest values. If the key in question has been marked as ubiquitous via a Defaults Configuration File, the latest value may not be immediately available, and the registered value will be returned instead.
--
-- ObjC selector: @- objectForKey:@
objectForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> defaultName -> IO RawId
objectForKey nsUserDefaults  defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    fmap (RawId . castPtr) $ sendMsg nsUserDefaults (mkSelector "objectForKey:") (retPtr retVoid) [argPtr (castPtr raw_defaultName :: Ptr ())]

-- | -setObject:forKey: immediately stores a value (or removes the value if nil is passed as the value) for the provided key in the search list entry for the receiver's suite name in the current user and any host, then asynchronously stores the value persistently, where it is made available to other processes.
--
-- ObjC selector: @- setObject:forKey:@
setObject_forKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> RawId -> defaultName -> IO ()
setObject_forKey nsUserDefaults  value defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "setObject:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_defaultName :: Ptr ())]

-- | -removeObjectForKey: is equivalent to -[... setObject:nil forKey:defaultName]
--
-- ObjC selector: @- removeObjectForKey:@
removeObjectForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> defaultName -> IO ()
removeObjectForKey nsUserDefaults  defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "removeObjectForKey:") retVoid [argPtr (castPtr raw_defaultName :: Ptr ())]

-- | -stringForKey: is equivalent to -objectForKey:, except that it will convert NSNumber values to their NSString representation. If a non-string non-number value is found, nil will be returned.
--
-- ObjC selector: @- stringForKey:@
stringForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> defaultName -> IO (Id NSString)
stringForKey nsUserDefaults  defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "stringForKey:") (retPtr retVoid) [argPtr (castPtr raw_defaultName :: Ptr ())] >>= retainedObject . castPtr

-- | -arrayForKey: is equivalent to -objectForKey:, except that it will return nil if the value is not an NSArray.
--
-- ObjC selector: @- arrayForKey:@
arrayForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> defaultName -> IO (Id NSArray)
arrayForKey nsUserDefaults  defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "arrayForKey:") (retPtr retVoid) [argPtr (castPtr raw_defaultName :: Ptr ())] >>= retainedObject . castPtr

-- | -dictionaryForKey: is equivalent to -objectForKey:, except that it will return nil if the value is not an NSDictionary.
--
-- ObjC selector: @- dictionaryForKey:@
dictionaryForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> defaultName -> IO (Id NSDictionary)
dictionaryForKey nsUserDefaults  defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "dictionaryForKey:") (retPtr retVoid) [argPtr (castPtr raw_defaultName :: Ptr ())] >>= retainedObject . castPtr

-- | -dataForKey: is equivalent to -objectForKey:, except that it will return nil if the value is not an NSData.
--
-- ObjC selector: @- dataForKey:@
dataForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> defaultName -> IO (Id NSData)
dataForKey nsUserDefaults  defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "dataForKey:") (retPtr retVoid) [argPtr (castPtr raw_defaultName :: Ptr ())] >>= retainedObject . castPtr

-- | -stringForKey: is equivalent to -objectForKey:, except that it will return nil if the value is not an NSArray<NSString *>. Note that unlike -stringForKey:, NSNumbers are not converted to NSStrings.
--
-- ObjC selector: @- stringArrayForKey:@
stringArrayForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> defaultName -> IO (Id NSArray)
stringArrayForKey nsUserDefaults  defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "stringArrayForKey:") (retPtr retVoid) [argPtr (castPtr raw_defaultName :: Ptr ())] >>= retainedObject . castPtr

-- | -integerForKey: is equivalent to -objectForKey:, except that it converts the returned value to an NSInteger. If the value is an NSNumber, the result of -integerValue will be returned. If the value is an NSString, it will be converted to NSInteger if possible. If the value is a boolean, it will be converted to either 1 for YES or 0 for NO. If the value is absent or can't be converted to an integer, 0 will be returned.
--
-- ObjC selector: @- integerForKey:@
integerForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> defaultName -> IO CLong
integerForKey nsUserDefaults  defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "integerForKey:") retCLong [argPtr (castPtr raw_defaultName :: Ptr ())]

-- | -floatForKey: is similar to -integerForKey:, except that it returns a float, and boolean values will not be converted.
--
-- ObjC selector: @- floatForKey:@
floatForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> defaultName -> IO CFloat
floatForKey nsUserDefaults  defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "floatForKey:") retCFloat [argPtr (castPtr raw_defaultName :: Ptr ())]

-- | -doubleForKey: is similar to -integerForKey:, except that it returns a double, and boolean values will not be converted.
--
-- ObjC selector: @- doubleForKey:@
doubleForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> defaultName -> IO CDouble
doubleForKey nsUserDefaults  defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "doubleForKey:") retCDouble [argPtr (castPtr raw_defaultName :: Ptr ())]

-- | -boolForKey: is equivalent to -objectForKey:, except that it converts the returned value to a BOOL. If the value is an NSNumber, NO will be returned if the value is 0, YES otherwise. If the value is an NSString, values of "YES" or "1" will return YES, and values of "NO", "0", or any other string will return NO. If the value is absent or can't be converted to a BOOL, NO will be returned.
--
-- ObjC selector: @- boolForKey:@
boolForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> defaultName -> IO Bool
boolForKey nsUserDefaults  defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserDefaults (mkSelector "boolForKey:") retCULong [argPtr (castPtr raw_defaultName :: Ptr ())]

-- | -URLForKey: is equivalent to -objectForKey: except that it converts the returned value to an NSURL. If the value is an NSString path, then it will construct a file URL to that path. If the value is an archived URL from -setURL:forKey: it will be unarchived. If the value is absent or can't be converted to an NSURL, nil will be returned.
--
-- ObjC selector: @- URLForKey:@
urlForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> defaultName -> IO (Id NSURL)
urlForKey nsUserDefaults  defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "URLForKey:") (retPtr retVoid) [argPtr (castPtr raw_defaultName :: Ptr ())] >>= retainedObject . castPtr

-- | -setInteger:forKey: is equivalent to -setObject:forKey: except that the value is converted from an NSInteger to an NSNumber.
--
-- ObjC selector: @- setInteger:forKey:@
setInteger_forKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> CLong -> defaultName -> IO ()
setInteger_forKey nsUserDefaults  value defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "setInteger:forKey:") retVoid [argCLong (fromIntegral value), argPtr (castPtr raw_defaultName :: Ptr ())]

-- | -setFloat:forKey: is equivalent to -setObject:forKey: except that the value is converted from a float to an NSNumber.
--
-- ObjC selector: @- setFloat:forKey:@
setFloat_forKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> CFloat -> defaultName -> IO ()
setFloat_forKey nsUserDefaults  value defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "setFloat:forKey:") retVoid [argCFloat (fromIntegral value), argPtr (castPtr raw_defaultName :: Ptr ())]

-- | -setDouble:forKey: is equivalent to -setObject:forKey: except that the value is converted from a double to an NSNumber.
--
-- ObjC selector: @- setDouble:forKey:@
setDouble_forKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> CDouble -> defaultName -> IO ()
setDouble_forKey nsUserDefaults  value defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "setDouble:forKey:") retVoid [argCDouble (fromIntegral value), argPtr (castPtr raw_defaultName :: Ptr ())]

-- | -setBool:forKey: is equivalent to -setObject:forKey: except that the value is converted from a BOOL to an NSNumber.
--
-- ObjC selector: @- setBool:forKey:@
setBool_forKey :: (IsNSUserDefaults nsUserDefaults, IsNSString defaultName) => nsUserDefaults -> Bool -> defaultName -> IO ()
setBool_forKey nsUserDefaults  value defaultName =
withObjCPtr defaultName $ \raw_defaultName ->
    sendMsg nsUserDefaults (mkSelector "setBool:forKey:") retVoid [argCULong (if value then 1 else 0), argPtr (castPtr raw_defaultName :: Ptr ())]

-- | -setURL:forKey is equivalent to -setObject:forKey: except that the value is archived to an NSData. Use -URLForKey: to retrieve values set this way.
--
-- ObjC selector: @- setURL:forKey:@
setURL_forKey :: (IsNSUserDefaults nsUserDefaults, IsNSURL url, IsNSString defaultName) => nsUserDefaults -> url -> defaultName -> IO ()
setURL_forKey nsUserDefaults  url defaultName =
withObjCPtr url $ \raw_url ->
  withObjCPtr defaultName $ \raw_defaultName ->
      sendMsg nsUserDefaults (mkSelector "setURL:forKey:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_defaultName :: Ptr ())]

-- | -registerDefaults: adds the registrationDictionary to the last item in every search list. This means that after NSUserDefaults has looked for a value in every other valid location, it will look in registered defaults, making them useful as a "fallback" value. Registered defaults are never stored between runs of an application, and are visible only to the application that registers them.
--
-- Default values from Defaults Configuration Files will automatically be registered.
--
-- ObjC selector: @- registerDefaults:@
registerDefaults :: (IsNSUserDefaults nsUserDefaults, IsNSDictionary registrationDictionary) => nsUserDefaults -> registrationDictionary -> IO ()
registerDefaults nsUserDefaults  registrationDictionary =
withObjCPtr registrationDictionary $ \raw_registrationDictionary ->
    sendMsg nsUserDefaults (mkSelector "registerDefaults:") retVoid [argPtr (castPtr raw_registrationDictionary :: Ptr ())]

-- | -addSuiteNamed: adds the full search list for 'suiteName' as a sub-search-list of the receiver's. The additional search lists are searched after the current domain, but before global defaults. Passing NSGlobalDomain or the current application's bundle identifier is unsupported.
--
-- ObjC selector: @- addSuiteNamed:@
addSuiteNamed :: (IsNSUserDefaults nsUserDefaults, IsNSString suiteName) => nsUserDefaults -> suiteName -> IO ()
addSuiteNamed nsUserDefaults  suiteName =
withObjCPtr suiteName $ \raw_suiteName ->
    sendMsg nsUserDefaults (mkSelector "addSuiteNamed:") retVoid [argPtr (castPtr raw_suiteName :: Ptr ())]

-- | -removeSuiteNamed: removes a sub-searchlist added via -addSuiteNamed:.
--
-- ObjC selector: @- removeSuiteNamed:@
removeSuiteNamed :: (IsNSUserDefaults nsUserDefaults, IsNSString suiteName) => nsUserDefaults -> suiteName -> IO ()
removeSuiteNamed nsUserDefaults  suiteName =
withObjCPtr suiteName $ \raw_suiteName ->
    sendMsg nsUserDefaults (mkSelector "removeSuiteNamed:") retVoid [argPtr (castPtr raw_suiteName :: Ptr ())]

-- | -dictionaryRepresentation returns a composite snapshot of the values in the receiver's search list, such that [[receiver dictionaryRepresentation] objectForKey:x] will return the same thing as [receiver objectForKey:x].
--
-- ObjC selector: @- dictionaryRepresentation@
dictionaryRepresentation :: IsNSUserDefaults nsUserDefaults => nsUserDefaults -> IO (Id NSDictionary)
dictionaryRepresentation nsUserDefaults  =
  sendMsg nsUserDefaults (mkSelector "dictionaryRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- volatileDomainForName:@
volatileDomainForName :: (IsNSUserDefaults nsUserDefaults, IsNSString domainName) => nsUserDefaults -> domainName -> IO (Id NSDictionary)
volatileDomainForName nsUserDefaults  domainName =
withObjCPtr domainName $ \raw_domainName ->
    sendMsg nsUserDefaults (mkSelector "volatileDomainForName:") (retPtr retVoid) [argPtr (castPtr raw_domainName :: Ptr ())] >>= retainedObject . castPtr

-- | @- setVolatileDomain:forName:@
setVolatileDomain_forName :: (IsNSUserDefaults nsUserDefaults, IsNSDictionary domain, IsNSString domainName) => nsUserDefaults -> domain -> domainName -> IO ()
setVolatileDomain_forName nsUserDefaults  domain domainName =
withObjCPtr domain $ \raw_domain ->
  withObjCPtr domainName $ \raw_domainName ->
      sendMsg nsUserDefaults (mkSelector "setVolatileDomain:forName:") retVoid [argPtr (castPtr raw_domain :: Ptr ()), argPtr (castPtr raw_domainName :: Ptr ())]

-- | @- removeVolatileDomainForName:@
removeVolatileDomainForName :: (IsNSUserDefaults nsUserDefaults, IsNSString domainName) => nsUserDefaults -> domainName -> IO ()
removeVolatileDomainForName nsUserDefaults  domainName =
withObjCPtr domainName $ \raw_domainName ->
    sendMsg nsUserDefaults (mkSelector "removeVolatileDomainForName:") retVoid [argPtr (castPtr raw_domainName :: Ptr ())]

-- | -persistentDomainNames returns an incomplete list of domains that have preferences stored in them.
--
-- ObjC selector: @- persistentDomainNames@
persistentDomainNames :: IsNSUserDefaults nsUserDefaults => nsUserDefaults -> IO (Id NSArray)
persistentDomainNames nsUserDefaults  =
  sendMsg nsUserDefaults (mkSelector "persistentDomainNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | -persistentDomainForName: returns a dictionary representation of the search list entry specified by 'domainName', the current user, and any host.
--
-- ObjC selector: @- persistentDomainForName:@
persistentDomainForName :: (IsNSUserDefaults nsUserDefaults, IsNSString domainName) => nsUserDefaults -> domainName -> IO (Id NSDictionary)
persistentDomainForName nsUserDefaults  domainName =
withObjCPtr domainName $ \raw_domainName ->
    sendMsg nsUserDefaults (mkSelector "persistentDomainForName:") (retPtr retVoid) [argPtr (castPtr raw_domainName :: Ptr ())] >>= retainedObject . castPtr

-- | -setPersistentDomain:forName: replaces all values in the search list entry specified by 'domainName', the current user, and any host, with the values in 'domain'. The change will be persisted.
--
-- ObjC selector: @- setPersistentDomain:forName:@
setPersistentDomain_forName :: (IsNSUserDefaults nsUserDefaults, IsNSDictionary domain, IsNSString domainName) => nsUserDefaults -> domain -> domainName -> IO ()
setPersistentDomain_forName nsUserDefaults  domain domainName =
withObjCPtr domain $ \raw_domain ->
  withObjCPtr domainName $ \raw_domainName ->
      sendMsg nsUserDefaults (mkSelector "setPersistentDomain:forName:") retVoid [argPtr (castPtr raw_domain :: Ptr ()), argPtr (castPtr raw_domainName :: Ptr ())]

-- | -removePersistentDomainForName: removes all values from the search list entry specified by 'domainName', the current user, and any host. The change is persistent.
--
-- ObjC selector: @- removePersistentDomainForName:@
removePersistentDomainForName :: (IsNSUserDefaults nsUserDefaults, IsNSString domainName) => nsUserDefaults -> domainName -> IO ()
removePersistentDomainForName nsUserDefaults  domainName =
withObjCPtr domainName $ \raw_domainName ->
    sendMsg nsUserDefaults (mkSelector "removePersistentDomainForName:") retVoid [argPtr (castPtr raw_domainName :: Ptr ())]

-- | -synchronize is deprecated and will be marked with the API_DEPRECATED macro in a future release.
--
-- -synchronize blocks the calling thread until all in-progress set operations have completed. This is no longer necessary. Replacements for previous uses of -synchronize depend on what the intent of calling synchronize was. If you synchronized... - ...before reading in order to fetch updated values: remove the synchronize call - ...after writing in order to notify another program to read: the other program can use KVO to observe the default without needing to notify - ...before exiting in a non-app (command line tool, agent, or daemon) process: call CFPreferencesAppSynchronize(kCFPreferencesCurrentApplication) - ...for any other reason: remove the synchronize call
--
-- ObjC selector: @- synchronize@
synchronize :: IsNSUserDefaults nsUserDefaults => nsUserDefaults -> IO Bool
synchronize nsUserDefaults  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserDefaults (mkSelector "synchronize") retCULong []

-- | @- objectIsForcedForKey:@
objectIsForcedForKey :: (IsNSUserDefaults nsUserDefaults, IsNSString key) => nsUserDefaults -> key -> IO Bool
objectIsForcedForKey nsUserDefaults  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserDefaults (mkSelector "objectIsForcedForKey:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- objectIsForcedForKey:inDomain:@
objectIsForcedForKey_inDomain :: (IsNSUserDefaults nsUserDefaults, IsNSString key, IsNSString domain) => nsUserDefaults -> key -> domain -> IO Bool
objectIsForcedForKey_inDomain nsUserDefaults  key domain =
withObjCPtr key $ \raw_key ->
  withObjCPtr domain $ \raw_domain ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserDefaults (mkSelector "objectIsForcedForKey:inDomain:") retCULong [argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_domain :: Ptr ())]

-- | +standardUserDefaults returns a global instance of NSUserDefaults configured to search the current application's search list.
--
-- ObjC selector: @+ standardUserDefaults@
standardUserDefaults :: IO (Id NSUserDefaults)
standardUserDefaults  =
  do
    cls' <- getRequiredClass "NSUserDefaults"
    sendClassMsg cls' (mkSelector "standardUserDefaults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- volatileDomainNames@
volatileDomainNames :: IsNSUserDefaults nsUserDefaults => nsUserDefaults -> IO (Id NSArray)
volatileDomainNames nsUserDefaults  =
  sendMsg nsUserDefaults (mkSelector "volatileDomainNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resetStandardUserDefaults@
resetStandardUserDefaultsSelector :: Selector
resetStandardUserDefaultsSelector = mkSelector "resetStandardUserDefaults"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSuiteName:@
initWithSuiteNameSelector :: Selector
initWithSuiteNameSelector = mkSelector "initWithSuiteName:"

-- | @Selector@ for @initWithUser:@
initWithUserSelector :: Selector
initWithUserSelector = mkSelector "initWithUser:"

-- | @Selector@ for @objectForKey:@
objectForKeySelector :: Selector
objectForKeySelector = mkSelector "objectForKey:"

-- | @Selector@ for @setObject:forKey:@
setObject_forKeySelector :: Selector
setObject_forKeySelector = mkSelector "setObject:forKey:"

-- | @Selector@ for @removeObjectForKey:@
removeObjectForKeySelector :: Selector
removeObjectForKeySelector = mkSelector "removeObjectForKey:"

-- | @Selector@ for @stringForKey:@
stringForKeySelector :: Selector
stringForKeySelector = mkSelector "stringForKey:"

-- | @Selector@ for @arrayForKey:@
arrayForKeySelector :: Selector
arrayForKeySelector = mkSelector "arrayForKey:"

-- | @Selector@ for @dictionaryForKey:@
dictionaryForKeySelector :: Selector
dictionaryForKeySelector = mkSelector "dictionaryForKey:"

-- | @Selector@ for @dataForKey:@
dataForKeySelector :: Selector
dataForKeySelector = mkSelector "dataForKey:"

-- | @Selector@ for @stringArrayForKey:@
stringArrayForKeySelector :: Selector
stringArrayForKeySelector = mkSelector "stringArrayForKey:"

-- | @Selector@ for @integerForKey:@
integerForKeySelector :: Selector
integerForKeySelector = mkSelector "integerForKey:"

-- | @Selector@ for @floatForKey:@
floatForKeySelector :: Selector
floatForKeySelector = mkSelector "floatForKey:"

-- | @Selector@ for @doubleForKey:@
doubleForKeySelector :: Selector
doubleForKeySelector = mkSelector "doubleForKey:"

-- | @Selector@ for @boolForKey:@
boolForKeySelector :: Selector
boolForKeySelector = mkSelector "boolForKey:"

-- | @Selector@ for @URLForKey:@
urlForKeySelector :: Selector
urlForKeySelector = mkSelector "URLForKey:"

-- | @Selector@ for @setInteger:forKey:@
setInteger_forKeySelector :: Selector
setInteger_forKeySelector = mkSelector "setInteger:forKey:"

-- | @Selector@ for @setFloat:forKey:@
setFloat_forKeySelector :: Selector
setFloat_forKeySelector = mkSelector "setFloat:forKey:"

-- | @Selector@ for @setDouble:forKey:@
setDouble_forKeySelector :: Selector
setDouble_forKeySelector = mkSelector "setDouble:forKey:"

-- | @Selector@ for @setBool:forKey:@
setBool_forKeySelector :: Selector
setBool_forKeySelector = mkSelector "setBool:forKey:"

-- | @Selector@ for @setURL:forKey:@
setURL_forKeySelector :: Selector
setURL_forKeySelector = mkSelector "setURL:forKey:"

-- | @Selector@ for @registerDefaults:@
registerDefaultsSelector :: Selector
registerDefaultsSelector = mkSelector "registerDefaults:"

-- | @Selector@ for @addSuiteNamed:@
addSuiteNamedSelector :: Selector
addSuiteNamedSelector = mkSelector "addSuiteNamed:"

-- | @Selector@ for @removeSuiteNamed:@
removeSuiteNamedSelector :: Selector
removeSuiteNamedSelector = mkSelector "removeSuiteNamed:"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

-- | @Selector@ for @volatileDomainForName:@
volatileDomainForNameSelector :: Selector
volatileDomainForNameSelector = mkSelector "volatileDomainForName:"

-- | @Selector@ for @setVolatileDomain:forName:@
setVolatileDomain_forNameSelector :: Selector
setVolatileDomain_forNameSelector = mkSelector "setVolatileDomain:forName:"

-- | @Selector@ for @removeVolatileDomainForName:@
removeVolatileDomainForNameSelector :: Selector
removeVolatileDomainForNameSelector = mkSelector "removeVolatileDomainForName:"

-- | @Selector@ for @persistentDomainNames@
persistentDomainNamesSelector :: Selector
persistentDomainNamesSelector = mkSelector "persistentDomainNames"

-- | @Selector@ for @persistentDomainForName:@
persistentDomainForNameSelector :: Selector
persistentDomainForNameSelector = mkSelector "persistentDomainForName:"

-- | @Selector@ for @setPersistentDomain:forName:@
setPersistentDomain_forNameSelector :: Selector
setPersistentDomain_forNameSelector = mkSelector "setPersistentDomain:forName:"

-- | @Selector@ for @removePersistentDomainForName:@
removePersistentDomainForNameSelector :: Selector
removePersistentDomainForNameSelector = mkSelector "removePersistentDomainForName:"

-- | @Selector@ for @synchronize@
synchronizeSelector :: Selector
synchronizeSelector = mkSelector "synchronize"

-- | @Selector@ for @objectIsForcedForKey:@
objectIsForcedForKeySelector :: Selector
objectIsForcedForKeySelector = mkSelector "objectIsForcedForKey:"

-- | @Selector@ for @objectIsForcedForKey:inDomain:@
objectIsForcedForKey_inDomainSelector :: Selector
objectIsForcedForKey_inDomainSelector = mkSelector "objectIsForcedForKey:inDomain:"

-- | @Selector@ for @standardUserDefaults@
standardUserDefaultsSelector :: Selector
standardUserDefaultsSelector = mkSelector "standardUserDefaults"

-- | @Selector@ for @volatileDomainNames@
volatileDomainNamesSelector :: Selector
volatileDomainNamesSelector = mkSelector "volatileDomainNames"

