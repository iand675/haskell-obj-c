{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A collection of key-value observations which may be registered with multiple observable objects
--
-- Generated bindings for @NSKeyValueSharedObservers@.
module ObjC.Foundation.NSKeyValueSharedObservers
  ( NSKeyValueSharedObservers
  , IsNSKeyValueSharedObservers(..)
  , initWithObservableClass
  , init_
  , new
  , addSharedObserver_forKey_options_context
  , addObserver_forKeyPath_options_context
  , snapshot
  , addObserver_forKeyPath_options_contextSelector
  , addSharedObserver_forKey_options_contextSelector
  , initSelector
  , initWithObservableClassSelector
  , newSelector
  , snapshotSelector

  -- * Enum types
  , NSKeyValueObservingOptions(NSKeyValueObservingOptions)
  , pattern NSKeyValueObservingOptionNew
  , pattern NSKeyValueObservingOptionOld
  , pattern NSKeyValueObservingOptionInitial
  , pattern NSKeyValueObservingOptionPrior

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | A new collection of observables for an observable object of the given class
--
-- ObjC selector: @- initWithObservableClass:@
initWithObservableClass :: IsNSKeyValueSharedObservers nsKeyValueSharedObservers => nsKeyValueSharedObservers -> Class -> IO RawId
initWithObservableClass nsKeyValueSharedObservers observableClass =
  sendOwnedMessage nsKeyValueSharedObservers initWithObservableClassSelector observableClass

-- | @- init@
init_ :: IsNSKeyValueSharedObservers nsKeyValueSharedObservers => nsKeyValueSharedObservers -> IO RawId
init_ nsKeyValueSharedObservers =
  sendOwnedMessage nsKeyValueSharedObservers initSelector

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "NSKeyValueSharedObservers"
    sendOwnedClassMessage cls' newSelector

-- | Add a new observer to the collection.
--
-- This method works like @-[NSObject addObserver: forKey: options: context:]@, but observations on nested and computed properties are disallowed. Observers are not registered until @setSharedObservers@ is called on the observable.
--
-- - Parameter observer: The observer object to register for KVO notifications.   The observer must implement the key-value observing method ``observeValue:   forKeyPath: of: change: context:`` - Parameter key: key of the property being observed. This cannot be a nested   key path or a computed property - Parameter options: A combination of NSKeyValueObservingOptions values that   specify what is included in observation notifications. For possible values   see NSKeyValueObservingOptions. - Parameter context: Arbitrary data which is passed to the observer object
--
-- ObjC selector: @- addSharedObserver:forKey:options:context:@
addSharedObserver_forKey_options_context :: (IsNSKeyValueSharedObservers nsKeyValueSharedObservers, IsNSObject observer, IsNSString key) => nsKeyValueSharedObservers -> observer -> key -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addSharedObserver_forKey_options_context nsKeyValueSharedObservers observer key options context =
  sendMessage nsKeyValueSharedObservers addSharedObserver_forKey_options_contextSelector (toNSObject observer) (toNSString key) options context

-- | @- addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_context :: (IsNSKeyValueSharedObservers nsKeyValueSharedObservers, IsNSObject observer, IsNSString keyPath) => nsKeyValueSharedObservers -> observer -> keyPath -> NSKeyValueObservingOptions -> Ptr () -> IO ()
addObserver_forKeyPath_options_context nsKeyValueSharedObservers observer keyPath options context =
  sendMessage nsKeyValueSharedObservers addObserver_forKeyPath_options_contextSelector (toNSObject observer) (toNSString keyPath) options context

-- | A momentary snapshot of all observers added to the collection thus far, that can be assigned to an observable using ``-[NSObject setSharedObservers:]``
--
-- ObjC selector: @- snapshot@
snapshot :: IsNSKeyValueSharedObservers nsKeyValueSharedObservers => nsKeyValueSharedObservers -> IO (Id NSKeyValueSharedObserversSnapshot)
snapshot nsKeyValueSharedObservers =
  sendMessage nsKeyValueSharedObservers snapshotSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithObservableClass:@
initWithObservableClassSelector :: Selector '[Class] RawId
initWithObservableClassSelector = mkSelector "initWithObservableClass:"

-- | @Selector@ for @init@
initSelector :: Selector '[] RawId
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] RawId
newSelector = mkSelector "new"

-- | @Selector@ for @addSharedObserver:forKey:options:context:@
addSharedObserver_forKey_options_contextSelector :: Selector '[Id NSObject, Id NSString, NSKeyValueObservingOptions, Ptr ()] ()
addSharedObserver_forKey_options_contextSelector = mkSelector "addSharedObserver:forKey:options:context:"

-- | @Selector@ for @addObserver:forKeyPath:options:context:@
addObserver_forKeyPath_options_contextSelector :: Selector '[Id NSObject, Id NSString, NSKeyValueObservingOptions, Ptr ()] ()
addObserver_forKeyPath_options_contextSelector = mkSelector "addObserver:forKeyPath:options:context:"

-- | @Selector@ for @snapshot@
snapshotSelector :: Selector '[] (Id NSKeyValueSharedObserversSnapshot)
snapshotSelector = mkSelector "snapshot"

