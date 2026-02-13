{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSNetServiceBrowser@.
module ObjC.Foundation.NSNetServiceBrowser
  ( NSNetServiceBrowser
  , IsNSNetServiceBrowser(..)
  , init_
  , scheduleInRunLoop_forMode
  , removeFromRunLoop_forMode
  , searchForBrowsableDomains
  , searchForRegistrationDomains
  , searchForServicesOfType_inDomain
  , stop
  , delegate
  , setDelegate
  , includesPeerToPeer
  , setIncludesPeerToPeer
  , delegateSelector
  , includesPeerToPeerSelector
  , initSelector
  , removeFromRunLoop_forModeSelector
  , scheduleInRunLoop_forModeSelector
  , searchForBrowsableDomainsSelector
  , searchForRegistrationDomainsSelector
  , searchForServicesOfType_inDomainSelector
  , setDelegateSelector
  , setIncludesPeerToPeerSelector
  , stopSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> IO (Id NSNetServiceBrowser)
init_ nsNetServiceBrowser =
  sendOwnedMessage nsNetServiceBrowser initSelector

-- | @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsNSNetServiceBrowser nsNetServiceBrowser, IsNSRunLoop aRunLoop, IsNSString mode) => nsNetServiceBrowser -> aRunLoop -> mode -> IO ()
scheduleInRunLoop_forMode nsNetServiceBrowser aRunLoop mode =
  sendMessage nsNetServiceBrowser scheduleInRunLoop_forModeSelector (toNSRunLoop aRunLoop) (toNSString mode)

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsNSNetServiceBrowser nsNetServiceBrowser, IsNSRunLoop aRunLoop, IsNSString mode) => nsNetServiceBrowser -> aRunLoop -> mode -> IO ()
removeFromRunLoop_forMode nsNetServiceBrowser aRunLoop mode =
  sendMessage nsNetServiceBrowser removeFromRunLoop_forModeSelector (toNSRunLoop aRunLoop) (toNSString mode)

-- | @- searchForBrowsableDomains@
searchForBrowsableDomains :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> IO ()
searchForBrowsableDomains nsNetServiceBrowser =
  sendMessage nsNetServiceBrowser searchForBrowsableDomainsSelector

-- | @- searchForRegistrationDomains@
searchForRegistrationDomains :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> IO ()
searchForRegistrationDomains nsNetServiceBrowser =
  sendMessage nsNetServiceBrowser searchForRegistrationDomainsSelector

-- | @- searchForServicesOfType:inDomain:@
searchForServicesOfType_inDomain :: (IsNSNetServiceBrowser nsNetServiceBrowser, IsNSString type_, IsNSString domainString) => nsNetServiceBrowser -> type_ -> domainString -> IO ()
searchForServicesOfType_inDomain nsNetServiceBrowser type_ domainString =
  sendMessage nsNetServiceBrowser searchForServicesOfType_inDomainSelector (toNSString type_) (toNSString domainString)

-- | @- stop@
stop :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> IO ()
stop nsNetServiceBrowser =
  sendMessage nsNetServiceBrowser stopSelector

-- | @- delegate@
delegate :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> IO RawId
delegate nsNetServiceBrowser =
  sendMessage nsNetServiceBrowser delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> RawId -> IO ()
setDelegate nsNetServiceBrowser value =
  sendMessage nsNetServiceBrowser setDelegateSelector value

-- | @- includesPeerToPeer@
includesPeerToPeer :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> IO Bool
includesPeerToPeer nsNetServiceBrowser =
  sendMessage nsNetServiceBrowser includesPeerToPeerSelector

-- | @- setIncludesPeerToPeer:@
setIncludesPeerToPeer :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> Bool -> IO ()
setIncludesPeerToPeer nsNetServiceBrowser value =
  sendMessage nsNetServiceBrowser setIncludesPeerToPeerSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSNetServiceBrowser)
initSelector = mkSelector "init"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @searchForBrowsableDomains@
searchForBrowsableDomainsSelector :: Selector '[] ()
searchForBrowsableDomainsSelector = mkSelector "searchForBrowsableDomains"

-- | @Selector@ for @searchForRegistrationDomains@
searchForRegistrationDomainsSelector :: Selector '[] ()
searchForRegistrationDomainsSelector = mkSelector "searchForRegistrationDomains"

-- | @Selector@ for @searchForServicesOfType:inDomain:@
searchForServicesOfType_inDomainSelector :: Selector '[Id NSString, Id NSString] ()
searchForServicesOfType_inDomainSelector = mkSelector "searchForServicesOfType:inDomain:"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @includesPeerToPeer@
includesPeerToPeerSelector :: Selector '[] Bool
includesPeerToPeerSelector = mkSelector "includesPeerToPeer"

-- | @Selector@ for @setIncludesPeerToPeer:@
setIncludesPeerToPeerSelector :: Selector '[Bool] ()
setIncludesPeerToPeerSelector = mkSelector "setIncludesPeerToPeer:"

