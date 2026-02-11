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
  , includesPeerToPeer
  , setIncludesPeerToPeer
  , initSelector
  , scheduleInRunLoop_forModeSelector
  , removeFromRunLoop_forModeSelector
  , searchForBrowsableDomainsSelector
  , searchForRegistrationDomainsSelector
  , searchForServicesOfType_inDomainSelector
  , stopSelector
  , includesPeerToPeerSelector
  , setIncludesPeerToPeerSelector


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

-- | @- init@
init_ :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> IO (Id NSNetServiceBrowser)
init_ nsNetServiceBrowser  =
  sendMsg nsNetServiceBrowser (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsNSNetServiceBrowser nsNetServiceBrowser, IsNSRunLoop aRunLoop, IsNSString mode) => nsNetServiceBrowser -> aRunLoop -> mode -> IO ()
scheduleInRunLoop_forMode nsNetServiceBrowser  aRunLoop mode =
withObjCPtr aRunLoop $ \raw_aRunLoop ->
  withObjCPtr mode $ \raw_mode ->
      sendMsg nsNetServiceBrowser (mkSelector "scheduleInRunLoop:forMode:") retVoid [argPtr (castPtr raw_aRunLoop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsNSNetServiceBrowser nsNetServiceBrowser, IsNSRunLoop aRunLoop, IsNSString mode) => nsNetServiceBrowser -> aRunLoop -> mode -> IO ()
removeFromRunLoop_forMode nsNetServiceBrowser  aRunLoop mode =
withObjCPtr aRunLoop $ \raw_aRunLoop ->
  withObjCPtr mode $ \raw_mode ->
      sendMsg nsNetServiceBrowser (mkSelector "removeFromRunLoop:forMode:") retVoid [argPtr (castPtr raw_aRunLoop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- searchForBrowsableDomains@
searchForBrowsableDomains :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> IO ()
searchForBrowsableDomains nsNetServiceBrowser  =
  sendMsg nsNetServiceBrowser (mkSelector "searchForBrowsableDomains") retVoid []

-- | @- searchForRegistrationDomains@
searchForRegistrationDomains :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> IO ()
searchForRegistrationDomains nsNetServiceBrowser  =
  sendMsg nsNetServiceBrowser (mkSelector "searchForRegistrationDomains") retVoid []

-- | @- searchForServicesOfType:inDomain:@
searchForServicesOfType_inDomain :: (IsNSNetServiceBrowser nsNetServiceBrowser, IsNSString type_, IsNSString domainString) => nsNetServiceBrowser -> type_ -> domainString -> IO ()
searchForServicesOfType_inDomain nsNetServiceBrowser  type_ domainString =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr domainString $ \raw_domainString ->
      sendMsg nsNetServiceBrowser (mkSelector "searchForServicesOfType:inDomain:") retVoid [argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_domainString :: Ptr ())]

-- | @- stop@
stop :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> IO ()
stop nsNetServiceBrowser  =
  sendMsg nsNetServiceBrowser (mkSelector "stop") retVoid []

-- | @- includesPeerToPeer@
includesPeerToPeer :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> IO Bool
includesPeerToPeer nsNetServiceBrowser  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNetServiceBrowser (mkSelector "includesPeerToPeer") retCULong []

-- | @- setIncludesPeerToPeer:@
setIncludesPeerToPeer :: IsNSNetServiceBrowser nsNetServiceBrowser => nsNetServiceBrowser -> Bool -> IO ()
setIncludesPeerToPeer nsNetServiceBrowser  value =
  sendMsg nsNetServiceBrowser (mkSelector "setIncludesPeerToPeer:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @searchForBrowsableDomains@
searchForBrowsableDomainsSelector :: Selector
searchForBrowsableDomainsSelector = mkSelector "searchForBrowsableDomains"

-- | @Selector@ for @searchForRegistrationDomains@
searchForRegistrationDomainsSelector :: Selector
searchForRegistrationDomainsSelector = mkSelector "searchForRegistrationDomains"

-- | @Selector@ for @searchForServicesOfType:inDomain:@
searchForServicesOfType_inDomainSelector :: Selector
searchForServicesOfType_inDomainSelector = mkSelector "searchForServicesOfType:inDomain:"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @includesPeerToPeer@
includesPeerToPeerSelector :: Selector
includesPeerToPeerSelector = mkSelector "includesPeerToPeer"

-- | @Selector@ for @setIncludesPeerToPeer:@
setIncludesPeerToPeerSelector :: Selector
setIncludesPeerToPeerSelector = mkSelector "setIncludesPeerToPeer:"

