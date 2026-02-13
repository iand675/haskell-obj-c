{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRControllerFactoryParams@.
module ObjC.Matter.MTRControllerFactoryParams
  ( MTRControllerFactoryParams
  , IsMTRControllerFactoryParams(..)
  , storageDelegate
  , startServer
  , setStartServer
  , paaCerts
  , setPaaCerts
  , cdCerts
  , setCdCerts
  , cdCertsSelector
  , paaCertsSelector
  , setCdCertsSelector
  , setPaaCertsSelector
  , setStartServerSelector
  , startServerSelector
  , storageDelegateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- storageDelegate@
storageDelegate :: IsMTRControllerFactoryParams mtrControllerFactoryParams => mtrControllerFactoryParams -> IO RawId
storageDelegate mtrControllerFactoryParams =
  sendMessage mtrControllerFactoryParams storageDelegateSelector

-- | @- startServer@
startServer :: IsMTRControllerFactoryParams mtrControllerFactoryParams => mtrControllerFactoryParams -> IO Bool
startServer mtrControllerFactoryParams =
  sendMessage mtrControllerFactoryParams startServerSelector

-- | @- setStartServer:@
setStartServer :: IsMTRControllerFactoryParams mtrControllerFactoryParams => mtrControllerFactoryParams -> Bool -> IO ()
setStartServer mtrControllerFactoryParams value =
  sendMessage mtrControllerFactoryParams setStartServerSelector value

-- | @- paaCerts@
paaCerts :: IsMTRControllerFactoryParams mtrControllerFactoryParams => mtrControllerFactoryParams -> IO (Id NSArray)
paaCerts mtrControllerFactoryParams =
  sendMessage mtrControllerFactoryParams paaCertsSelector

-- | @- setPaaCerts:@
setPaaCerts :: (IsMTRControllerFactoryParams mtrControllerFactoryParams, IsNSArray value) => mtrControllerFactoryParams -> value -> IO ()
setPaaCerts mtrControllerFactoryParams value =
  sendMessage mtrControllerFactoryParams setPaaCertsSelector (toNSArray value)

-- | @- cdCerts@
cdCerts :: IsMTRControllerFactoryParams mtrControllerFactoryParams => mtrControllerFactoryParams -> IO (Id NSArray)
cdCerts mtrControllerFactoryParams =
  sendMessage mtrControllerFactoryParams cdCertsSelector

-- | @- setCdCerts:@
setCdCerts :: (IsMTRControllerFactoryParams mtrControllerFactoryParams, IsNSArray value) => mtrControllerFactoryParams -> value -> IO ()
setCdCerts mtrControllerFactoryParams value =
  sendMessage mtrControllerFactoryParams setCdCertsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @storageDelegate@
storageDelegateSelector :: Selector '[] RawId
storageDelegateSelector = mkSelector "storageDelegate"

-- | @Selector@ for @startServer@
startServerSelector :: Selector '[] Bool
startServerSelector = mkSelector "startServer"

-- | @Selector@ for @setStartServer:@
setStartServerSelector :: Selector '[Bool] ()
setStartServerSelector = mkSelector "setStartServer:"

-- | @Selector@ for @paaCerts@
paaCertsSelector :: Selector '[] (Id NSArray)
paaCertsSelector = mkSelector "paaCerts"

-- | @Selector@ for @setPaaCerts:@
setPaaCertsSelector :: Selector '[Id NSArray] ()
setPaaCertsSelector = mkSelector "setPaaCerts:"

-- | @Selector@ for @cdCerts@
cdCertsSelector :: Selector '[] (Id NSArray)
cdCertsSelector = mkSelector "cdCerts"

-- | @Selector@ for @setCdCerts:@
setCdCertsSelector :: Selector '[Id NSArray] ()
setCdCertsSelector = mkSelector "setCdCerts:"

