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
  , storageDelegateSelector
  , startServerSelector
  , setStartServerSelector
  , paaCertsSelector
  , setPaaCertsSelector
  , cdCertsSelector
  , setCdCertsSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- storageDelegate@
storageDelegate :: IsMTRControllerFactoryParams mtrControllerFactoryParams => mtrControllerFactoryParams -> IO RawId
storageDelegate mtrControllerFactoryParams  =
    fmap (RawId . castPtr) $ sendMsg mtrControllerFactoryParams (mkSelector "storageDelegate") (retPtr retVoid) []

-- | @- startServer@
startServer :: IsMTRControllerFactoryParams mtrControllerFactoryParams => mtrControllerFactoryParams -> IO Bool
startServer mtrControllerFactoryParams  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrControllerFactoryParams (mkSelector "startServer") retCULong []

-- | @- setStartServer:@
setStartServer :: IsMTRControllerFactoryParams mtrControllerFactoryParams => mtrControllerFactoryParams -> Bool -> IO ()
setStartServer mtrControllerFactoryParams  value =
    sendMsg mtrControllerFactoryParams (mkSelector "setStartServer:") retVoid [argCULong (if value then 1 else 0)]

-- | @- paaCerts@
paaCerts :: IsMTRControllerFactoryParams mtrControllerFactoryParams => mtrControllerFactoryParams -> IO (Id NSArray)
paaCerts mtrControllerFactoryParams  =
    sendMsg mtrControllerFactoryParams (mkSelector "paaCerts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPaaCerts:@
setPaaCerts :: (IsMTRControllerFactoryParams mtrControllerFactoryParams, IsNSArray value) => mtrControllerFactoryParams -> value -> IO ()
setPaaCerts mtrControllerFactoryParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrControllerFactoryParams (mkSelector "setPaaCerts:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cdCerts@
cdCerts :: IsMTRControllerFactoryParams mtrControllerFactoryParams => mtrControllerFactoryParams -> IO (Id NSArray)
cdCerts mtrControllerFactoryParams  =
    sendMsg mtrControllerFactoryParams (mkSelector "cdCerts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCdCerts:@
setCdCerts :: (IsMTRControllerFactoryParams mtrControllerFactoryParams, IsNSArray value) => mtrControllerFactoryParams -> value -> IO ()
setCdCerts mtrControllerFactoryParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrControllerFactoryParams (mkSelector "setCdCerts:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @storageDelegate@
storageDelegateSelector :: Selector
storageDelegateSelector = mkSelector "storageDelegate"

-- | @Selector@ for @startServer@
startServerSelector :: Selector
startServerSelector = mkSelector "startServer"

-- | @Selector@ for @setStartServer:@
setStartServerSelector :: Selector
setStartServerSelector = mkSelector "setStartServer:"

-- | @Selector@ for @paaCerts@
paaCertsSelector :: Selector
paaCertsSelector = mkSelector "paaCerts"

-- | @Selector@ for @setPaaCerts:@
setPaaCertsSelector :: Selector
setPaaCertsSelector = mkSelector "setPaaCerts:"

-- | @Selector@ for @cdCerts@
cdCertsSelector :: Selector
cdCertsSelector = mkSelector "cdCerts"

-- | @Selector@ for @setCdCerts:@
setCdCertsSelector :: Selector
setCdCertsSelector = mkSelector "setCdCerts:"

