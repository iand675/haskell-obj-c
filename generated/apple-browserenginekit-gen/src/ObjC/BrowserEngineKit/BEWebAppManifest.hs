{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents a web application manifest
--
-- Generated bindings for @BEWebAppManifest@.
module ObjC.BrowserEngineKit.BEWebAppManifest
  ( BEWebAppManifest
  , IsBEWebAppManifest(..)
  , init_
  , initWithJSONData_manifestURL
  , jsonData
  , manifestURL
  , initSelector
  , initWithJSONData_manifestURLSelector
  , jsonDataSelector
  , manifestURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBEWebAppManifest beWebAppManifest => beWebAppManifest -> IO (Id BEWebAppManifest)
init_ beWebAppManifest =
  sendOwnedMessage beWebAppManifest initSelector

-- | Returns nil if manifestURL is invalid or jsonData cannot be parsed.
--
-- ObjC selector: @- initWithJSONData:manifestURL:@
initWithJSONData_manifestURL :: (IsBEWebAppManifest beWebAppManifest, IsNSData jsonData, IsNSURL manifestURL) => beWebAppManifest -> jsonData -> manifestURL -> IO (Id BEWebAppManifest)
initWithJSONData_manifestURL beWebAppManifest jsonData manifestURL =
  sendOwnedMessage beWebAppManifest initWithJSONData_manifestURLSelector (toNSData jsonData) (toNSURL manifestURL)

-- | @- jsonData@
jsonData :: IsBEWebAppManifest beWebAppManifest => beWebAppManifest -> IO (Id NSData)
jsonData beWebAppManifest =
  sendMessage beWebAppManifest jsonDataSelector

-- | @- manifestURL@
manifestURL :: IsBEWebAppManifest beWebAppManifest => beWebAppManifest -> IO (Id NSURL)
manifestURL beWebAppManifest =
  sendMessage beWebAppManifest manifestURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BEWebAppManifest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithJSONData:manifestURL:@
initWithJSONData_manifestURLSelector :: Selector '[Id NSData, Id NSURL] (Id BEWebAppManifest)
initWithJSONData_manifestURLSelector = mkSelector "initWithJSONData:manifestURL:"

-- | @Selector@ for @jsonData@
jsonDataSelector :: Selector '[] (Id NSData)
jsonDataSelector = mkSelector "jsonData"

-- | @Selector@ for @manifestURL@
manifestURLSelector :: Selector '[] (Id NSURL)
manifestURLSelector = mkSelector "manifestURL"

