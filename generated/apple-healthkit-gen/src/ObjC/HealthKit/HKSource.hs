{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKSource
--
-- Represents the entity that created an object stored by HealthKit.
--
-- Generated bindings for @HKSource@.
module ObjC.HealthKit.HKSource
  ( HKSource
  , IsHKSource(..)
  , defaultSource
  , init_
  , name
  , bundleIdentifier
  , bundleIdentifierSelector
  , defaultSourceSelector
  , initSelector
  , nameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | defaultSource
--
-- Returns the source representing the calling application.
--
-- ObjC selector: @+ defaultSource@
defaultSource :: IO (Id HKSource)
defaultSource  =
  do
    cls' <- getRequiredClass "HKSource"
    sendClassMessage cls' defaultSourceSelector

-- | @- init@
init_ :: IsHKSource hkSource => hkSource -> IO (Id HKSource)
init_ hkSource =
  sendOwnedMessage hkSource initSelector

-- | name
--
-- The name of the source represented by the receiver.  If the source is an app, then the name is the                localized name of the app.
--
-- ObjC selector: @- name@
name :: IsHKSource hkSource => hkSource -> IO (Id NSString)
name hkSource =
  sendMessage hkSource nameSelector

-- | bundleIdentifier
--
-- The bundle identifier of the source represented by the receiver.
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsHKSource hkSource => hkSource -> IO (Id NSString)
bundleIdentifier hkSource =
  sendMessage hkSource bundleIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultSource@
defaultSourceSelector :: Selector '[] (Id HKSource)
defaultSourceSelector = mkSelector "defaultSource"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKSource)
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector '[] (Id NSString)
bundleIdentifierSelector = mkSelector "bundleIdentifier"

