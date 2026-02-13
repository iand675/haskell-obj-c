{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a snapshot of changes made to the cinematic script since recording. Can be used as a snapshot to quickly revert to previously saved edits via @-[CNScript reloadWithChanges:]@
--
-- Generated bindings for @CNScriptChanges@.
module ObjC.Cinematic.CNScriptChanges
  ( CNScriptChanges
  , IsCNScriptChanges(..)
  , initWithDataRepresentation
  , init_
  , new
  , dataRepresentation
  , fNumber
  , userDecisions
  , addedDetectionTracks
  , addedDetectionTracksSelector
  , dataRepresentationSelector
  , fNumberSelector
  , initSelector
  , initWithDataRepresentationSelector
  , newSelector
  , userDecisionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create from previously saved data representation
--
-- ObjC selector: @- initWithDataRepresentation:@
initWithDataRepresentation :: (IsCNScriptChanges cnScriptChanges, IsNSData dataRepresentation) => cnScriptChanges -> dataRepresentation -> IO (Id CNScriptChanges)
initWithDataRepresentation cnScriptChanges dataRepresentation =
  sendOwnedMessage cnScriptChanges initWithDataRepresentationSelector (toNSData dataRepresentation)

-- | @- init@
init_ :: IsCNScriptChanges cnScriptChanges => cnScriptChanges -> IO (Id CNScriptChanges)
init_ cnScriptChanges =
  sendOwnedMessage cnScriptChanges initSelector

-- | @+ new@
new :: IO (Id CNScriptChanges)
new  =
  do
    cls' <- getRequiredClass "CNScriptChanges"
    sendOwnedClassMessage cls' newSelector

-- | Get persistent data representation of these changes for later restoration.
--
-- The changes can only be used with the original cinematic asset from which the CNScript was created.
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsCNScriptChanges cnScriptChanges => cnScriptChanges -> IO (Id NSData)
dataRepresentation cnScriptChanges =
  sendMessage cnScriptChanges dataRepresentationSelector

-- | The f/number to apply to the entire movie.
--
-- ObjC selector: @- fNumber@
fNumber :: IsCNScriptChanges cnScriptChanges => cnScriptChanges -> IO CFloat
fNumber cnScriptChanges =
  sendMessage cnScriptChanges fNumberSelector

-- | All active user decisions, including those made at recording time, unless they have been removed.
--
-- ObjC selector: @- userDecisions@
userDecisions :: IsCNScriptChanges cnScriptChanges => cnScriptChanges -> IO (Id NSArray)
userDecisions cnScriptChanges =
  sendMessage cnScriptChanges userDecisionsSelector

-- | All detection tracks that have been added. Does not include those created at recording time.
--
-- ObjC selector: @- addedDetectionTracks@
addedDetectionTracks :: IsCNScriptChanges cnScriptChanges => cnScriptChanges -> IO (Id NSArray)
addedDetectionTracks cnScriptChanges =
  sendMessage cnScriptChanges addedDetectionTracksSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDataRepresentation:@
initWithDataRepresentationSelector :: Selector '[Id NSData] (Id CNScriptChanges)
initWithDataRepresentationSelector = mkSelector "initWithDataRepresentation:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNScriptChanges)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNScriptChanges)
newSelector = mkSelector "new"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector '[] (Id NSData)
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @fNumber@
fNumberSelector :: Selector '[] CFloat
fNumberSelector = mkSelector "fNumber"

-- | @Selector@ for @userDecisions@
userDecisionsSelector :: Selector '[] (Id NSArray)
userDecisionsSelector = mkSelector "userDecisions"

-- | @Selector@ for @addedDetectionTracks@
addedDetectionTracksSelector :: Selector '[] (Id NSArray)
addedDetectionTracksSelector = mkSelector "addedDetectionTracks"

